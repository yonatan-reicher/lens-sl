//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::Masked as MaskedState;
use crate::arm::{Register, State, run_program_masked, what_program_reads};
use crate::collect_registers::Collector;
use crate::direction::Direction;
use crate::graph;
use crate::len::Len;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs_sl as programs;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;

// std imports
use std::ops::ControlFlow::{self, Break, Continue};

use rustc_hash::{FxHashMap, FxHashSet};
use serde::de::DeserializeOwned;

use functionality::prelude::*;

// =========================================== Graph ==============================================

type Inst<W> = crate::arm::Inst<W, BitWord<W>>;

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<(MaskedState<W>, MaskedState<W>), Programs<W>>;

// ========================================== Oracle ==============================================

// impl<W: Word> oracle::smt::Inst<State<W>> for Inst<W> {
//     type StateVars<'st> = StateVars<'st, W::SmtWord<'st>>;
//
//     type SymbolicState<'st> = SymbolicState<'st, W::SmtWord<'st>>;
//
//     fn new_state_vars<'st>(st: &'st smtlib::Storage, name: &str) -> Self::StateVars<'st> {
//         StateVars::new(st, name)
//     }
//
//     fn state_neq<'st>(
//         s1: Self::SymbolicState<'st>,
//         s2: Self::SymbolicState<'st>,
//     ) -> smtlib::Bool<'st> {
//         !s1.eq(s2)
//     }
//
//     fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>) {
//         self.run_symbolic(s);
//     }
//
//     fn step<'st>(&self, s: &mut State<W>) {
//         self.run(s);
//     }
//
//     fn extract_from_model<'st>(
//         model: &smtlib::Model<'st>,
//         s: StateVars<'st, W::SmtWord<'st>>,
//     ) -> State<W> {
//         // == Registers ==
//         let mut state = State::default();
//         for (i, var) in s.registers.iter().enumerate() {
//             let reg = Register(i as u8);
//             let val = model
//                 .eval(*var)
//                 .map(W::SmtWord::try_into_word)
//                 .unwrap_or_else(|| Some(0.into()))
//                 //.try_into()
//                 .unwrap_or_else(|| {
//                     panic!(
//                         "Failed to convert variable '{var:?}' to the right type in model {model}."
//                     )
//                 });
//             state.set_register(
//                 reg,
//                 val.into_word(), /* This is actually the same word type but whatever */
//             );
//         }
//         // == Flags ==
//         let load_bool = |b| {
//             model
//                 .eval(b)
//                 .and_then(|b| bool_term_to_bool(b))
//                 .unwrap_or(false /* Arbitrary default, result did not matter */)
//         };
//         state.set_flags(
//             Flags {
//                 z: load_bool(s.flags.z),
//                 n: load_bool(s.flags.n),
//                 c: load_bool(s.flags.c),
//                 v: load_bool(s.flags.v),
//             }
//             .into(),
//         );
//         state
//     }
// }

// ====================================== Implementation ==========================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word + HasBitWord, WS: Word + HasBitWord + serde::de::DeserializeOwned>(
    program: &[Inst<WT>],
    additional_registers: impl IntoIterator<Item = Register>,
    additional_immediates: impl IntoIterator<Item = WT>,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
) -> Option<Program<WT>>
where
    BitWord<WS>: DeserializeOwned,
    <WS as All>::Iter: Clone,
{
    let mut reducer = Reducer::<WT, WS>::default();
    let mut reduced_program = Vec::with_capacity(program.len());
    for inst in program {
        // This puts the original unreduced constants into the reducer.
        reduced_program.push(inst.reduce(&mut reducer));
    }
    let additional_immediates_reduced: Vec<WS> = additional_immediates
        .into_iter()
        .map(|i| reducer.reduce(i, &ImmediateInfo { is_shift: false }))
        .collect();

    // Collect all the registers and immediates that might be useful for synthesis.
    let registers = Collector::new()
        .mutate(|c| c.program(program))
        .pipe(|c| c.registers)
        .mutate(|r| r.extend(additional_registers))
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());
    let immediates: Vec<WS> = reducer
        .immediates()
        .chain(additional_immediates_reduced)
        .collect::<Vec<WS>>()
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());

    let oracle = SmtOracle::new(program.to_vec());
    let oracle_reduced = SmtOracle::new(reduced_program.clone());

    synthesize::<WT, WS>(
        &registers,
        &immediates,
        oracle,
        oracle_reduced,
        reducer,
        program.len(),
        reduced_program,
        tui,
    )
}

type Bank<W> = FxHashMap<MaskedState<W>, FxHashMap<MaskedState<W>, FxHashSet<Inst<W>>>>;

#[allow(clippy::too_many_arguments)]
fn synthesize<WT, W>(
    registers: &[Register],
    immediates: &[W],
    oracle: impl Oracle<[Inst<WT>], State<WT>>,
    oracle_reduced: impl Oracle<[Inst<W>], State<W>>,
    reducer: Reducer<WT, W>,
    // The length of the original program.
    // In the future, this could be max_cost.
    original_length: usize,
    original_reduced: Program<W>,
    tui: &impl for<'a> TuiHook<&'a Graph<W>, MaskedState<W>>,
) -> Option<Program<WT>>
where
    WT: Word + HasBitWord,
    W: Word + HasBitWord + DeserializeOwned,
    BitWord<W>: DeserializeOwned,
    <W as All>::Iter: Clone,
{
    let mut bank: Bank<W> = Default::default();
    let enumeration_info = &EnumerationInfo::<W> {
        registers: EnumerationInfoOptions::Limited(registers),
        immediates: EnumerationInfoOptions::Limited(immediates),
        include_nop: false,
        skip_cond_code: false,
    };
    let mut globals = Globals {
        oracle,
        oracle_reduced,
        inputs: vec![],
        outputs: vec![],
        forward_length: 0,
        extender: reducer,
        tui,
        total_instructions: Inst::enumerate(*enumeration_info).count(),
        original_reduced,
        registers,
        immediates,
    };
    loop {
        // ------------------------------ Search Phase --------------------------------------------
        tui.searching();
        tui.progress(0, globals.total_instructions);
        // Look for a program that can start from all these.
        let res = loop {
            let (inputs, outputs) = (globals.inputs.clone(), globals.outputs.clone());
            break match connect(&mut globals, &mut bank, &mut vec![], &inputs, &outputs) {
                Continue(()) => ConnectAndRefineResult::Continue,
                Break(ProgramOrRetry::Program(p)) => ConnectAndRefineResult::Found(p),
                Break(ProgramOrRetry::Retry) => continue,
            };
        };
        match res {
            ConnectAndRefineResult::Found(prog) => {
                println!("Found program of length {}", prog.len());
                return Some(prog);
            }
            ConnectAndRefineResult::Continue => {}
        }
        tui.progress(globals.total_instructions, globals.total_instructions);
        if globals.forward_length == original_length - 1 {
            return None;
        }
        // ------------------------------ Expand Phase --------------------------------------------
        let direction = Direction::Forward;
        tui.expanding(direction);
        //expand(&mut todo!(), globals.tui);
        globals.forward_length += 1;
    }
}

enum ConnectAndRefineResult<W: Word + HasBitWord> {
    Found(Program<W>),
    Continue,
}

/// WT - word for the target program. WS - word for the synthesis process.
struct Globals<
    'tui,
    WT: Word + HasBitWord,
    WS: Word + HasBitWord,
    OT: Oracle<[Inst<WT>], State<WT>>,
    OS: Oracle<[Inst<WS>], State<WS>>,
    TUI: for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
> {
    oracle: OT,
    /// The oracle that checks program in the reduced word size.
    oracle_reduced: OS,
    inputs: Vec<MaskedState<WS>>,
    outputs: Vec<MaskedState<WS>>,
    /// The length of the prefixes of the program being built.
    forward_length: usize,
    extender: Reducer<WT, WS>,
    tui: &'tui TUI,
    /// The total instructions we are enumerating
    total_instructions: usize,
    original_reduced: Program<WS>,
    registers: &'tui [Register],
    immediates: &'tui [WS],
}

enum ProgramOrRetry<W: Word + HasBitWord> {
    Program(Program<W>),
    Retry,
}

fn connect<WT, W>(
    g: &mut Globals<
        '_,
        WT,
        W,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<W>], State<W>>,
        impl for<'g> TuiHook<&'g Graph<W>, MaskedState<W>>,
    >,
    bank: &mut Bank<W>,
    program: &mut Vec<Inst<W>>,
    input_states: &[MaskedState<W>],
    output_states: &[MaskedState<W>],
) -> ControlFlow<ProgramOrRetry<WT>>
where
    WT: Word + HasBitWord,
    W: Word + HasBitWord,
{
    // Do we need to add more instructions?
    if program.len() >= g.forward_length {
        // Did we succeed?
        return if input_states == output_states {
            verify(program, g)
        } else {
            Continue(())
        };
    }
    // First we need to check that all the states are properly represented in the bank.
    for inp in input_states
        .iter()
        .flat_map(|s| s.sub_states())
        .filter(|s| !bank.contains_key(s))
        .collect::<Vec<_>>()
    {
        let mut e = FxHashMap::<_, FxHashSet<_>>::default();
        for inst in Inst::enumerate(EnumerationInfo {
            registers: EnumerationInfoOptions::Limited(g.registers),
            immediates: EnumerationInfoOptions::Limited(g.immediates),
            include_nop: false,
            skip_cond_code: false,
        }) {
            let Some(out) = inst.run_masked(inp) else {
                continue;
            };
            e.entry(out).or_default().insert(inst);
        }
        bank.insert(inp, e);
    }
    // Find all instructions (and their effects!) that can run from the current states.
    // Instead of doing this by iterating all instructions, do this by intersection of equivalence
    // classes that can run from the states.
    let empty = Default::default();
    let empty1 = Default::default();
    let insts = input_states
        .iter()
        .map(|state| {
            // For this state, return the equivalence classes that can run from it.
            state
                .sub_states()
                .flat_map(|sub_state| {
                    bank.get(&sub_state)
                        .unwrap_or(&empty)
                        .iter()
                        .flat_map(|(_, set)| set.iter().copied())
                })
                .collect::<FxHashSet<_>>()
        })
        .collect::<Vec<_>>()
        // Intersect!
        .pipe(|sets| {
            let smallest = sets.iter().min_by_key(|s| s.len()).unwrap_or(&empty1);
            smallest
                .iter()
                .cloned()
                .filter(|x| sets.iter().all(|s| s.contains(x)))
                .collect::<FxHashSet<_>>()
        });
    for inst in insts {
        // Recurse motha fucka!
        program.push(inst);
        let next_states = &input_states.iter().map(|s| inst.run_masked(*s).unwrap()).collect::<Vec<_>>();
        connect(g, bank, program, next_states, output_states)?;
        program.pop();
    }
    Continue(())
}

/// Go through each program prefix in the graph, and expand it by one
/// instruction forward. This is done for each program, and for each
/// instruction.
fn expand<W: Word + HasBitWord>(
    graph: &mut Graph<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, MaskedState<W>>,
) {
    let old_graph = std::mem::replace(graph, Graph::Leaf(Default::default()));
    debug_assert_ne!(old_graph.n_programs(), 0);
    let mut out_states = vec![];
    let mut effects = vec![];
    let mut i = 0;
    let total = old_graph.n_leaves();
    recurse_outer(&old_graph, &mut effects, &mut |progs, effects| {
        tui.progress(i, total);
        out_states.clear();
        recurse_inner(&old_graph, graph, progs, effects, &mut out_states);
        i += 1;
    });
    tui.progress(total, total);
    debug_assert_ne!(graph.n_programs(), 0);

    fn recurse_outer<W: Word + HasBitWord>(
        old_graph: &Graph<W>,
        effects: &mut Vec<(MaskedState<W>, MaskedState<W>)>,
        f: &mut impl FnMut(&Programs<W>, &[(MaskedState<W>, MaskedState<W>)]),
    ) {
        match old_graph {
            graph::Graph::Leaf(progs) => f(progs, effects),
            graph::Graph::Nest(hash_map) => {
                for (effect, sub_graph) in hash_map {
                    effects.push(*effect);
                    recurse_outer(sub_graph, effects, f);
                    effects.pop();
                }
            }
        }
    }

    fn recurse_inner<W: Word + HasBitWord>(
        old_graph: &Graph<W>,
        new_graph: &mut Graph<W>,
        progs: &Programs<W>,
        effects: &[(MaskedState<W>, MaskedState<W>)],
        out_states: &mut Vec<(MaskedState<W>, MaskedState<W>)>,
    ) {
        match old_graph {
            Graph::Leaf(programs) if programs.is_empty() => (),
            Graph::Leaf(programs) => {
                // TODO
                // Move this composition to the match arm below!
                let Some(effects): Option<Vec<_>> = effects
                    .iter()
                    .zip(out_states)
                    .map(|(e1, e2)| MaskedState::compose(*e1, *e2))
                    .collect()
                else {
                    return;
                };
                let programs = progs.clone().concat(programs.clone());
                new_graph.insert_all(&effects, [programs]);
            }
            Graph::Nest(hash_map) => {
                for (e, sub_graph) in hash_map.iter() {
                    out_states.push(*e);
                    recurse_inner(sub_graph, new_graph, progs, effects, out_states);
                    out_states.pop();
                }
            }
        }
    }
}

fn build_forward<W: Word + HasBitWord>(graph: &mut Graph<W>, input: &MaskedState<W>) {
    build_forwards_or_backwards(graph, input, |program, input| {
        use itertools::Either;
        // Check what we need to run and that we have it
        let necessary = what_program_reads(program.iter().cloned(), input.state());
        let Some(output) = run_program_masked(program.iter().cloned(), input & necessary.into())
        else {
            return Either::Left(std::iter::empty());
        };
        // What do we not need to run? (we add with it to the graph anyway)
        let dont_matter = input.mask() & !necessary;
        dont_matter
            .into_mask()
            .sub_masks()
            // TODO: Filter additional masks that have more than like 2 or 3 registers.
            .map(move |additional| {
                let input = input & (necessary.into_mask() | additional);
                (input, output | input)
            })
            .pipe(Either::Right)
    });
}

fn build_forwards_or_backwards<
    W: Word + HasBitWord,
    StepRet: IntoIterator<Item = (MaskedState<W>, MaskedState<W>)>,
>(
    graph: &mut Graph<W>,
    input: &MaskedState<W>,
    step: impl Fn(&Program<W>, MaskedState<W>) -> StepRet,
) {
    debug_assert!(matches!(graph, Graph::Leaf(..)));
    // Rebuild the graph.
    // TODO: We can probably avoid completely rebuilding by just removing and adding programs on
    // the same data-structure. This would reduce allocations, but you need to mark which programs
    // have been visited, or store them in a list.
    let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    old_graph.for_each(&mut |programs| {
        programs.each(|program| {
            let programs: Programs<W> = program.iter().cloned().collect();
            for output in step(&program, *input) {
                graph.insert(output, [programs.clone()]);
            }
        });
    });
}

/// Checks if the given counter-example has already been seen, by searching the input-output pairs
/// in the global context.
fn has_counter_example_been_seen<WT: Word + HasBitWord, WS: Word + HasBitWord>(
    globals: &mut Globals<
        '_,
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
        impl for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
    >,
    inp: &MaskedState<WS>,
    out: &MaskedState<WS>,
) -> bool {
    globals
        .inputs
        .iter()
        .zip(&globals.outputs)
        .any(|(i, o)| i == inp && o == out)
}

fn verify<WT, WS>(
    prog: &[Inst<WS>],
    g: &mut Globals<
        '_,
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
        impl for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
    >,
) -> ControlFlow<ProgramOrRetry<WT>>
where
    WT: Word + HasBitWord,
    WS: Word + HasBitWord,
{
    use crate::verify;
    match verify::verify(
        prog,
        &g.extender,
        &mut g.oracle_reduced,
        &mut g.oracle,
        |equivalent_prog| Break(ProgramOrRetry::Program(equivalent_prog.to_vec())),
    ) {
        verify::Result::CounterExample(inp, _out) => {
            let read_mask = what_program_reads(g.original_reduced.iter().cloned(), &inp);
            let inp = inp.masked(read_mask.into());
            let out = run_program_masked(g.original_reduced.iter().cloned(), inp).expect("the counter example found by the oracle must be runnable and the input mask for the program must be enough for it to run");
            g.tui.found_counter_example(inp, out);
            assert!(
                !has_counter_example_been_seen(g, &inp, &out),
                "Counter-example from reduced oracle should not have been seen before."
            );
            g.inputs.push(inp);
            g.outputs.push(out);
            Break(ProgramOrRetry::Retry)
        }
        verify::Result::Break(prog) => Break(prog),
        verify::Result::Continue => Continue(()),
    }
}
