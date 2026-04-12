//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::Masked as MaskedState;
use crate::arm::{Register, State, run_program_masked, what_program_reads};
use crate::collect_registers::Collector;
use crate::direction::Direction;
use crate::graph;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs_sl as programs;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{Cancelled, ShouldCancel};

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
    should_cancel: ShouldCancel,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
) -> Result<Option<Program<WT>>, Cancelled>
where
    BitWord<WS>: DeserializeOwned,
    <WS as All>::Iter: Clone,
{
    if program.is_empty() {
        return Ok(None);
    }

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
        should_cancel,
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
    should_cancel: ShouldCancel,
    tui: &impl for<'a> TuiHook<&'a Graph<W>, MaskedState<W>>,
) -> Result<Option<Program<WT>>, Cancelled>
where
    WT: Word + HasBitWord,
    W: Word + HasBitWord + DeserializeOwned,
    BitWord<W>: DeserializeOwned,
    <W as All>::Iter: Clone,
{
    let enumeration_info = &EnumerationInfo::<W> {
        registers: EnumerationInfoOptions::Limited(registers),
        immediates: EnumerationInfoOptions::Limited(immediates),
        include_nop: false,
        skip_cond_code: false,
    };
    let mut g = Globals {
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
        seen: Default::default(),
        current_states: Default::default(),
        next_states: Default::default(),
        bank: Default::default(),
    };
    'restart: loop {
        let (inputs, outputs) = (g.inputs.clone(), g.outputs.clone());
        g.current_states.clear();
        g.current_states.push((inputs, vec![]));
        for length in 0..g.original_reduced.len() {
            dbg!(length);
            tui.searching();
            tui.progress(0, g.total_instructions);
            let len = g.current_states.len();
            for (i, (inputs, prog)) in std::mem::take(&mut g.current_states)
                .into_iter()
                .enumerate()
            {
                // Should we stop?
                if should_cancel.check() {
                    return Err(Cancelled);
                }
                // Did we succeed?
                if inputs == outputs {
                    match verify(&prog, &mut g) {
                        Continue(()) => todo!(),
                        Break(ProgramOrRetry::Program(p)) => return Ok(Some(p)),
                        Break(ProgramOrRetry::Retry) => continue 'restart,
                    }
                }
                tui.progress(i, len);
                let res = {
                    let input_states: &[MaskedState<W>] = &inputs;
                    // First we need to check that all the states are properly represented in the bank.
                    for inp in input_states
                        .iter()
                        .flat_map(|s| s.sub_states())
                        .filter(|s| !g.bank.contains_key(s))
                        .collect::<Vec<_>>()
                    {
                        // TODO: call this init_state_in_bank or something like that. Also make it so instructions
                        // are only added when the state is excatly what they read.
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
                        g.bank.insert(inp, e);
                    }
                    // Find all instructions (and their effects!) that can run from the current states.
                    // Instead of doing this by iterating all instructions, do this by intersection of equivalence
                    // classes that can run from the states.
                    let empty = Default::default();
                    let empty1 = Default::default();
                    let insts = input_states
                        .iter()
                        .map(|state| {
                            // For this state, return set of commands that can run from it.
                            state
                                .sub_states()
                                .flat_map(|sub_state| {
                                    g.bank
                                        .get(&sub_state)
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
                        let next_states = input_states
                            .iter()
                            .map(|s| inst.run_masked(*s))
                            .collect::<Option<Vec<_>>>()
                            .unwrap();
                        if g.seen.contains(&next_states) {
                            continue;
                        }
                        // TODO: Add Hila's full seen set.
                        g.seen.insert(next_states.clone());
                        // TODO: you know how to solve this memory allocation...
                        g.next_states
                            .push((next_states, prog.clone().mutate(|p| p.push(inst))));
                    }
                    Continue(())
                };
                let res = match res {
                    Continue(()) => ConnectAndRefineResult::Continue,
                    Break(ProgramOrRetry::Program(p)) => ConnectAndRefineResult::Found(p),
                    Break(ProgramOrRetry::Retry) => continue 'restart,
                };
                match res {
                    ConnectAndRefineResult::Found(prog) => {
                        println!("Found program of length {}", prog.len());
                        return Ok(Some(prog));
                    }
                    ConnectAndRefineResult::Continue => {}
                }
            }
            tui.progress(len, len);
            if g.forward_length == original_length - 1 {
                return Ok(None);
            }
            // ------------------------------ Expand Phase --------------------------------------------
            let direction = Direction::Forward;
            tui.expanding(direction);
            //expand(&mut todo!(), g.tui);
            g.forward_length += 1;
            std::mem::swap(&mut g.next_states, &mut g.current_states);
        }
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
    seen: FxHashSet<Vec<MaskedState<WS>>>,
    current_states: Vec<(Vec<MaskedState<WS>>, Program<WS>)>,
    next_states: Vec<(Vec<MaskedState<WS>>, Program<WS>)>,
    bank: Bank<WS>,
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

// TODO: OrCancel...
enum ProgramOrRetry<W: Word + HasBitWord> {
    Program(Program<W>),
    Retry,
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
