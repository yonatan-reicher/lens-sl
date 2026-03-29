//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::Direction;
use crate::all::All;
use crate::arm::state::Masked as MaskedState;
use crate::arm::{
    BackwardMap, Flags, Inst, Register, State, StateVars, SymbolicState, extend_program_for_each,
    run_program_masked, what_program_reads,
};
use crate::collect_registers::Collector;
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph;
use crate::len::Len;
use crate::oracle::{self, Oracle, SmtOracle};
use crate::programs;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;

// std imports
use std::ops::ControlFlow::{self, Break, Continue};

// smt stuff!
use crate::smtlib_utils::bool_term_to_bool;

use functionality::prelude::*;

// =========================================== Graph ==============================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<(MaskedState<W>, MaskedState<W>), Programs<W>>;

// ========================================== Oracle ==============================================

impl<W: Word> oracle::smt::Inst<State<W>> for Inst<W> {
    type StateVars<'st> = StateVars<'st, W::SmtWord<'st>>;

    type SymbolicState<'st> = SymbolicState<'st, W::SmtWord<'st>>;

    fn new_state_vars<'st>(st: &'st smtlib::Storage, name: &str) -> Self::StateVars<'st> {
        StateVars::new(st, name)
    }

    fn state_neq<'st>(
        s1: Self::SymbolicState<'st>,
        s2: Self::SymbolicState<'st>,
    ) -> smtlib::Bool<'st> {
        !s1.eq(s2)
    }

    fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>) {
        self.run_symbolic(s);
    }

    fn step<'st>(&self, s: &mut State<W>) {
        self.run(s);
    }

    fn extract_from_model<'st>(
        model: &smtlib::Model<'st>,
        s: StateVars<'st, W::SmtWord<'st>>,
    ) -> State<W> {
        // == Registers ==
        let mut state = State::default();
        for (i, var) in s.registers.iter().enumerate() {
            let reg = Register(i as u8);
            let val = model
                .eval(*var)
                .map(W::SmtWord::try_into_word)
                .unwrap_or_else(|| Some(0.into()))
                //.try_into()
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to convert variable '{var:?}' to the right type in model {model}."
                    )
                });
            state.set_register(
                reg,
                val.into_word(), /* This is actually the same word type but whatever */
            );
        }
        // == Flags ==
        let load_bool = |b| {
            model
                .eval(b)
                .and_then(|b| bool_term_to_bool(b))
                .unwrap_or(false /* Arbitrary default, result did not matter */)
        };
        state.set_flags(
            Flags {
                z: load_bool(s.flags.z),
                n: load_bool(s.flags.n),
                c: load_bool(s.flags.c),
                v: load_bool(s.flags.v),
            }
            .into(),
        );
        state
    }
}

// ====================================== Implementation ==========================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word, WS: Word + serde::de::DeserializeOwned>(
    program: &[Inst<WT>],
    additional_registers: impl IntoIterator<Item = Register>,
    additional_immediates: impl IntoIterator<Item = WT>,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, MaskedState<WS>>,
) -> Option<Program<WT>>
where
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

fn synthesize<WT: Word, W: Word + serde::de::DeserializeOwned>(
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
    <W as All>::Iter: Clone,
{
    // The graph starts with having just the empty program.
    let mut graph = Graph::Leaf(Programs::empty_program());
    let enumeration_info = &EnumerationInfo::<W> {
        registers: EnumerationInfoOptions::Limited(registers),
        immediates: EnumerationInfoOptions::Limited(immediates),
    };
    let mut globals = Globals {
        oracle,
        oracle_reduced,
        inputs: vec![],
        outputs: vec![],
        forward_length: 0,
        backward_length: 0,
        extender: reducer,
        tui,
        backward_map: BackwardMap::new(registers).unwrap(),
        total_instructions: Enumerator::new().into_iter(enumeration_info).count(),
        original_reduced,
    };
    // Generate a first input
    println!("Checking empty program");
    match verify(&[], &mut globals) {
        // Excuse the confusing case names please.
        // TODO: Make an enum for this.
        Continue(()) => panic!("Could not generate the first counter example"), // TODO: Keep trying programs until something works?
        Break(ProgramOrRetry::Program(p)) => return Some(p),
        Break(ProgramOrRetry::Retry) => (), // Found a counter example, keep going
    }
    tui.report_graph(Direction::Forward, &graph);
    loop {
        // ------------------------------ Search Phase --------------------------------------------
        tui.searching();
        tui.progress(0, globals.total_instructions);
        let res = connect_and_refine::<WT, W>(&mut globals, &mut graph, 1);
        match res {
            ConnectAndRefineResult::Found(prog) => {
                println!("Found program of length {}", prog.len());
                return Some(prog);
            }
            ConnectAndRefineResult::Continue => {}
        }
        // ------------------------------ Expand Phase --------------------------------------------
        tui.progress(globals.total_instructions, globals.total_instructions);
        tui.report_graph(Direction::Forward, &graph);
        if globals.forward_length == original_length - 1 {
            return None;
        }
        let direction = Direction::Forward;
        tui.expanding(direction);
        expand_forward(
            &mut graph,
            enumeration_info,
            globals.tui,
            globals.total_instructions,
        );
        globals.forward_length += 1;
        tui.report_graph(Direction::Forward, &graph);
    }
}

enum ConnectAndRefineResult<W: Word> {
    Found(Program<W>),
    Continue,
}

/// WT - word for the target program. WS - word for the synthesis process.
struct Globals<
    'tui,
    WT: Word,
    WS: Word,
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
    backward_length: usize,
    extender: Reducer<WT, WS>,
    tui: &'tui TUI,
    /// Stores data needed for running instructions backwards in time.
    backward_map: BackwardMap<WS>,
    /// The total instructions we are enumerating
    total_instructions: usize,
    original_reduced: Program<WS>,
}

enum ProgramOrRetry<W: Word> {
    Program(Program<W>),
    Retry,
}

fn connect_and_refine<WT: Word, WS: Word>(
    globals: &mut Globals<
        '_,
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
        impl for<'a> TuiHook<&'a Graph<WS>, MaskedState<WS>>,
    >,
    graph: &mut Graph<WS>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult<WT> {
    if k > globals.inputs.len() {
        match &graph {
            Graph::Leaf(programs) => {
                // We found a class of candidate programs.
                // Try each one. If one works, return it. If none work, adds all counter-examples.
                let ret = programs.try_each(|program| verify(&program, globals));
                match ret {
                    Break(ProgramOrRetry::Program(p)) => return ConnectAndRefineResult::Found(p),
                    Break(ProgramOrRetry::Retry) => return connect_and_refine(globals, graph, k),
                    Continue(()) => (),
                }
            }
            _ => {
                println!("Graph is not leaves at the end.");
                panic!();
            }
        }
    }

    if matches!(graph, Graph::Leaf(..)) {
        build_forward(graph, &globals.inputs[k - 1]);
    }

    let Graph::Nest(map) = graph else {
        panic!();
    };
    for ((inp, out), sub_graph) in map {
        // Check if the input and output feel good to match against the real ones.
        // The input just needs to be at least as general as the real input.
        // The output needs to match exactly.
        let (ce_inp, ce_out) = (&globals.inputs[k - 1], &globals.outputs[k - 1]);
        let missing_inputs = inp.mask() & !ce_inp.mask();
        let ce_inp_masked = *ce_inp & inp.mask().into_mask();
        let good = missing_inputs.is_empty() && ce_inp_masked == *inp && ce_out == out;
        if good {
            let res = connect_and_refine(globals, sub_graph, k + 1);
            match res {
                ConnectAndRefineResult::Found(prog) => return ConnectAndRefineResult::Found(prog),
                ConnectAndRefineResult::Continue => {}
            }
        }
    }
    ConnectAndRefineResult::Continue
}

/// Go through each program prefix in the graph, and expand it by one
/// instruction forward. This is done for each program, and for each
/// instruction.
fn expand_forward<W: Word>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, MaskedState<W>>,
    total_inst: usize,
) {
    expand_forward_or_backward(graph, ei, tui, total_inst, |state, inst| {
        inst.run_masked(state)
    })
}

fn expand_forward_or_backward<W: Word, StepRet: IntoIterator<Item = MaskedState<W>>>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, MaskedState<W>>,
    total_inst: usize,
    step: impl Fn(MaskedState<W>, Inst<W>) -> StepRet,
) {
    let old_graph = std::mem::replace(graph, Graph::Leaf(Default::default()));
    debug_assert_ne!(old_graph.n_programs(), 0);
    let mut out_states = vec![];
    // TODO: I think this would be faster if we iterate through the graph only once, and have this
    // for loop on each leaf. Or maybe, iterate on like a 100 instructions at once.
    for (i, inst) in Enumerator::new().into_iter(ei).enumerate() {
        tui.progress(i, total_inst);
        out_states.clear();
        recurse(&old_graph, graph, inst, &mut out_states, &step);
    }
    tui.progress(total_inst, total_inst);
    debug_assert_ne!(graph.n_programs(), 0);

    fn recurse<W: Word, StepRet: IntoIterator<Item = MaskedState<W>>>(
        old_graph: &Graph<W>,
        new_graph: &mut Graph<W>,
        inst: Inst<W>,
        out_states: &mut Vec<(MaskedState<W>, MaskedState<W>)>,
        step: &impl Fn(MaskedState<W>, Inst<W>) -> StepRet,
    ) {
        match old_graph {
            Graph::Leaf(programs) if programs.is_empty() => (),
            Graph::Leaf(programs) => {
                let programs = programs.clone().concat(inst);
                new_graph.insert_all(out_states, [programs]);
            }
            Graph::Nest(hash_map) => {
                for ((inp, prev_out), sub_graph) in hash_map.iter() {
                    for out in step(*prev_out, inst) {
                        out_states.push((*inp, out));
                        recurse(sub_graph, new_graph, inst, out_states, step);
                        out_states.pop();
                    }
                }
            }
        }
    }
}

fn build_forward<W: Word>(graph: &mut Graph<W>, input: &MaskedState<W>) {
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

fn build_backward<W: Word>(graph: &mut Graph<W>, input: &MaskedState<W>, bm: &BackwardMap<W>) {
    build_forwards_or_backwards(graph, input, |program, output| {
        todo!();
        []
        /*
        // A vector of reaching states, that we push backwards in time, one instruction at a time.
        let mut states = vec![output];
        let mut new_states = vec![];
        for inst in program.iter().rev() {
            for state in states.drain(..) {
                for new_state in inst.run_backward_masked(state, bm) {
                    new_states.push(*new_state);
                }
            }
            std::mem::swap(&mut states, &mut new_states);
            debug_assert!(new_states.is_empty());
        }
        states
        */
    });
}

fn build_forwards_or_backwards<
    W: Word,
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
fn has_counter_example_been_seen<WT: Word, WS: Word>(
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

fn verify<WBig: Word, W: Word>(
    prog: &[Inst<W>],
    globals: &mut Globals<
        WBig,
        W,
        impl Oracle<[Inst<WBig>], State<WBig>>,
        impl Oracle<[Inst<W>], State<W>>,
        impl for<'g> TuiHook<&'g Graph<W>, MaskedState<W>>,
    >,
) -> ControlFlow<ProgramOrRetry<WBig>> {
    match globals.oracle_reduced.check_program(prog) {
        // Found!
        Ok(()) => {
            extend_program_for_each(prog, &globals.extender, |extended_program| {
                match globals.oracle.check_program(extended_program) {
                    Ok(()) => Break(ProgramOrRetry::Program(extended_program.to_vec())),
                    Err(_) => Continue(()),
                }
            })
        }
        Err((inp, _out)) => {
            let read_mask = what_program_reads(globals.original_reduced.iter().cloned(), &inp);
            let inp = inp.masked(read_mask.into());
            let out = run_program_masked(globals.original_reduced.iter().cloned(), inp).expect("the counter example found by the oracle must be runnable and the input mask for the program must be enough for it to run");
            globals.tui.found_counter_example(inp, out);
            debug_assert!(
                !has_counter_example_been_seen(globals, &inp, &out),
                "Counter-example from reduced oracle should not have been seen before."
            );
            globals.inputs.push(inp);
            globals.outputs.push(out);
            Break(ProgramOrRetry::Retry)
        }
    }
}
