//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::Direction;
use crate::collect_registers::Collector;
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph;
use crate::isa::{
    BackwardMap, Flags, Inst, Register, State, StateVars, SymbolicState, extend_program_for_each,
};
use crate::len::Len;
use crate::oracle::{self, Oracle, SmtOracle};
use crate::programs;
use crate::reduce_bit_width::Reducer;
use crate::tui::TuiHook;
use crate::word::prelude::*;

// std imports
use std::ops::ControlFlow::{Break, Continue};
use std::rc::Rc;

// functionality
use functionality::Pipe;

// smt stuff!
use crate::smtlib_utils::bool_term_to_bool;
use smtlib::Sorted;

// =========================================== Graph ==============================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Rc<Programs<W>>>;

// ========================================== Oracle ==============================================

impl<W: Word> oracle::test_cases::Program<State<W>> for [Inst<W>] {
    fn run(&self, state: &mut State<W>) {
        for inst in self {
            inst.run(state);
        }
    }
}

impl<W: Word> oracle::smt::Inst for Inst<W> {
    type State = State<W>;

    type StateVars<'st> = StateVars<'st, W>;

    type SymbolicState<'st> = SymbolicState<'st, W>;

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

    fn step<'st>(&self, s: &mut Self::State) {
        self.run(s);
    }

    fn extract_from_model<'st>(model: &smtlib::Model<'st>, s: StateVars<'st, W>) -> State<W> {
        // The state to return at the end.
        let st = s.registers[0].st();
        // == Registers ==
        let mut state = State::default();
        for (i, var) in s.registers.iter().enumerate() {
            let reg = Register(i as u8);
            let val = model
                .eval(*var)
                .unwrap_or_else(|| W::new_bit_vec(st, 0.as_()))
                .pipe(W::bit_vec_try_into)
                //.try_into()
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to convert variable '{var:?}' to the right type in model {model}."
                    )
                })
                .as_();
            state.set_register(reg, val);
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
    inputs: &[&[(Register, WT::Unsigned)]], // TODO: Return a program in Program<WT> instead...
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
) -> Option<Program<WT>> {
    let mut reducer = Reducer::<WT, WS>::default();
    let mut reduced_program = Vec::with_capacity(program.len());
    for inst in program {
        // This puts the original unreduced constants into the reducer.
        reduced_program.push(inst.reduce(&mut reducer));
    }

    // Run the program on each input to get the outputs. We call these "test cases".
    let test_cases: Vec<(State<WT>, State<WT>)> = inputs
        .iter()
        .map(|input| {
            let input = State::from((Flags::default(), input.iter().map(|(r, v)| (*r, v.as_()))));
            let mut output = input.clone();
            for inst in program {
                inst.run(&mut output);
            }
            (input, output)
        })
        .collect();
    let _test_cases_reduced: Vec<(State<WS>, State<WS>)> = test_cases
        .iter()
        .map(|(input, _output)| {
            let input = input.reduce(&mut reducer.clone());
            let mut output = input.clone();
            for inst in &reduced_program {
                inst.run(&mut output);
            }
            (input, output)
        })
        .collect();

    // Collect all the registers and immediates that might be useful for synthesis.
    let mut collector = Collector::new();
    collector.program(program);
    collector.test_cases(&test_cases);
    let Collector { registers } = collector;
    let immediates: Vec<WS::Unsigned> = reducer.immediates().chain([0.as_()]).collect();

    // let oracle = TestCasesOracle { test_cases };
    // let oracle_reduced = TestCasesOracle {
    //     test_cases: test_cases_reduced,
    // };

    let oracle = SmtOracle::new(program.to_vec());
    let oracle_reduced = SmtOracle::new(reduced_program);

    synthesize::<WT, WS>(
        &registers,
        &immediates,
        oracle,
        oracle_reduced,
        reducer,
        program.len(),
        tui,
    )
}

fn synthesize<WT: Word, W: Word + serde::de::DeserializeOwned>(
    registers: &[Register],
    immediates: &[W::Unsigned],
    oracle: impl Oracle<[Inst<WT>], State<WT>>,
    oracle_reduced: impl Oracle<[Inst<W>], State<W>>,
    reducer: Reducer<WT, W>,
    // The length of the original program.
    // In the future, this could be max_cost.
    original_length: usize,
    tui: &impl for<'a> TuiHook<&'a Graph<W>, State<W>>,
) -> Option<Program<WT>> {
    // The forward and backward graphs start while having the empty program.
    let empty_program = Rc::new(Programs::empty_program());
    let mut forward_graph = Graph::Leaf(empty_program.clone());
    let mut backward_graph = Graph::Leaf(empty_program);
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
    };
    // Generate a first input
    println!("Checking empty program");
    match globals.oracle_reduced.check_program(&[]) {
        Ok(()) => return Some(vec![]), // Turns out it's actually the empty program 🤷
        Err((inp, out)) => {
            tui.found_counter_example(inp.clone(), out.clone());
            globals.inputs.push(inp);
            globals.outputs.push(out);
        }
    }
    tui.report_graph(Direction::Forward, &forward_graph);
    tui.report_graph(Direction::Backward, &backward_graph);
    loop {
        tui.searching();
        for (i, inst) in Enumerator::new().into_iter(enumeration_info).enumerate() {
            tui.progress(i, globals.total_instructions);
            let res = connect_and_refine::<WT, W>(
                &mut globals,
                &mut forward_graph,
                &mut backward_graph,
                inst,
                1,
            );
            match res {
                ConnectAndRefineResult::Found(prog) => {
                    println!("Found program of length {}", prog.len());
                    return Some(prog);
                }
                ConnectAndRefineResult::Continue => {}
            }
        }
        tui.progress(globals.total_instructions, globals.total_instructions);
        tui.report_graph(Direction::Forward, &forward_graph);
        tui.report_graph(Direction::Backward, &backward_graph);
        if globals.forward_length + globals.backward_length + 1 == original_length - 1 {
            return None;
        }
        let should_expand_forward = 2 * globals.backward_length >= globals.forward_length;
        let direction = Direction::from_is_forward(should_expand_forward);
        tui.expanding(direction);
        if should_expand_forward {
            expand_forward(
                &mut forward_graph,
                enumeration_info,
                globals.tui,
                globals.total_instructions,
            );
            globals.forward_length += 1;
            tui.report_graph(Direction::Forward, &forward_graph);
        } else {
            expand_backward(
                &mut backward_graph,
                enumeration_info,
                globals.tui,
                globals.total_instructions,
                &globals.backward_map,
            );
            globals.backward_length += 1;
            tui.report_graph(Direction::Backward, &backward_graph);
        }
        // print_stats(&forward_graph, &backward_graph);
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
    TUI: for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
> {
    oracle: OT,
    /// The oracle that checks program in the reduced word size.
    oracle_reduced: OS,
    inputs: Vec<State<WS>>,
    outputs: Vec<State<WS>>,
    /// The length of the prefixes of the program being built.
    forward_length: usize,
    backward_length: usize,
    extender: Reducer<WT, WS>,
    tui: &'tui TUI,
    /// Stores data needed for running instructions backwards in time.
    backward_map: BackwardMap<WS>,
    /// The total instructions we are enumerating
    total_instructions: usize,
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
        impl for<'a> TuiHook<&'a Graph<WS>, State<WS>>,
    >,
    forward_graph: &mut Graph<WS>,
    backward_graph: &mut Graph<WS>,
    inst: Inst<WS>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult<WT> {
    let tui = globals.tui;
    if k > globals.inputs.len() {
        let mut counter_example_added = false;
        match (&forward_graph, &backward_graph) {
            (Graph::Leaf(prefixes), Graph::Leaf(postfixes)) => {
                let ret = {
                    // We found a class of candidate programs.
                    // Try each one. If one works, return it. If none work, adds all counter-examples.
                    // First, make a buffer to hold the program.
                    let program_length = globals.forward_length + 1 + globals.backward_length;
                    let mut program = Vec::with_capacity(program_length);
                    prefixes.try_each(|prefix| {
                        debug_assert_eq!(prefix.len(), globals.forward_length);
                        postfixes.try_each(|postfix| {
                            debug_assert_eq!(postfix.len(), globals.backward_length);
                            // Build the current candidate (reduced) program.
                            program.clear();
                            program.extend(prefix.iter());
                            program.push(inst);
                            program.extend(postfix.iter());
                            match globals.oracle_reduced.check_program(&program) {
                                // Found!
                                Ok(()) => extend_program_for_each(
                                    &program,
                                    &globals.extender,
                                    |extended_program| match globals
                                        .oracle
                                        .check_program(extended_program)
                                    {
                                        Ok(()) => {
                                            Break(ProgramOrRetry::Program(extended_program.to_vec()))
                                        }
                                        Err(_) => Continue(()),
                                    },
                                ),
                                Err((inp, out)) => {
                                    tui.found_counter_example(
                                        inp.clone(),
                                        out.clone(),
                                    );
                                    let mut actual = inp.clone();
                                    program.iter().for_each(|i| i.run(&mut actual));
                                    debug_assert!(
                                        !has_counter_example_been_seen(globals, &inp, &out),
                                        "Counter-example from reduced oracle should not have been seen before."
                                    );
                                    debug_assert!(actual != out, "Found mismatched interpreter behaviours!");
                                    globals.inputs.push(inp);
                                    globals.outputs.push(out);
                                    counter_example_added = true;
                                    Break(ProgramOrRetry::Retry)
                                }
                            }
                        })
                    })
                };
                match ret {
                    Break(ProgramOrRetry::Program(prog)) => {
                        return ConnectAndRefineResult::Found(prog);
                    }
                    Break(ProgramOrRetry::Retry) => {
                        return connect_and_refine(globals, forward_graph, backward_graph, inst, k);
                    }
                    Continue(()) => (),
                }
            }
            _ => {
                println!("Graphs are not leaves at the end.");
                println!("Forward Graph: \n{}", forward_graph.pretty_print());
                println!("Backward Graph: \n{}", backward_graph.pretty_print());
                panic!();
            }
        }
        if !counter_example_added {
            // When we don't find a counter-example, we know that we are already at the deepest part of
            // the graph, a leaf. When you are at a leaf, it means you don't have any more input-output
            // pairs to match between the forward and backward graph, so you can't connect the forward
            // and backwards graphs. That is to say, you are done!
            return ConnectAndRefineResult::Continue;
        }
    }

    if matches!(forward_graph, Graph::Leaf(..)) {
        build_forward(forward_graph, &globals.inputs[k - 1]);
    }

    if matches!(backward_graph, Graph::Leaf(..)) {
        build_backward(
            backward_graph,
            &globals.outputs[k - 1],
            &globals.backward_map,
        );
    }

    // Must be nests, because build_forwards/backwards always turn leaves into nests.
    let Graph::Nest(forward_outputs) = forward_graph else {
        panic!();
    };
    let Graph::Nest(backward_outputs) = backward_graph else {
        panic!();
    };

    let mut next = State::default();
    for (forward_output, forward_subgraph) in forward_outputs {
        forward_output.clone_to(&mut next);
        inst.run(&mut next);
        if let Some(backward_subgraph) = backward_outputs.get_mut(&next) {
            let res = connect_and_refine(globals, forward_subgraph, backward_subgraph, inst, k + 1);
            match res {
                ConnectAndRefineResult::Found(prog) => {
                    return ConnectAndRefineResult::Found(prog);
                }
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
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    total_inst: usize,
) {
    expand_forward_or_backward(graph, ei, tui, total_inst, |mut state, inst| {
        inst.run(&mut state);
        [state]
    })
}

fn expand_backward<W: Word>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    total_inst: usize,
    bm: &BackwardMap<W>,
) {
    expand_forward_or_backward(graph, ei, tui, total_inst, |state, inst| {
        inst.run_backward(state, bm).into_iter().cloned()
    });
}

fn expand_forward_or_backward<W: Word, StepRet: IntoIterator<Item = State<W>>>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    total_inst: usize,
    step: impl Fn(State<W>, Inst<W>) -> StepRet,
) {
    // let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
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

    fn recurse<W: Word, StepRet: IntoIterator<Item = State<W>>>(
        old_graph: &Graph<W>,
        new_graph: &mut Graph<W>,
        inst: Inst<W>,
        out_states: &mut Vec<State<W>>,
        step: &impl Fn(State<W>, Inst<W>) -> StepRet,
    ) {
        match old_graph {
            Graph::Leaf(programs) if programs.is_empty() => (),
            Graph::Leaf(programs) => {
                let programs = Rc::new(programs.clone().concat(inst));
                new_graph.insert_all(out_states, &programs);
            }
            Graph::Nest(hash_map) => {
                for (state, sub_graph) in hash_map.iter() {
                    for out_state in step(state.clone(), inst) {
                        out_states.push(out_state);
                        recurse(sub_graph, new_graph, inst, out_states, step);
                        out_states.pop();
                    }
                }
            }
        }
    }
}

fn build_forward<W: Word>(graph: &mut Graph<W>, input: &State<W>) {
    build_forwards_or_backwards(graph, input, |program, mut state| {
        for inst in program {
            inst.run(&mut state);
        }
        [state]
    });
}

fn build_backward<W: Word>(graph: &mut Graph<W>, input: &State<W>, bm: &BackwardMap<W>) {
    build_forwards_or_backwards(graph, input, |program, output| {
        // A vector of reaching states, that we push backwards in time, one instruction at a time.
        let mut states = vec![output];
        let mut new_states = vec![];
        for inst in program.iter().rev() {
            for state in states.drain(..) {
                for new_state in inst.run_backward(state, bm) {
                    new_states.push(new_state.clone());
                }
            }
            std::mem::swap(&mut states, &mut new_states);
            debug_assert!(new_states.is_empty());
        }
        states
    });
}

fn build_forwards_or_backwards<W: Word, StepRet: IntoIterator<Item = State<W>>>(
    graph: &mut Graph<W>,
    input: &State<W>,
    step: impl Fn(&Program<W>, State<W>) -> StepRet,
) {
    debug_assert!(matches!(graph, Graph::Leaf(..)));
    // Rebuild the graph.
    // TODO: We can probably avoid completely rebuilding by just removing and adding programs on
    // the same data-structure. This would reduce allocations, but you need to mark which programs
    // have been visited, or store them in a list.
    let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    old_graph.for_each(&mut |programs| {
        programs.each(|program| {
            let programs: Rc<Programs<W>> = Rc::new(program.iter().cloned().collect());
            for output in step(&program, input.clone()) {
                graph.insert(output, &programs);
            }
        });
        // let program = programs
        //     .sample()
        //     .expect("programs should not be empty here.");
        // my_outputs.clear();
        // dbg!(programs.len());
        // for i in initial_states {
        //     let mut my_output = i.clone();
        //     step(&program, &mut my_output);
        //     my_outputs.push(my_output);
        // }
        // graph.insert_all(&my_outputs, programs);
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
        impl for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
    >,
    inp: &State<WS>,
    out: &State<WS>,
) -> bool {
    globals
        .inputs
        .iter()
        .zip(&globals.outputs)
        .any(|(i, o)| i == inp && o == out)
}
