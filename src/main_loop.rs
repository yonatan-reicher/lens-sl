//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::{BackwardMap, Inst, Register, State};
use crate::collect_registers::Collector;
use crate::direction::Direction;
use crate::len::Len;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::verify::{self, verify};
use crate::word::prelude::*;
use crate::{Cancelled, Config, ShouldCancel};
use crate::{backward_graph, graph};

// std imports
use std::ops::ControlFlow::{self, Break, Continue};

use rustc_hash::FxHashMap;
use serde::de::DeserializeOwned;

use functionality::prelude::*;

// =========================================== Graph ==============================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Programs<W>>;

type BackwardGraph<W> = backward_graph::BackwardGraph<State<W>, Inst<W>>;
type BackwardGraphPath<'a, W> = backward_graph::BackwardGraphPath<'a, State<W>, Inst<W>>;

// ====================================== Implementation ==========================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word + HasBitWord, WS: Word + HasBitWord + serde::de::DeserializeOwned>(
    c: Config<WT>,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
) -> Result<Option<Program<WT>>, Cancelled>
where
    BitWord<WS>: DeserializeOwned,
    <WS as All>::Iter: Clone,
{
    if c.program.is_empty() {
        return Ok(None);
    }

    let mut reducer = Reducer::<WT, WS>::default();
    let mut reduced_program = Vec::with_capacity(c.program.len());
    for inst in c.program {
        // This puts the original unreduced constants into the reducer.
        reduced_program.push(inst.reduce(&mut reducer));
    }
    let additional_immediates_reduced: Vec<WS> = c
        .additional_immediates
        .iter()
        .map(|i| reducer.reduce(*i, &ImmediateInfo { is_shift: false }))
        .collect();

    // Collect all the registers and immediates that might be useful for synthesis.
    let registers = Collector::new()
        .mutate(|col| col.program(c.program))
        .pipe(|col| col.registers)
        .mutate(|r| r.extend(c.additional_registers))
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());
    let immediates: Vec<WS> = reducer
        .immediates()
        .chain(additional_immediates_reduced)
        .collect::<Vec<WS>>()
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());

    // let oracle = TestCasesOracle { test_cases };
    // let oracle_reduced = TestCasesOracle {
    //     test_cases: test_cases_reduced,
    // };

    let oracle = SmtOracle::new(c.program.to_vec());
    let oracle_reduced = SmtOracle::new(reduced_program);

    synthesize::<WT, WS>(
        c,
        &registers,
        &immediates,
        oracle,
        oracle_reduced,
        reducer,
        tui,
    )
}

fn synthesize<WT: Word + HasBitWord, W: Word + HasBitWord + serde::de::DeserializeOwned>(
    c: Config<WT>,
    registers: &[Register],
    immediates: &[W],
    oracle: impl Oracle<[Inst<WT>], State<WT>>,
    oracle_reduced: impl Oracle<[Inst<W>], State<W>>,
    reducer: Reducer<WT, W>,
    tui: &impl for<'a> TuiHook<&'a Graph<W>, State<W>>,
) -> Result<Option<Program<WT>>, Cancelled>
where
    BitWord<W>: DeserializeOwned,
    <W as All>::Iter: Clone,
{
    // The forward and backward graphs start while having the empty program.
    let empty_program = Programs::empty_program();
    let mut forward_graph = Graph::Leaf(empty_program.clone());
    let mut backward_graph = BackwardGraph::default();
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
        backward_length: 0,
        extender: reducer,
        tui,
        backward_map: BackwardMap::new(registers).unwrap(),
        total_instructions: Inst::enumerate(*enumeration_info).count(),
        enumeration_info: *enumeration_info,
    };
    // Generate a first input
    println!("Checking empty program");
    match globals.oracle_reduced.check_program(&[]) {
        // TODO: What if the reduced program is equivalent but not the unreduced?
        Ok(()) => return Ok(Some(vec![])), // Turns out it's actually the empty program 🤷
        Err((inp, out)) => {
            tui.found_counter_example(inp, out);
            globals.inputs.push(inp);
            globals.outputs.push(out);
            // Put the empty program into the backward graph under that input-output pair.
            backward_graph
                .0
                .push(vec![[(out, empty_program)].into_iter().collect()]);
        }
    }
    tui.report_graph(Direction::Forward, &forward_graph);
    // tui.report_graph(Direction::Backward, &backward_graph);
    loop {
        // ------------------------------ Search Phase --------------------------------------------
        tui.searching();
        for (i, inst) in Inst::enumerate(*enumeration_info).enumerate()
        /* Inst::enumerate - go through everything, .enumerate() - give indices */
        {
            tui.progress(i, globals.total_instructions);
            let res = connect_and_refine::<WT, W>(
                &mut globals,
                &c.should_cancel,
                &mut forward_graph,
                &mut backward_graph.root(),
                inst,
                1,
            );
            match res {
                ConnectAndRefineResult::Found(prog) => {
                    println!("Found program of length {}", prog.len());
                    return Ok(Some(prog));
                }
                ConnectAndRefineResult::Continue => {}
                ConnectAndRefineResult::Cancel => return Err(Cancelled),
            }
        }
        // ------------------------------ Expand Phase --------------------------------------------
        tui.progress(globals.total_instructions, globals.total_instructions);
        tui.report_graph(Direction::Forward, &forward_graph);
        // tui.report_graph(Direction::Backward, &backward_graph);
        if globals.forward_length + globals.backward_length + 1 == c.program.len() - 1 {
            return Ok(None);
        }
        let should_expand_forward =
            c.forward_only || 2 * globals.backward_length >= globals.forward_length;
        let direction = Direction::from_is_forward(should_expand_forward);
        tui.expanding(direction);
        let ret = if should_expand_forward {
            let ret = expand_forward(
                &mut forward_graph,
                enumeration_info,
                globals.tui,
                globals.total_instructions,
                &c.should_cancel,
            );
            globals.forward_length += 1;
            tui.report_graph(Direction::Forward, &forward_graph);
            ret
        } else {
            let ret = expand_backward(
                &mut backward_graph,
                enumeration_info,
                globals.tui,
                &globals.backward_map,
                &c.should_cancel,
                &globals.outputs,
                globals.backward_length + 1,
            );
            globals.backward_length += 1;
            // tui.report_graph(Direction::Backward, &backward_graph);
            ret
        };
        match ret {
            Break(Cancelled) => return Err(Cancelled),
            Continue(()) => (),
        }
        // print_stats(&forward_graph, &backward_graph);
    }
}

enum ConnectAndRefineResult<W: Word + HasBitWord> {
    Found(Program<W>),
    Continue,
    Cancel,
}

/// WT - word for the target program. WS - word for the synthesis process.
struct Globals<
    'tui,
    WT: Word + HasBitWord,
    WS: Word + HasBitWord,
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
    enumeration_info: EnumerationInfo<'tui, WS>,
}

enum ProgramOrRetry<W: Word + HasBitWord> {
    Program(Program<W>),
    Retry,
}

fn connect_and_refine<WT: Word + HasBitWord, WS: Word + HasBitWord>(
    globals: &mut Globals<
        '_,
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
        impl for<'a> TuiHook<&'a Graph<WS>, State<WS>>,
    >,
    should_cancel: &ShouldCancel,
    forward_graph: &mut Graph<WS>,
    backward_graph: &mut BackwardGraphPath<WS>,
    inst: Inst<WS>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult<WT> {
    if should_cancel.check() {
        return ConnectAndRefineResult::Cancel;
    }
    let tui = globals.tui;
    if k > globals.inputs.len() {
        let mut counter_example_added = false;
        match (&forward_graph, backward_graph.get()) {
            (Graph::Leaf(prefixes), Some(postfixes)) => {
                let ret = {
                    // We found a class of candidate programs.
                    // Try each one. If one works, return it. If none work, adds all counter-examples.
                    // First, make a buffer to hold the program.
                    let program_length = globals.forward_length + 1 + globals.backward_length;
                    let mut program = Vec::with_capacity(program_length);
                    prefixes.try_each(|prefix| {
                        debug_assert_eq!(prefix.len(), globals.forward_length);
                        postfixes.iter().try_for_each(|postfix| {
                            debug_assert_eq!(postfix.len(), globals.backward_length);
                            // Build the current candidate (reduced) program.
                            program.clear();
                            program.extend(prefix.iter());
                            program.push(inst);
                            program.extend(postfix.iter());
                            let (inputs, outputs) = (&mut globals.inputs, &mut globals.outputs);
                            match verify(
                                &program,
                                &globals.extender,
                                &mut globals.oracle_reduced,
                                &mut globals.oracle,
                                |equivalent_prog| Break(ProgramOrRetry::Program(equivalent_prog.to_vec())),
                            ) {
                                verify::Result::CounterExample(inp, out) => {
                                    tui.found_counter_example( inp, out,);
                                    let mut actual = inp;
                                    program.iter().for_each(|i| i.run(&mut actual));
                                    debug_assert!(
                                        !has_counter_example_been_seen(inputs, outputs, &inp, &out),
                                        "Counter-example from reduced oracle should not have been seen before."
                                    );
                                    debug_assert!(actual != out, "Found mismatched interpreter behaviours!");
                                    inputs.push(inp);
                                    outputs.push(out);
                                    // TODO: What is the equivalent of this in greenthumb's impl?
                                    // backward_graph.g.0.push(vec![[(inp, empty_program)].into_iter().collect()]);
                                    counter_example_added = true;
                                    Break(ProgramOrRetry::Retry)
                                }
                                verify::Result::Break(x) => Break(x),
                                verify::Result::Continue => Continue(()),
                            }
                        })
                    })
                };
                match ret {
                    Break(ProgramOrRetry::Program(prog)) => {
                        return ConnectAndRefineResult::Found(prog);
                    }
                    Break(ProgramOrRetry::Retry) => {
                        return connect_and_refine(
                            globals,
                            should_cancel,
                            forward_graph,
                            backward_graph,
                            inst,
                            k,
                        );
                    }
                    Continue(()) => (),
                }
            }
            _ => {
                println!("Graphs are not leaves at the end.");
                println!("Forward Graph: \n{}", forward_graph.pretty_print());
                // println!("Backward Graph: \n{}", backward_graph.pretty_print());
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

    if backward_graph.ended() {
        build_backward(
            backward_graph.g,
            k - 1,
            &globals.backward_map,
            globals.enumeration_info,
            &globals.outputs,
            globals.backward_length,
        );
    }

    // Must be nests, because build_forwards/backwards always turn leaves into nests.
    let Graph::Nest(forward_outputs) = forward_graph else {
        panic!();
    };
    debug_assert!(!backward_graph.ended());

    let mut next = State::default();
    for (forward_output, forward_subgraph) in forward_outputs {
        forward_output.clone_to(&mut next);
        inst.run(&mut next);
        if let Ok(()) = backward_graph.try_descend(next) {
            let res = connect_and_refine(
                globals,
                should_cancel,
                forward_subgraph,
                backward_graph,
                inst,
                k + 1,
            );
            match res {
                ConnectAndRefineResult::Found(p) => return ConnectAndRefineResult::Found(p),
                ConnectAndRefineResult::Continue => {}
                ConnectAndRefineResult::Cancel => return ConnectAndRefineResult::Cancel,
            }
            backward_graph.ascend();
        }
    }
    ConnectAndRefineResult::Continue
}

/// Go through each program prefix in the graph, and expand it by one
/// instruction forward. This is done for each program, and for each
/// instruction.
fn expand_forward<W: Word + HasBitWord>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    total_inst: usize,
    should_cancel: &ShouldCancel,
) -> ControlFlow<Cancelled> {
    expand_forward_or_backward(
        graph,
        ei,
        tui,
        total_inst,
        |mut state, inst| {
            inst.run(&mut state);
            [state]
        },
        should_cancel,
    )
}

fn expand_backward<W: Word + HasBitWord>(
    graph: &mut BackwardGraph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    bm: &BackwardMap<W>,
    should_cancel: &ShouldCancel,
    outputs: &[State<W>],
    backward_length: usize,
) -> ControlFlow<Cancelled> {
    let n_counter_examples = outputs.len();
    for i in 0..n_counter_examples {
        if should_cancel.check() {
            return Break(Cancelled);
        }
        tui.progress(i, n_counter_examples);
        update_backward_graph(*ei, bm, graph, outputs, backward_length, i);
    }
    Continue(())
}

fn update_backward_graph<W: Word + HasBitWord>(
    ei: EnumerationInfo<W>,
    bm: &BackwardMap<W>,
    backward_graph: &mut BackwardGraph<W>,
    outputs: &[State<W>],
    postfix_length: usize,
    ce: usize,
) {
    // Implementation: treat the backward graph as a matrix, where the row index is length, and the
    // column index is counter example index. Go through column `ce` top to bottom, assuming all 0
    // <= i < ce have been initialized. For each cell, if we need to, initialize it. Once we
    // initialized a cell, we never have to mutate it again.
    let output = outputs[ce];
    let same_as = (0..ce).find(|i| outputs[*i] == output);
    // Initializing the first row (postfixes of length 0)
    if backward_graph.0[0].len() == ce {
        let cell = FxHashMap::default().mutate(|m| {
            m.insert(outputs[ce], Programs::empty_program());
        });
        backward_graph.0[0].push(cell);
    }
    // Initializing the rest!
    for len in 1..=postfix_length {
        debug_assert!(
            backward_graph.0.len() >= len,
            "we are initializing this one by one"
        );
        if backward_graph.0.len() == len {
            backward_graph.0.push(vec![]);
        }
        let [.., prev_row, curr_row] = backward_graph.0.as_mut_slice() else {
            panic!();
        };
        let n_ces_ran_on = curr_row.len();
        debug_assert!(n_ces_ran_on >= ce, "assuming that we created the previous columns");
        if n_ces_ran_on == ce {
            // We haven't created this one yet!
            curr_row.push(FxHashMap::default());
            let (prev_cell, curr_cell) = (&mut prev_row[ce], &mut curr_row[ce]);
            if let Some(ce_before) = same_as {
                // We already calculated this...
                curr_row[ce] = curr_row[ce_before].clone();
            } else {
                update_backward_graph_cell(ei, prev_cell, curr_cell, bm);
            }
        }
    }
}

fn update_backward_graph_cell<W: Word + HasBitWord>(
    ei: EnumerationInfo<W>,
    prev: &mut FxHashMap<State<W>, Programs<W>>,
    current: &mut FxHashMap<State<W>, Programs<W>>,
    bm: &BackwardMap<W>,
) {
    for inst in Inst::enumerate(ei) {
        for (input, programs) in prev.iter() {
            let in_list = inst.run_backward(*input, bm);
            let new_progs = programs.clone().concat(inst);
            for new_input in in_list {
                current.entry(new_input).or_default().extend(&new_progs);
            }
        }
    }
}

fn expand_forward_or_backward<W: Word + HasBitWord, StepRet: IntoIterator<Item = State<W>>>(
    graph: &mut Graph<W>,
    ei: &EnumerationInfo<W>,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    total_inst: usize,
    step: impl Fn(State<W>, Inst<W>) -> StepRet,
    should_cancel: &ShouldCancel,
) -> ControlFlow<Cancelled> {
    // let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    let old_graph = std::mem::replace(graph, Graph::Leaf(Default::default()));
    debug_assert_ne!(old_graph.n_programs(), 0);
    let mut out_states = vec![];
    // TODO: I think this would be faster if we iterate through the graph only once, and have this
    // for loop on each leaf. Or maybe, iterate on like a 100 instructions at once.
    for (i, inst) in Inst::enumerate(*ei).enumerate()
    /* Inst::enumerate - go through everything, .enumerate() - give indices */
    {
        tui.progress(i, total_inst);
        out_states.clear();
        recurse(
            &old_graph,
            graph,
            inst,
            &mut out_states,
            &step,
            should_cancel,
        )?;
    }
    tui.progress(total_inst, total_inst);
    debug_assert_ne!(graph.n_programs(), 0);
    return Continue(());

    fn recurse<W: Word + HasBitWord, StepRet: IntoIterator<Item = State<W>>>(
        old_graph: &Graph<W>,
        new_graph: &mut Graph<W>,
        inst: Inst<W>,
        out_states: &mut Vec<State<W>>,
        step: &impl Fn(State<W>, Inst<W>) -> StepRet,
        should_cancel: &ShouldCancel,
    ) -> ControlFlow<Cancelled> {
        if should_cancel.check() {
            return Break(Cancelled);
        }
        match old_graph {
            Graph::Leaf(programs) if programs.is_empty() => (),
            Graph::Leaf(programs) => {
                let programs = programs.clone().concat(inst);
                new_graph.insert_all(out_states, [programs]);
            }
            Graph::Nest(hash_map) => {
                for (state, sub_graph) in hash_map.iter() {
                    for out_state in step(*state, inst) {
                        out_states.push(out_state);
                        recurse(sub_graph, new_graph, inst, out_states, step, should_cancel)?;
                        out_states.pop();
                    }
                }
            }
        }
        Continue(())
    }
}

fn build_forward<W: Word + HasBitWord>(graph: &mut Graph<W>, input: &State<W>) {
    build_forwards_or_backwards(graph, input, |program, mut state| {
        for inst in program {
            inst.run(&mut state);
        }
        [state]
    });
}

fn build_backward<W: Word + HasBitWord>(
    graph: &mut BackwardGraph<W>,
    ce: usize,
    bm: &BackwardMap<W>,
    ei: EnumerationInfo<W>,
    outputs: &[State<W>],
    postfix_length: usize,
) {
    update_backward_graph(ei, bm, graph, outputs, postfix_length, ce);
}

fn build_forwards_or_backwards<W: Word + HasBitWord, StepRet: IntoIterator<Item = State<W>>>(
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
            let programs: Programs<W> = program.iter().cloned().collect();
            for output in step(&program, *input) {
                graph.insert(output, [programs.clone()]);
            }
        });
    });
}

/// Checks if the given counter-example has already been seen, by searching the input-output pairs
/// in the global context.
fn has_counter_example_been_seen<WS: Word + HasBitWord>(
    inputs: &[State<WS>],
    outputs: &[State<WS>],
    inp: &State<WS>,
    out: &State<WS>,
) -> bool {
    inputs
        .iter()
        .zip(outputs)
        .any(|(i, o)| i == inp && o == out)
}
