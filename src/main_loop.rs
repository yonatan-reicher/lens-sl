//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::collect_registers::{self, Collector};
use crate::enumerate::{EnumerationInfo, Enumerator};
use crate::graph;
use crate::isa::{
    self, Flags, FlagsBitField, Inst, Register, State as _, StateVars, SymbolicState,
    extend_program_for_each,
};
use crate::oracle::{self, Oracle, SmtOracle};
use crate::programs;
use crate::reduce_bit_width::Reducer;
use crate::word::prelude::*;

use std::ops::ControlFlow::{Break, Continue};
use std::rc::Rc;

use functionality::Pipe;

use crate::smtlib_utils::bool_term_to_bool;
use smtlib::Sorted;

// =========================================== State ==============================================

/// The state of the machine at a given point in time.
#[derive(Clone, Debug, Default, derive_more::Display, PartialEq, Eq, Hash)]
#[display(
    "Registers: {{{}}}, Flags: {}",
    registers
        .iter()
        .map(|(r, v)| format!("{r:?}: {v}"))
        .collect::<Vec<_>>()
        .join(", "),
    match &flags {
        Some(f) => format!("{f:?}"),
        None => "None".to_string(),
    }
)]
pub struct State<W: Word> {
    /// This vector is always sorted by register.
    /// Registers that are not present are not "live".
    pub registers: Vec<(Register, W::Unsigned)>,
    /// The value of the flags register. If None, flags is not "live".
    pub flags: Option<FlagsBitField>,
}

impl<W: Word> State<W> {
    /// Copies this state to another state object. Used to avoid clones, that in a loop, can
    /// allocate more.
    #[inline]
    fn clone_to(&self, other: &mut Self) {
        other.registers.clear();
        other.registers.extend(&self.registers);
        other.flags = self.flags;
    }

    fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> State<WSmall> {
        State {
            registers: self
                .registers
                .iter()
                .map(|(r, v)| (*r, reducer.reduce(*v, &Default::default())))
                .collect(),
            flags: self.flags,
        }
    }
}

impl<W: Word> isa::State<W> for State<W> {
    fn get_register(&self, reg: Register) -> W::Unsigned {
        for (r, v) in &self.registers {
            if *r == reg {
                return *v;
            }
        }
        panic!("Register {reg:?} not found in state.");
    }

    fn set_register(&mut self, reg: Register, value: W::Unsigned) {
        for (r, v) in &mut self.registers {
            if *r == reg {
                *v = value;
                return;
            }
        }
        self.registers.push((reg, value));
        self.registers.sort_by_key(|(r, _)| *r);
    }

    fn get_flags(&self) -> FlagsBitField {
        // self.flags.expect("Flags not set in state.")
        self.flags.unwrap_or_default()
    }

    fn set_flags(&mut self, flags: FlagsBitField) {
        self.flags = Some(flags);
    }
}

impl<W: Word> collect_registers::State<W> for State<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W::Unsigned)> {
        self.registers.iter().cloned()
    }
}

impl<W: Word> oracle::test_cases::State for State<W> {
    fn clone_to(&self, output: &mut Self) {
        self.clone_to(output);
    }
}

// =========================================== Graph ==============================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Programs<W>>;

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
pub fn optimize<WT: Word, WS: Word>(
    program: &[Inst<WT>],
    inputs: &[&[(Register, WT::Unsigned)]], // TODO: Return a program in Program<WT> instead...
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
            let input: State<WT> = State {
                registers: input.iter().map(|(r, v)| (*r, v.as_())).collect(),
                flags: None,
            };
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
    )
}

fn synthesize<WT: Word, W: Word>(
    registers: &[Register],
    immediates: &[W::Unsigned],
    oracle: impl Oracle<[Inst<WT>], State<WT>>,
    oracle_reduced: impl Oracle<[Inst<W>], State<W>>,
    reducer: Reducer<WT, W>,
    // The length of the original program.
    // In the future, this could be max_cost.
    original_length: usize,
) -> Option<Program<WT>> {
    // The forward and backward graphs start while having the empty program.
    let mut forward_graph = Graph::Leaf(Programs::program(vec![]));
    let mut backward_graph = Graph::Leaf(Programs::program(vec![]));
    let enumeration_info = &EnumerationInfo::<W> {
        registers,
        immediates,
    };
    let mut globals = Globals {
        oracle,
        oracle_reduced,
        inputs: vec![],
        outputs: vec![],
        forward_length: 0,
        backward_length: 0,
        extender: reducer,
    };
    // Generate a first input
    println!("Checking empty program");
    match globals.oracle_reduced.check_program(&[]) {
        Ok(()) => return Some(vec![]), // Turns out it's actually the empty program 🤷
        Err((inp, out)) => {
            globals.inputs.push(inp);
            globals.outputs.push(out);
        }
    }
    loop {
        // Searching phase
        println!(
            "Searching forward_length={} backward_length={}",
            globals.forward_length, globals.backward_length
        );
        // println!("Forward Graph: \n{}", forward_graph.pretty_print());
        // println!("Backward Graph: \n{}", backward_graph.pretty_print());
        for inst in Enumerator::new().into_iter(enumeration_info) {
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
        if globals.forward_length + globals.backward_length + 1 == original_length - 1 {
            return None;
        }
        // println!("Forward Graph: \n{}", forward_graph.pretty_print());
        // println!("Backward Graph: \n{}", backward_graph.pretty_print());
        // Expanding phase
        println!("Expanding");
        print_stats(&forward_graph, &backward_graph);
        let should_exapnd_forward = true;
        if should_exapnd_forward {
            expand_forward(&mut forward_graph, &globals.inputs, enumeration_info);
            globals.forward_length += 1;
        } else {
            expand_backward(&mut backward_graph);
            globals.backward_length += 1;
        }
        print_stats(&forward_graph, &backward_graph);
    }
}

enum ConnectAndRefineResult<W: Word> {
    Found(Program<W>),
    Continue,
}

/// WT - word for the target program. WS - word for the synthesis process.
struct Globals<
    WT: Word,
    WS: Word,
    OT: Oracle<[Inst<WT>], State<WT>>,
    OS: Oracle<[Inst<WS>], State<WS>>,
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
}

enum ProgramOrRetry<W: Word> {
    Program(Program<W>),
    Retry,
}

fn connect_and_refine<WT: Word, WS: Word>(
    globals: &mut Globals<
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
    >,
    forward_graph: &mut Graph<WS>,
    backward_graph: &mut Graph<WS>,
    inst: Inst<WS>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult<WT> {
    if k > globals.inputs.len() {
        let mut counter_example_added = false;
        match (&forward_graph, &backward_graph) {
            (Graph::Leaf(prefixes), Graph::Leaf(postfixes)) => {
                println!(
                    "Found leaf graphs at index {k}. Trying to connect them. {}",
                    prefixes.len() * postfixes.len()
                );
                // We found a class of candidate programs.
                // Try each one. If one works, return it. If none work, adds all counter-examples.
                // First, make a buffer to hold the program.
                let mut program =
                    Vec::with_capacity(globals.forward_length + 1 + globals.backward_length);
                let ret = prefixes.try_for_each_ref(&mut |prefix| {
                    debug_assert_eq!(prefix.len(), globals.forward_length);
                    postfixes.try_for_each_ref(&mut |postfix| {
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
                                debug_assert!(
                                    !has_counter_example_been_seen(globals, &inp, &out),
                                    "Counter-example from reduced oracle should not have been seen before."
                                );
                                println!("Oracle found counter example.");
                                println!("  Input: {:?}", &inp);
                                println!("  Expected output: {:?}", &out);
                                println!("  For program:");
                                for inst in &program {
                                    println!("    {inst}");
                                }
                                globals.inputs.push(inp);
                                globals.outputs.push(out);
                                counter_example_added = true;
                                Break(ProgramOrRetry::Retry)
                            }
                        }
                    })
                });
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
        build_forward(forward_graph, &globals.inputs[k - 1..]);
    }

    if matches!(backward_graph, Graph::Leaf(..)) {
        build_backward(backward_graph, &globals.outputs[k - 1..]);
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
fn expand_forward<W: Word>(graph: &mut Graph<W>, inputs: &Vec<State<W>>, ei: &EnumerationInfo<W>) {
    fn inner<W: Word>(
        graph: Graph<W>,
        inputs: &Vec<State<W>>,
        out: &mut Graph<W>,
        ei: &EnumerationInfo<W>,
    ) {
        match graph {
            Graph::Leaf(programs) if programs.is_empty() => {}
            Graph::Leaf(programs) => {
                // Calculate the outputs of the current programs.
                // (All the programs in the same leaf have the same outputs)
                let program = programs
                    .sample()
                    .expect("programs should not be empty here.");
                let outputs: Vec<State<W>> = inputs
                    .iter()
                    .map(|input| {
                        let mut state = input.clone();
                        for inst in &program {
                            inst.run(&mut state);
                        }
                        state
                    })
                    .collect();
                let mut outputs_after_inst = vec![];
                let programs = Rc::new(programs);
                for inst in Enumerator::new().into_iter(ei) {
                    outputs_after_inst.clear();
                    for output in &outputs {
                        let mut next_state = output.clone();
                        inst.run(&mut next_state);
                        outputs_after_inst.push(next_state);
                    }
                    out.insert_all(&outputs_after_inst, programs.clone().concat(inst));
                }
            }
            Graph::Nest(hash_map) => {
                for sub_graph in hash_map.into_values() {
                    inner(sub_graph, inputs, out, ei);
                }
            }
        }
    }

    let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    inner(old_graph, inputs, graph, ei);
}

fn expand_backward<W: Word>(_graph: &mut Graph<W>) {}

fn build_forward<W: Word>(graph: &mut Graph<W>, test_cases_inputs: &[State<W>]) {
    build_forwards_or_backwards(graph, test_cases_inputs, |program, state| {
        for inst in program {
            inst.run(state);
        }
    });
}

fn build_backward<W: Word>(graph: &mut Graph<W>, test_cases_outputs: &[State<W>]) {
    build_forwards_or_backwards::<W>(graph, test_cases_outputs, |program, _state| {
        for _inst in program.iter().rev() {
            todo!("Backward execution not implemented yet.");
        }
    });
}

fn build_forwards_or_backwards<W: Word>(
    graph: &mut Graph<W>,
    initial_states: &[State<W>],
    step: impl Fn(&Program<W>, &mut State<W>),
) {
    assert!(
        !initial_states.is_empty(),
        "Must give initial states to build the graph from."
    );
    // Rebuild the graph.
    // TODO: We can probably avoid completely rebuilding by just removing and adding programs on
    // the same data-structure. This would reduce allocations, but you need to mark which programs
    // have been visited, or store them in a list.
    let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    let mut my_outputs = Vec::with_capacity(initial_states.len());
    old_graph.for_each(&mut |programs| {
        programs.for_each_ref(&mut |program| {
            my_outputs.clear();
            for i in initial_states {
                let mut my_output = i.clone();
                step(&program, &mut my_output);
                my_outputs.push(my_output);
            }
            graph.insert_all(&my_outputs, Programs::Program(program));
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
        WT,
        WS,
        impl Oracle<[Inst<WT>], State<WT>>,
        impl Oracle<[Inst<WS>], State<WS>>,
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

/// Print information that shows us growth and memory usage of the graphs.
fn print_stats<W: Word>(forward_graph: &Graph<W>, backward_graph: &Graph<W>) {
    macro_rules! ignore {
        ( $a:tt, $b:tt ) => {
            $b
        };
    }
    macro_rules! print_row {
        ( $($e:expr),+ $(,)? ) => {
            println!(
                concat!( $( ignore!($e, "{:<15}") ),+ ),
                $( $e ),+
            );
        };
    }

    print_row!["Name", "Depth", "Nodes", "Leaves", "Programs"];
    print_row![
        "Forward",
        forward_graph.depth(),
        forward_graph.n_nodes(),
        forward_graph.n_leaves(),
        forward_graph.n_programs(),
    ];
    print_row![
        "Backward",
        backward_graph.depth(),
        backward_graph.n_nodes(),
        backward_graph.n_leaves(),
        backward_graph.n_programs(),
    ];
}
