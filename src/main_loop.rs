//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::collect::{self, Collector};
use crate::enumerate::{EnumerationInfo, Enumerator};
use crate::extend_bitwidth;
use crate::graph;
use crate::isa::{self, Flags, Inst, Register};
use crate::programs;
use crate::word::prelude::*;
use rustc_hash::FxHashSet;
use std::ops::ControlFlow::{Break, Continue};
use std::rc::Rc;

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
struct State<W: Word> {
    /// This vector is always sorted by register.
    /// Registers that are not present are not "live".
    pub registers: Vec<(Register, W::Unsigned)>,
    /// The value of the flags register. If None, flags is not "live".
    pub flags: Option<Flags>,
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

    fn convert_to<T: Word>(&self) -> State<T> {
        State {
            registers: self.registers.iter().map(|(r, v)| (*r, v.as_())).collect(),
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

    fn get_flags(&self) -> Flags {
        self.flags.expect("Flags not set in state.")
    }

    fn set_flags(&mut self, flags: Flags) {
        self.flags = Some(flags);
    }
}

impl<W: Word> collect::State<W> for State<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W::Unsigned)> {
        self.registers.iter().cloned()
    }
}

// =========================================== Graph ==============================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Programs<W>>;

// ========================================== Oracle ==============================================

type CounterExample<W> = (State<W>, State<W>);

/// In the future, we will have a solver implement this trait.
trait Oracle<W: Word> {
    fn check_program(&mut self, program: &[Inst<W>]) -> Result<(), CounterExample<W>>;
}

struct TestCasesOracle<W: Word> {
    test_cases: Vec<CounterExample<W>>,
}

impl<W: Word> Oracle<W> for TestCasesOracle<W> {
    fn check_program(&mut self, program: &[Inst<W>]) -> Result<(), CounterExample<W>> {
        // Maybe we could not check test cases again, but it's probably not really slowing us down.
        for (i, (input, expected_output)) in self.test_cases.iter().enumerate() {
            let mut output = input.clone();
            for inst in program {
                inst.run(&mut output);
            }
            if &output != expected_output {
                println!(
                    "Oracle found counter example {}/{}.",
                    i + 1,
                    self.test_cases.len()
                );
                println!("  Input: {input:?}");
                println!("  Expected output: {expected_output:?}");
                println!("  Actual output: {output:?}");
                return Err((input.clone(), expected_output.clone()));
            }
        }
        Ok(())
    }
}

// ====================================== Implementation ==========================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word, WS: Word>(
    program: &[Inst<WT>],
    inputs: &[&[(Register, WT::Unsigned)]], // TODO: Return a program in Program<WT> instead...
) -> Program<WS> {
    let program: Vec<Inst<WS>> = program.iter().map(|inst| inst.convert_to()).collect();
    let program = program.as_slice();

    // Run the program on each input to get the outputs. We call these "test cases".
    let test_cases: Vec<(State<WS>, State<WS>)> = inputs
        .iter()
        .map(|input| {
            let input: State<WS> = State {
                registers: input.iter().map(|(r, v)| (*r, v.as_())).collect(),
                flags: None,
            };
            let mut output = input.clone();
            for inst in program {
                inst.convert_to().run(&mut output);
            }
            (input, output)
        })
        .collect();

    // Collect all the registers and immediates that might be useful for synthesis.
    let mut collector = Collector::<WS>::new();
    collector.program(program);
    collector.test_cases(&test_cases);
    let Collector {
        registers,
        immediates,
    } = collector;
    let immediates: Vec<WS::Unsigned> = immediates.into_iter().map(|imm| imm.as_()).collect();

    let oracle = TestCasesOracle { test_cases };

    synthesize::<WT, WS>(&registers, &immediates, oracle)
}

fn synthesize<WT: Word, W: Word>(
    registers: &[Register],
    immediates: &[W::Unsigned],
    oracle: impl Oracle<W>,
) -> Program<W> {
    // The forward and backward graphs start while having the empty program.
    let mut forward_graph = Graph::Leaf(Programs::program(vec![]));
    let mut backward_graph = Graph::Leaf(Programs::program(vec![]));
    let enumeration_info = &EnumerationInfo::<W> {
        registers,
        immediates,
    };
    let mut globals = Globals {
        oracle,
        inputs: vec![],
        outputs: vec![],
        forward_length: 0,
        backward_length: 0,
        extender: extend_bitwidth::Extender::new(immediates.iter().cloned()),
    };
    // Generate a first input
    println!("Checking empty program");
    match globals.oracle.check_program(&[]) {
        Ok(_) => return vec![], // Turns out it's actually the empty program 🤷
        Err(counter_example) => {
            globals.inputs.push(counter_example.0);
            globals.outputs.push(counter_example.1);
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
            let res = connect_and_refine(
                &mut globals,
                &mut forward_graph,
                &mut backward_graph,
                inst,
                1,
            );
            match res {
                ConnectAndRefineResult::Found(prog) => {
                    println!("Found program of length {}", prog.len());
                    return prog;
                }
                ConnectAndRefineResult::Continue => {}
            }
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
struct Globals<WT: Word, WS: Word, O: Oracle<WT>> {
    oracle: O,
    inputs: Vec<State<WS>>,
    outputs: Vec<State<WS>>,
    /// The length of the prefixes of the program being built.
    forward_length: usize,
    backward_length: usize,
    extender: extend_bitwidth::Extender<WT, WS>,
}

fn connect_and_refine<WT: Word, WS: Word>(
    globals: &mut Globals<WT, WS, impl Oracle<WT>>,
    forward_graph: &mut Graph<WS>,
    backward_graph: &mut Graph<WS>,
    inst: Inst<WS>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult<WT> {
    if k > globals.inputs.len() {
        match (&forward_graph, &backward_graph) {
            (Graph::Leaf(prefixes), Graph::Leaf(postfixes)) => {
                // We found a class of candidate programs.
                // Try each one. If one works, return it. If none work, adds all counter-examples.
                // First, make a buffer to hold the program.
                let mut program =
                    Vec::with_capacity(globals.forward_length + 1 + globals.backward_length);
                let mut found = false;
                let mut counter_examples = FxHashSet::default();
                let _ = prefixes.try_for_each_ref(&mut |prefix| {
                    debug_assert_eq!(prefix.len(), globals.forward_length);
                    postfixes.try_for_each_ref(&mut |postfix| {
                        debug_assert_eq!(postfix.len(), globals.backward_length);
                        // Build the current candidate program.
                        program.clear();
                        program.extend(prefix.iter());
                        program.push(inst);
                        program.extend(postfix.iter());
                        println!("Found candidate program:");
                        for inst in &program {
                            println!("  {inst}");
                        }
                        match globals.oracle.check_program(&program) {
                            // Found!
                            Ok(()) => {
                                found = true;
                                Break(())
                            }
                            Err(counter_example) => {
                                if !counter_examples.contains(&counter_example) {
                                    counter_examples.insert(counter_example.clone());
                                    globals.inputs.push(counter_example.0);
                                    globals.outputs.push(counter_example.1);
                                }
                                Continue(())
                            }
                        }
                    })
                });
                if found {
                    return ConnectAndRefineResult::Found(program);
                }
            }
            _ => {
                println!("Graphs are not leaves at the end.");
                println!("Forward Graph: \n{}", forward_graph.pretty_print());
                println!("Backward Graph: \n{}", backward_graph.pretty_print());
                panic!();
            }
        }
    }

    if matches!(forward_graph, Graph::Leaf(..)) {
        println!("Building forward");
        build_forward(forward_graph, &globals.inputs[k - 1..]);
    }

    if matches!(backward_graph, Graph::Leaf(..)) {
        println!("Building backward");
        build_backward(backward_graph, &globals.outputs[k - 1..]);
    }

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
            // println!("  Found matching state: {next}");
            // println!("  k = {k}  inputs.len()={}", inputs.len());
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
    // Rebuild the graph.
    let old_graph = std::mem::replace(graph, Graph::Nest(Default::default()));
    let mut my_outputs = Vec::with_capacity(initial_states.len());
    old_graph.for_each(&mut |programs| {
        let program = programs
            .sample()
            .expect("programs should not be empty here.");
        my_outputs.clear();
        for i in initial_states {
            let mut my_output = i.clone();
            step(&program, &mut my_output);
            my_outputs.push(my_output);
        }
        graph.insert_all(&my_outputs, programs);
    });
}

/// Print information that shows us growth and memory usage of the graphs.
fn print_stats<W: Word>(forward_graph: &Graph<W>, backward_graph: &Graph<W>) {
    macro_rules! ignore {
        ( $a:tt, $b:tt ) => {
            $b
        };
    }
    macro_rules! print_row {
        ( $($e:expr),+ ) => {
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
        forward_graph.depth()
    ];
    print_row![
        "Backward",
        backward_graph.depth(),
        backward_graph.n_nodes(),
        backward_graph.n_leaves(),
        backward_graph.n_programs(),
        backward_graph.depth()
    ];
}
