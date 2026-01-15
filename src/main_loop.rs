//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::enumerate::{EnumerationInfo, Enumerator};
use crate::graph;
use crate::isa::{self, ArgType, Flags, Inst, Register, Word64};
use functionality::prelude::*;

// =========================================== State ==============================================

/// The state of the machine at a given point in time.
#[derive(Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
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
struct State {
    /// This vector is always sorted by register.
    /// Registers that are not present are not "live".
    pub registers: Vec<(Register, u64)>,
    /// The value of the flags register. If None, flags is not "live".
    pub flags: Option<Flags>,
}

impl isa::State for State {
    type W = Word64;

    fn get_register(&self, reg: Register) -> u64 {
        for (r, v) in &self.registers {
            if *r == reg {
                return *v;
            }
        }
        panic!("Register {reg:?} not found in state.");
    }

    fn set_register(&mut self, reg: Register, value: u64) {
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

type Program = Vec<Inst<Word64>>;

// =========================================== Graph ==============================================

type Graph = graph::Graph<State, Program>;

// ========================================== Oracle ==============================================

type CounterExample = (State, State);

/// In the future, we will have a solver implement this trait.
trait Oracle {
    fn check_program(&mut self, program: &[Inst<Word64>]) -> Result<(), CounterExample>;
}

struct TestCasesOracle {
    test_cases: Vec<CounterExample>,
}

impl Oracle for TestCasesOracle {
    fn check_program(&mut self, program: &[Inst<Word64>]) -> Result<(), CounterExample> {
        // Maybe we could not check test cases again, but it's probably not really slowing us down.
        for (input, expected_output) in &self.test_cases {
            let mut output = input.clone();
            for inst in program {
                inst.run(&mut output);
            }
            if &output != expected_output {
                println!("Oracle found counter example.");
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
pub fn optimize(program: &[Inst<Word64>], inputs: &[&[(Register, u64)]]) -> Program {
    let test_cases: Vec<(State, State)> = inputs
        .iter()
        .map(|input| {
            let input = State {
                registers: input.to_vec(),
                flags: None,
            };
            let mut output = input.clone();
            for inst in program {
                inst.run(&mut output);
            }
            (input, output)
        })
        .collect();

    let mut registers: Vec<Register> = vec![];
    // let immediates: Vec<u64> = vec![0, 1, 2];
    let mut immediates: Vec<u64> = vec![];
    for (input, output) in &test_cases {
        for (reg, val) in &input.registers {
            if !registers.contains(reg) {
                registers.push(*reg);
            }
            if !immediates.contains(val) {
                immediates.push(*val);
            }
        }
        for (reg, val) in &output.registers {
            if !registers.contains(reg) {
                registers.push(*reg);
            }
            if !immediates.contains(val) {
                immediates.push(*val);
            }
        }
    }
    for inst in program {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            match arg_type {
                ArgType::Reg => {
                    let reg: Register = Register(*arg as _);
                    if !registers.contains(&reg) {
                        registers.push(reg);
                    }
                }
                ArgType::Imm => {
                    let imm: u64 = *arg;
                    if !immediates.contains(&imm) {
                        immediates.push(imm);
                    }
                }
                ArgType::Unused => {}
            }
        }
    }

    let oracle = TestCasesOracle { test_cases };

    synthesize(&registers, &immediates, oracle)
}

fn synthesize(registers: &[Register], immediates: &[u64], mut oracle: impl Oracle) -> Program {
    // The length of the prefixes of the program being built.
    let mut forward_length = 0;
    let mut backward_length = 0;
    // The forward and backward graphs start while having the empty program.
    let mut forward_graph = Graph::Leaf(vec![vec![]]);
    let mut backward_graph = Graph::Leaf(vec![vec![]]);
    let enumeration_info = &EnumerationInfo {
        registers,
        immediates,
    };
    let mut inputs = vec![];
    let mut outputs = vec![];
    // Generate a first input
    match oracle.check_program(&[]) {
        Ok(_) => return vec![], // Turns out it's actually the empty program 🤷
        Err(counter_example) => {
            inputs.push(counter_example.0);
            outputs.push(counter_example.1);
        }
    }
    loop {
        // Searching phase
        println!("Searching lF={forward_length} lB={backward_length}");
        // println!("Forward Graph: \n{}", forward_graph.pretty_print());
        // println!("Backward Graph: \n{}", backward_graph.pretty_print());
        for inst in Enumerator::new().into_iter(enumeration_info) {
            let res = connect_and_refine(
                &mut oracle,
                &mut inputs,
                &mut outputs,
                forward_length,
                backward_length,
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
        let should_exapnd_forward = true;
        if should_exapnd_forward {
            expand_forward(&mut forward_graph, &inputs, enumeration_info);
            forward_length += 1;
        } else {
            expand_backward(&mut backward_graph);
            backward_length += 1;
        }
    }
}

enum ConnectAndRefineResult {
    Found(Program),
    Continue,
}

fn connect_and_refine(
    oracle: &mut impl Oracle,
    inputs: &mut Vec<State>,
    outputs: &mut Vec<State>,
    forward_length: usize,
    backward_length: usize,
    forward_graph: &mut Graph,
    backward_graph: &mut Graph,
    inst: Inst<Word64>,
    // This is the index of the input/output pair we are currently trying to connect.
    k: usize,
) -> ConnectAndRefineResult {
    if k > inputs.len() {
        match (&forward_graph, &backward_graph) {
            (Graph::Leaf(prefixes), Graph::Leaf(postfixes)) => {
                let mut program = Vec::with_capacity(forward_length + 1 + backward_length);
                'my_break: for prefix in prefixes {
                    debug_assert_eq!(prefix.len(), forward_length);
                    for postfix in postfixes {
                        debug_assert_eq!(postfix.len(), backward_length);
                        program.clear();
                        program.extend(prefix.iter());
                        program.push(inst);
                        program.extend(postfix.iter());
                        println!("Found candidate program:");
                        for inst in &program {
                            println!("  {inst}");
                        }
                        // TODO: try this program
                        match oracle.check_program(&program) {
                            Ok(()) => {
                                return ConnectAndRefineResult::Found(program);
                            }
                            Err(counter_example) => {
                                inputs.push(counter_example.0);
                                outputs.push(counter_example.1);
                                if true {
                                    break 'my_break;
                                }
                            }
                        }
                    }
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
        build_forward(forward_graph, &inputs[k - 1..]);
    }

    if matches!(backward_graph, Graph::Leaf(..)) {
        println!("Building backward");
        build_backward(backward_graph, &outputs[k - 1..]);
    }

    let Graph::Nest(forward_outputs) = forward_graph else {
        panic!();
    };
    let Graph::Nest(backward_outputs) = backward_graph else {
        panic!();
    };
    for (forward_output, forward_subgraph) in forward_outputs {
        let mut next = forward_output.clone();
        inst.run(&mut next);
        if let Some(backward_subgraph) = backward_outputs.get_mut(&next) {
            // println!("  Found matching state: {next}");
            // println!("  k = {k}  inputs.len()={}", inputs.len());
            return connect_and_refine(
                oracle,
                inputs,
                outputs,
                forward_length,
                backward_length,
                forward_subgraph,
                backward_subgraph,
                inst,
                k + 1,
            );
        }
    }
    ConnectAndRefineResult::Continue
}

/// Go through each program prefix in the graph, and expand it by one
/// instruction forward. This is done for each program, and for each
/// instruction.
fn expand_forward(graph: &mut Graph, inputs: &Vec<State>, ei: &EnumerationInfo) {
    fn inner(graph: &Graph, inputs: &Vec<State>, out: &mut Graph, ei: &EnumerationInfo) {
        match graph {
            Graph::Leaf(programs) if programs.is_empty() => {}
            Graph::Leaf(programs) => {
                // Calculate the outputs of the current programs.
                // (All the programs in the same leaf have the same outputs)
                let program = &programs[0];
                let outputs: Vec<State> = inputs.iter()
                    .map(|input| {
                        let mut state = input.clone();
                        for inst in program {
                            inst.run(&mut state);
                        }
                        state
                    })
                    .collect();
                let mut outputs_after_inst = vec![];
                for inst in Enumerator::new().into_iter(ei) {
                    outputs_after_inst.clear();
                    for output in &outputs {
                        let mut next_state = output.clone();
                        inst.run(&mut next_state);
                        outputs_after_inst.push(next_state);
                    }
                    out.insert_all(
                        &outputs_after_inst,
                        programs.iter().map(|p| {
                            Vec::with_capacity(p.len() + 1)
                                .mutate(|v| v.extend(p))
                                .mutate(|v| v.push(inst))
                        }).collect::<Vec<_>>(),
                    );
                }
            }
            Graph::Nest(hash_map) => {
                for sub_graph in hash_map.values() {
                    inner(sub_graph, inputs, out, ei);
                }
            }
        }
    }

    let mut out = Graph::Nest(Default::default());
    inner(graph, inputs, &mut out, ei);
    *graph = out;
}

fn expand_backward(graph: &mut Graph) {}

fn build_forward(graph: &mut Graph, test_cases_inputs: &[State]) {
    build_forwards_or_backwards(graph, test_cases_inputs, |program, state| {
        for inst in program {
            inst.run(state);
        }
    });
}

fn build_backward(graph: &mut Graph, test_cases_outputs: &[State]) {
    build_forwards_or_backwards(graph, test_cases_outputs, |program, _state| {
        for _inst in program.iter().rev() {
            todo!("Backward execution not implemented yet.");
        }
    });
}

fn build_forwards_or_backwards(
    graph: &mut Graph,
    initial_states: &[State],
    step: impl Fn(&Program, &mut State),
) {
    // Rebuild the graph.
    let old_graph = std::mem::take(graph);
    *graph = Graph::Nest(Default::default());
    let mut my_outputs = Vec::with_capacity(initial_states.len());
    old_graph.for_each(&mut |program| {
        my_outputs.clear();
        for i in initial_states {
            let mut my_output = i.clone();
            step(&program, &mut my_output);
            my_outputs.push(my_output);
        }
        graph.insert_all(&my_outputs, [program.clone()]);
    });
}
