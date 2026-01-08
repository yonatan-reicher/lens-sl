use functionality::prelude::*;

use crate::{
    Flags, Inst, Register, Word64,
    enumerate::{EnumerationInfo, Enumerator},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct State {
    /// This vector is always sorted by register.
    registers: Vec<(Register, u64)>,
    flags: Option<Flags>,
}

impl crate::isa::State for State {
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

type Graph = crate::graph::Graph<State, Program>;

fn synthesize(
    test_cases: &[(State, State)],
    registers: &[Register],
    immediates: &[u64],
) -> Vec<Inst<Word64>> {
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
    let test_cases_inputs = test_cases
        .iter()
        .map(|(input, _)| input.clone())
        .collect::<Vec<_>>();
    let test_cases_outputs = test_cases
        .iter()
        .map(|(_, output)| output.clone())
        .collect::<Vec<_>>();
    loop {
        // Searching phase
        println!("Searching lF={forward_length} lB={backward_length}");
        for inst in Enumerator::new().into_iter(enumeration_info) {
            let res = connect_and_refine(
                &test_cases_inputs,
                &test_cases_outputs,
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
        println!("Expanding");
        let should_exapnd_forward = true;
        if should_exapnd_forward {
            expand_forward(&mut forward_graph, enumeration_info);
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
    test_cases_inputs: &[State],
    test_cases_outputs: &[State],
    forward_length: usize,
    backward_length: usize,
    forward_graph: &mut Graph,
    backward_graph: &mut Graph,
    inst: Inst<Word64>,
    k: usize,
) -> ConnectAndRefineResult {
    if k > test_cases_inputs.len() {
        match (&forward_graph, &backward_graph) {
            (Graph::Leaf(prefixes), Graph::Leaf(postfixes)) => {
                let mut program = Vec::with_capacity(forward_length + 1 + backward_length);
                for prefix in prefixes {
                    debug_assert_eq!(prefix.len(), forward_length);
                    for postfix in postfixes {
                        debug_assert_eq!(postfix.len(), backward_length);
                        program.clear();
                        program.extend(prefix.iter());
                        program.push(inst);
                        program.extend(postfix.iter());
                        // TODO: try this program
                        if true {
                            return ConnectAndRefineResult::Found(program);
                        }
                    }
                }
            }
            _ => {
                println!("Graphs are not leaves at the end.");
                dbg!(forward_graph);
                dbg!(backward_graph);
                panic!();
            }
        }
    }

    if matches!(forward_graph, Graph::Leaf(..)) {
        println!("  Building forward");
        build_forward(forward_graph, test_cases_inputs, k);
    }

    if matches!(backward_graph, Graph::Leaf(..)) {
        println!("  Building backward");
        build_backward(backward_graph, test_cases_outputs, k);
    }

    let Graph::Nest(forward_outputs) = forward_graph else {
        panic!();
    };
    let Graph::Nest(backward_outputs) = backward_graph else {
        panic!();
    };
    println!("  Trying to connect forward and backward at k={k}");
    for (forward_output, forward_subgraph) in forward_outputs {
        let mut next = forward_output.clone();
        inst.run(&mut next);
        if let Some(backward_subgraph) = backward_outputs.get_mut(&next) {
            return connect_and_refine(
                test_cases_inputs,
                test_cases_outputs,
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
fn expand_forward(graph: &mut Graph, ei: &EnumerationInfo) {
    fn inner(graph: &Graph, outputs: &mut Vec<State>, out: &mut Graph, ei: &EnumerationInfo) {
        match graph {
            Graph::Leaf(programs) => {
                for inst in Enumerator::new().into_iter(ei) {
                    let outputs: Vec<_> = outputs
                        .iter()
                        .cloned()
                        .map(|mut o| {
                            inst.run(&mut o);
                            o
                        })
                        .collect();
                    let mut expanded_program = vec![];
                    for program in programs {
                        expanded_program.clear();
                        expanded_program.extend(program.iter());
                        expanded_program.push(inst);
                    }
                    out.insert_all(
                        &outputs,
                        programs.iter().map(|p| {
                            Vec::with_capacity(p.len() + 1)
                                .mutate(|v| v.extend(p))
                                .mutate(|v| v.push(inst))
                        }),
                    );
                }
            }
            Graph::Nest(hash_map) => {
                for (state, sub_graph) in hash_map {
                    outputs.push(state.clone());
                    inner(sub_graph, outputs, out, ei);
                    outputs.pop();
                }
            }
        }
    }

    let mut out = Graph::Nest(Default::default());
    inner(graph, &mut vec![], &mut out, ei);
    *graph = out;
}

fn expand_backward(graph: &mut Graph) {
}

fn build_forward(graph: &mut Graph, test_cases_inputs: &[State], k: usize) {
    let programs = match graph {
        Graph::Leaf(items) => std::mem::take(items),
        Graph::Nest(hash_map) => todo!(),
    };
    // Rebuild the graph.
    *graph = Graph::Nest(Default::default());
    let mut my_outputs = Vec::with_capacity(k);
    for program in programs {
        my_outputs.clear();
        for i in &test_cases_inputs[..k] {
            let mut my_output = i.clone();
            for inst in &program {
                inst.run(&mut my_output);
            }
            my_outputs.push(my_output);
        }
        graph.insert_all(&my_outputs, [program.clone()]);
    }
}

fn build_backward(graph: &mut Graph, test_cases_outputs: &[State], k: usize) {
    let programs = match graph {
        Graph::Leaf(items) => std::mem::take(items),
        Graph::Nest(hash_map) => todo!(),
    };
    // Rebuild the graph.
    *graph = Graph::Nest(Default::default());
    let mut my_inputs = Vec::with_capacity(k);
    for program in programs {
        my_inputs.clear();
        for o in &test_cases_outputs[..k] {
            let mut my_input = o.clone();
            for inst in program.iter().rev() {
                // inst.run_backward(&mut my_input);
                todo!();
            }
            my_inputs.push(my_input);
        }
        graph.insert_all(&my_inputs, [program.clone()]);
    }
}

pub fn optimize(program: &[Inst<Word64>], inputs: &[&[(Register, u64)]]) -> Vec<Inst<Word64>> {
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
    let immediates: Vec<u64> = vec![0, 1, 2, 3, 4, 5, 6];
    for (input, output) in &test_cases {
        for (reg, _) in &input.registers {
            if !registers.contains(reg) {
                registers.push(*reg);
            }
        }
        for (reg, _) in &output.registers {
            if !registers.contains(reg) {
                registers.push(*reg);
            }
        }
    }
    for inst in program {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            if arg_type != crate::isa::ArgType::Reg {
                continue;
            }
            let reg: Register = Register(*arg as _);
            if !registers.contains(&reg) {
                registers.push(reg);
            }
        }
    }

    synthesize(&test_cases, &registers, &immediates)
}
