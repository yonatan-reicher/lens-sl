use crate::{
    Flags, Inst, Register, Word64,
    enumerate::{EnumerationInfo, Enumerator},
    graph::Graph,
    shortest_path::shortest_path,
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

fn synthesize(
    test_cases: &[(State, State)],
    registers: &[Register],
    immediates: &[u64],
) -> Vec<Inst<Word64>> {
    let mut i: usize = 0;
    let mut graph = Graph::<Inst<Word64>, Vec<State>>::new();
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
    // Build up to N deep just so we stop eventually.
    const N: usize = 10;
    for iter in 0..N {
        println!("Iteration {iter} - i={i}.");
        let enumerator = Enumerator::new();
        for inst in enumerator.into_iter(enumeration_info) {
            // Insert the current instruction.
            let mut all_states = graph.all_states().cloned().collect::<Vec<_>>();
            if all_states.is_empty() {
                all_states.push(test_cases_inputs.clone());
            }
            for s1 in all_states {
                let s2 = s1
                    .iter()
                    .map(|s1| {
                        let mut s2 = s1.clone();
                        inst.run(&mut s2);
                        s2
                    })
                    .collect::<Vec<_>>();
                graph.insert_forward(inst, s1, s2.clone());
                if s2 == test_cases_outputs {
                    // We are done!
                    let v = shortest_path(
                        &test_cases_inputs,
                        |states| {
                            graph
                                .get_forward(states)
                                .unwrap_or_default()
                                .map(|(k, v)| (v.clone(), k.clone()))
                        },
                        |states| states == &test_cases_outputs,
                    )
                    .unwrap();
                    return v.into_iter().map(|insts| insts[0]).collect();
                }

                i += 1;
                if i % 1_000_000 == 0 {
                    println!("  Tested {i} programs so far...");
                }
            }
        }
    }
    panic!("No program found.");
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
