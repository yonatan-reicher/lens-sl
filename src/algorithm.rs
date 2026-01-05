use crate::{
    Inst, OpCode, Register, Word64,
    enumerate::{EnumerationInfo, Enumerator},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct State {
    /// This vector is always sorted by register.
    registers: Vec<(Register, u64)>,
}

impl crate::isa::State<Word64> for State {
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
}

fn synthesize(
    test_cases: &[(State, State)],
    registers: &[Register],
    immediates: &[u64],
) -> Vec<Inst<Word64>> {
    let mut i: usize = 0;
    for length in 0..=10 {
        println!("Looking for programs of length {length}.");
        let mut program: Vec<Inst<Word64>> = Vec::with_capacity(length);
        let mut iterators = vec![Enumerator::new(); length];
        let enumeration_info = &EnumerationInfo {
            registers,
            immediates,
        };
        loop {
            // Build the current program.
            program.clear();
            for it in &iterators {
                program.push(it.current(enumeration_info));
            }

            // Test the current program.
            let mut all_match = true;
            for (input, expected_output) in test_cases {
                let mut state = input.clone();
                for inst in &program {
                    inst.run(&mut state);
                }
                if &state != expected_output {
                    all_match = false;
                    break;
                }
            }
            if all_match {
                return program;
            }

            i += 1;
            if i % 1_000_000 == 0 {
                println!("  Tested {i} programs so far...");
            }

            // Advance to the next program.
            let mut index = 0;
            while index < length {
                let o = iterators[index].advance(enumeration_info);
                let done = o.is_none();
                if !done {
                    break;
                }
                iterators[index] = Enumerator::new();
                index += 1;
            }
            if index == length {
                break;
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
            };
            let mut output = input.clone();
            for inst in program {
                inst.run(&mut output);
            }
            (input, output)
        })
        .collect();

    let mut registers: Vec<Register> = vec![];
    let mut immediates: Vec<u64> = vec![0, 1, 2, 3, 4, 5, 6];
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
