use crate::isa::{ArgType, Inst, Register};

pub trait State<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W)>;
}

/// Collects the registers and immediates that appear in a program or in test cases.
#[derive(Debug)]
pub struct Collector {
    pub registers: Vec<Register>,
}

impl Collector {
    pub const fn new() -> Self {
        Self { registers: vec![] }
    }

    // --- Program ---

    pub fn program<W>(&mut self, program: &[Inst<W>]) {
        for inst in program {
            self.inst(*inst);
        }
    }

    fn inst<W>(&mut self, inst: Inst<W>) {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            self.arg::<W>(*arg, arg_type);
        }
    }

    fn arg<W>(&mut self, arg: W, arg_type: ArgType) {
        match arg_type {
            ArgType::Reg => {
                let reg: Register = Register(arg.as_());
                if !self.registers.contains(&reg) {
                    self.registers.push(reg);
                }
            }
            ArgType::Imm | ArgType::Unused => {}
        }
    }

    // --- Test cases ---

    pub fn test_cases<W, S: State<W>>(&mut self, test_cases: &[(S, S)]) {
        for (input, output) in test_cases {
            self.state(input);
            self.state(output);
        }
    }

    fn state<W, S: State<W>>(&mut self, state: &S) {
        for (reg, _) in state.registers() {
            if !self.registers.contains(&reg) {
                self.registers.push(reg);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{bit_vec::ConcreteBitVec, inst};
    use super::*;

    #[test]
    fn test_1() {
        let mut collector = Collector::new();
        collector.program::<ConcreteBitVec<64>>(&[
            inst!(Add, 0, 1, 2),
            inst!(Add, 0, 1, 2),
            inst!(Sub, 0, 3, 2),
            inst!(SubI, 0, 3, 5),
        ]);
        assert_eq!(collector.registers, vec![Register(0), Register(1), Register(2), Register(3)]);
    }
}
