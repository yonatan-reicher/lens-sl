use crate::arm::{ArgType, Inst, Register};
use crate::word::prelude::*;

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

    pub fn program<W: Word, WShift: Word>(&mut self, program: &[Inst<W, WShift>]) {
        for inst in program {
            self.inst(*inst);
        }
    }

    fn inst<W: Word, WShift: Word>(&mut self, inst: Inst<W, WShift>) {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            self.arg::<W>(*arg, arg_type);
        }
    }

    fn arg<W: Word>(&mut self, arg: W, arg_type: ArgType) {
        match arg_type {
            ArgType::Reg(..) => {
                let reg = Register::from(arg);
                if !self.registers.contains(&reg) {
                    self.registers.push(reg);
                }
            }
            ArgType::Imm | ArgType::Unused => {}
        }
    }

    // --- Test cases ---

    #[allow(unused)]
    pub fn test_cases<W: Word, S: State<W>>(&mut self, test_cases: &[(S, S)]) {
        for (input, output) in test_cases {
            self.state(input);
            self.state(output);
        }
    }

    fn state<W: Word, S: State<W>>(&mut self, state: &S) {
        for (reg, _) in state.registers() {
            if !self.registers.contains(&reg) {
                self.registers.push(reg);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Word64;
    use crate::inst;

    #[test]
    fn test_1() {
        let mut collector = Collector::new();
        collector.program::<Word64, Word6>(&[
            inst!(Add, 0, 1, 2),
            inst!(Add, 0, 1, 2),
            inst!(Sub, 0, 3, 2),
            inst!(SubI, 0, 3, 5),
        ]);
        assert_eq!(
            collector.registers,
            vec![Register(0), Register(1), Register(2), Register(3)]
        );
    }
}
