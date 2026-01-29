use crate::isa::{ArgType, Inst, Register};
use crate::word::prelude::*;

pub trait State<W: Word> {
    fn registers(&self) -> impl Iterator<Item = (Register, W::Unsigned)>;
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

    pub fn program<W: Word>(&mut self, program: &[Inst<W>]) {
        for inst in program {
            self.inst(*inst);
        }
    }

    fn inst<W: Word>(&mut self, inst: Inst<W>) {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            self.arg::<W>(*arg, arg_type);
        }
    }

    fn arg<W: Word>(&mut self, arg: W::Unsigned, arg_type: ArgType) {
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
