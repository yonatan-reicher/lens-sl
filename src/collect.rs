use crate::isa::{ArgType, Inst, Register, Word};
use num_traits::AsPrimitive;

pub trait State<W: Word> {
    fn registers(&self) -> impl Iterator<Item = (Register, W::Unsigned)>;
}

/// Collects the registers and immediates that appear in a program or in test cases.
#[derive(Debug)]
pub struct Collector<W: Word> {
    pub registers: Vec<Register>,
    pub immediates: Vec<W::Unsigned>,
}

impl<W: Word> Collector<W> {
    pub fn new() -> Self {
        Self {
            registers: vec![],
            immediates: vec![],
        }
    }

    // --- Program ---

    pub fn program(&mut self, program: &[Inst<W>]) {
        for inst in program {
            self.inst(*inst);
        }
    }

    fn inst(&mut self, inst: Inst<W>) {
        for (arg, arg_type) in inst.args.iter().zip(inst.op_code.arg_types()) {
            self.arg(*arg, arg_type);
        }
    }

    fn arg(&mut self, arg: W::Unsigned, arg_type: ArgType) {
        match arg_type {
            ArgType::Reg => {
                let reg: Register = Register(arg.as_());
                if !self.registers.contains(&reg) {
                    self.registers.push(reg);
                }
            }
            ArgType::Imm => {
                let imm = arg;
                if !self.immediates.contains(&imm) {
                    self.immediates.push(imm);
                }
            }
            ArgType::Unused => {}
        }
    }

    // --- Test cases ---

    pub fn test_cases<S: State<W>>(&mut self, test_cases: &[(S, S)]) {
        for (input, output) in test_cases {
            self.state(input);
            self.state(output);
        }
    }

    fn state<S: State<W>>(&mut self, state: &S) {
        for (reg, value) in state.registers() {
            if !self.registers.contains(&reg) {
                self.registers.push(reg);
            }
            if !self.immediates.contains(&value) {
                self.immediates.push(value);
            }
        }
    }
}
