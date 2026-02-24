use super::Oracle;
use crate::isa::{Inst, State};
use crate::word::prelude::*;
use smtlib::{
    prelude::*,
    backend::cvc5_binary::Cvc5Binary,
    Solver,
    Storage,
    terms::Const,
    BitVec,
    Bool,
};

struct SmtOracle<const BITS: usize> {}

type Program<W> = [Inst<W>];

impl<S: State<W>, W: Word> Oracle<Program<W>, S> for SmtOracle {
    fn check_program(&mut self, program: &Program<W>) -> Result<(), super::CounterExample<S>> {
        let st = Storage::new();
        let cvc5 = Cvc5Binary::new("cvc5").expect("failed to initialize cvc5 - binary 'cvc5' not found on path");
        let mut solver = Solver::new(&st, cvc5);

        let mut state_vars: Vec<StateVars> = (0..=program.len())
            .map(|step| StateVars::new(&st, step))
            .collect();

        todo!()
    }
}

#[derive(Debug, Clone)]
struct StateVars<'st, const BITS: usize> 
{
    registers: [Const<'st, BitVec<'st, BITS>>; 16],
    flags: FlagVars<'st>,
}

#[derive(Debug, Clone)]
struct FlagVars<'st> {
    pub z: Const<'st, Bool<'st>>,
    pub n: Const<'st, Bool<'st>>,
    pub c: Const<'st, Bool<'st>>,
    pub v: Const<'st, Bool<'st>>,
}

/// An number representing which point in the run SMT constants (symbolic variables) refer to. A 0
/// is before the run of the first instruction, a 1 is after it, and `n` is after the run of the
/// nth instruction.
type Step = usize;

impl<'st, const BITS: usize> StateVars<'st, BITS> {
    pub fn new(st: &'st Storage, step: Step) -> Self {
        Self {
            registers: [
                BitVec::new_const(st, &format!("r0_{}", step)),
                BitVec::new_const(st, &format!("r1_{}", step)),
                BitVec::new_const(st, &format!("r2_{}", step)),
                BitVec::new_const(st, &format!("r3_{}", step)),
                BitVec::new_const(st, &format!("r4_{}", step)),
                BitVec::new_const(st, &format!("r5_{}", step)),
                BitVec::new_const(st, &format!("r6_{}", step)),
                BitVec::new_const(st, &format!("r7_{}", step)),
                BitVec::new_const(st, &format!("r8_{}", step)),
                BitVec::new_const(st, &format!("r9_{}", step)),
                BitVec::new_const(st, &format!("r10_{}", step)),
                BitVec::new_const(st, &format!("r11_{}", step)),
                BitVec::new_const(st, &format!("r12_{}", step)),
                BitVec::new_const(st, &format!("r13_{}", step)),
                BitVec::new_const(st, &format!("r14_{}", step)),
                BitVec::new_const(st, &format!("r15_{}", step)),
            ],
            flags: FlagVars::new(st, step),
        }
    }
}

impl<'st> FlagVars<'st> {
    pub fn new(st: &'st Storage, step: Step) -> Self {
        Self {
            z: Bool::new_const(st, &format!("z_{}", step)),
            n: Bool::new_const(st, &format!("n_{}", step)),
            c: Bool::new_const(st, &format!("c_{}", step)),
            v: Bool::new_const(st, &format!("v_{}", step)),
        }
    }
}
