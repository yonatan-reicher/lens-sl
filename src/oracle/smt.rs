use super::Oracle;
use crate::word::prelude::*;
use smtlib::{
    prelude::*,
    backend::cvc5_binary::Cvc5Binary,
    Solver,
    Storage,
    BitVec,
    Bool,
};

#[derive(Debug)]
struct SmtOracle;

impl<I: Inst> Oracle<[I], I::State> for SmtOracle {
    fn check_program(&mut self, program: &[I]) -> Result<(), super::CounterExample<I::State>> {
        let st = Storage::new();
        let cvc5 = Cvc5Binary::new("cvc5").expect("failed to initialize cvc5 - binary 'cvc5' not found on path");
        let mut solver = Solver::new(&st, cvc5);

        let mut state_vars: Vec<I::SymbolicState> = (0..=program.len())

        todo!()
    }
}

pub trait Inst {
    type State;
    type SymbolicState<'st>;

    fn new_const<'st>() -> Self::SymbolicState<'st>;
    fn step(&self, s: Self::SymbolicState) -> Self::SymbolicState;
}

/// An number representing which point in the run SMT constants (symbolic variables) refer to. A 0
/// is before the run of the first instruction, a 1 is after it, and `n` is after the run of the
/// nth instruction.
type Step = usize;
