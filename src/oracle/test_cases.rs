use super::{CounterExample, Oracle};

/// An oracle that only checks against a fixed set of input-output pairs.
#[derive(Debug)]
pub struct TestCasesOracle<S> {
    pub test_cases: Vec<CounterExample<S>>,
}

pub trait Program<S> {
    fn run(&self, state: &mut S);
}

impl<P: Program<S> + ?Sized, S: Clone + Default + Eq> Oracle<P, S> for TestCasesOracle<S> {
    fn check_program(&mut self, program: &P) -> Result<(), CounterExample<S>> {
        // Maybe we could not check test cases again, but it's probably not really slowing us down.
        for (input, expected_output) in self.test_cases.iter() {
            let mut output = input.clone();
            program.run(&mut output);
            if &output != expected_output {
                return Err((input.clone(), output));
            }
        }
        Ok(())
    }
}

use crate::arm::Inst;
use crate::arm::state::State as ArmState;
use crate::word::{HasBitWord, Word};
impl<W: Word + HasBitWord> Program<ArmState<W>> for [Inst<W>] {
    fn run(&self, state: &mut ArmState<W>) {
        for inst in self {
            inst.run(state);
        }
    }
}
