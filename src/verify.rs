use crate::arm::{Inst, State, extend_program_for_each};
use crate::oracle::Oracle;
use crate::reduce_bit_width::Reducer;
use crate::word::{HasBitWord, Word};
use std::ops::ControlFlow;

pub fn verify<T, WBig, WSmall>(
    program: &[Inst<WSmall>],
    reducer: &Reducer<WBig, WSmall>,
    oracle_reduced: &mut impl Oracle<[Inst<WSmall>], State<WSmall>>,
    oracle: &mut impl Oracle<[Inst<WBig>], State<WBig>>,
    mut on_found_program: impl FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
) -> Result<WSmall, T>
where
    WSmall: Word + HasBitWord,
    WBig: Word + HasBitWord,
{
    match oracle_reduced.check_program(program) {
        Ok(()) => {
            // The reduced program is equivalent. Because of that, we un-reduce it to lots of
            // possible programs, and see if there are any equivalent.
            extend_program_for_each(program, reducer, |program| {
                match oracle.check_program(program) {
                    Ok(()) => on_found_program(program),
                    Err(_) => ControlFlow::Continue(()),
                }
            })
            .into()
        }
        Err((inp, out)) => Result::CounterExample(inp, out),
    }
}

pub enum Result<W, T> {
    CounterExample(State<W>, State<W>),
    Break(T),
    Continue,
}

impl<W, T> From<ControlFlow<T>> for Result<W, T> {
    fn from(x: ControlFlow<T>) -> Self {
        match x {
            ControlFlow::Continue(()) => Result::Continue,
            ControlFlow::Break(x) => Result::Break(x),
        }
    }
}
