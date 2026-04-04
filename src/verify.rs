use crate::arm::{Inst, State, extend_program_for_each};
use crate::oracle::Oracle;
use crate::reduce_bit_width::Reducer;
use crate::word::{HasBitWord, Word};
use std::ops::ControlFlow::{self, Continue};

pub fn verify<T, WBig, WSmall>(
    program: &[Inst<WSmall>],
    reducer: &Reducer<WBig, WSmall>,
    oracle_reduced: &mut impl Oracle<[Inst<WSmall>], State<WSmall>>,
    oracle: &mut impl Oracle<[Inst<WBig>], State<WBig>>,
    mut on_found_counter_example: impl FnMut(State<WSmall>, State<WSmall>) -> ControlFlow<T>,
    mut on_found_program: impl FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
) -> ControlFlow<T>
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
                    Err(_) => Continue(()),
                }
            })
        }
        Err((inp, out)) => on_found_counter_example(inp, out),
    }
}
