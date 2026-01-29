use crate::all_permutations::Iter as PermutationIter;
use crate::isa::{ArgType, Inst};
use crate::iter_slice_or_single::Iter;
use crate::word::prelude::*;
use rustc_hash::FxHashMap;
use std::ops::ControlFlow;

/// What is this immediate used for? This decides what it's equivalent should be.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ImmediateInfo {
    /// True if this immediate is used as a shift argument.
    pub is_shift: bool,
}

pub fn reduce_value<WBig: Word, WSmall: Word>(
    arg: WBig::Unsigned,
    info: &ImmediateInfo,
) -> WSmall::Unsigned {
    if info.is_shift {
        let bit_width = WBig::Unsigned::from_(WBig::Unsigned::BITS as u64);
        let reduced_bit_width = WSmall::Unsigned::from_(WSmall::Unsigned::BITS as u64);
        return if arg == bit_width {
            reduced_bit_width
        } else if arg == bit_width - 1.as_() {
            reduced_bit_width - 1.as_()
        } else if bit_width / 2.as_() <= arg && arg < bit_width - 1.as_() {
            reduced_bit_width / 2.as_()
        } else
        /* 0 <= arg && arg < bit_width / 2 */
        {
            1.as_()
        };
    }
    arg.as_()
}

/// This structure remembers how constants were reduced, so that we can extend them back.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Reducer<WBig: Word, WSmall: Word>(FxHashMap<WSmall::Unsigned, Vec<WBig::Unsigned>>);

impl<WBig: Word, WSmall: Word> Reducer<WBig, WSmall> {
    pub fn reduce_value(
        &mut self,
        value: WBig::Unsigned,
        info: &ImmediateInfo,
    ) -> WSmall::Unsigned {
        let reduced = reduce_value::<WBig, WSmall>(value, info);
        self.0
            .entry(reduced)
            .or_insert_with(|| vec![value])
            .push(value);
        reduced
    }

    fn reduce_arg(
        &mut self,
        arg: WBig::Unsigned,
        arg_type: ArgType,
        info: &ImmediateInfo,
    ) -> WSmall::Unsigned {
        match arg_type {
            ArgType::Imm => self.reduce_value(arg, info),
            ArgType::Reg | ArgType::Unused => arg.as_(),
        }
    }

    pub fn reduce_inst(&mut self, inst: &Inst<WBig>) -> Inst<WSmall> {
        let args = inst.args;
        let arg_types = inst.op_code.arg_types();
        let info = ImmediateInfo {
            // is_shift: inst.op_code.is_shift_instruction(),
            is_shift: true,
        };
        let ret_args = [
            self.reduce_arg(args[0], arg_types[0], &info),
            self.reduce_arg(args[1], arg_types[1], &info),
            self.reduce_arg(args[2], arg_types[2], &info),
        ];
        Inst {
            op_code: inst.op_code,
            cond_code: inst.cond_code,
            args: ret_args,
        }
    }

    pub fn reduce_program(&mut self, program: &[Inst<WBig>]) -> Vec<Inst<WSmall>> {
        program.iter().map(|inst| self.reduce_inst(inst)).collect()
    }

    fn extend_value(&self, value: WSmall::Unsigned) -> Iter<'_, WBig::Unsigned> {
        self.0
            .get(&value)
            .map_or(Iter::Single(value.as_()), |v| Iter::Slice(v.as_slice()))
    }

    fn extend_arg(
        &self,
        arg: WSmall::Unsigned,
        arg_type: ArgType,
    ) -> impl Iterator<Item = WBig::Unsigned> + Clone {
        match arg_type {
            ArgType::Imm => self.extend_value(arg),
            ArgType::Reg | ArgType::Unused => Iter::Single(arg.as_()),
        }
    }

    pub fn extend_inst(&self, inst: Inst<WSmall>) -> impl Iterator<Item = Inst<WBig>> + Clone {
        let args = inst.args;
        let arg_types = inst.op_code.arg_types();
        self.extend_arg(args[0], arg_types[0])
            .flat_map(move |arg0| {
                self.extend_arg(args[1], arg_types[1])
                    .flat_map(move |arg1| {
                        self.extend_arg(args[2], arg_types[2])
                            .map(move |arg2| Inst {
                                op_code: inst.op_code,
                                cond_code: inst.cond_code,
                                args: [arg0, arg1, arg2],
                            })
                    })
            })
    }

    pub fn extend_program_for_each<F, T>(
        &self,
        program: &[Inst<WSmall>],
        mut f: F,
    ) -> ControlFlow<T>
    where
        F: FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
    {
        let mut ret = vec![];
        let iters: Vec<_> = program.iter().map(|inst| self.extend_inst(*inst)).collect();
        let mut iter = PermutationIter::new(iters.as_slice());
        while let Some(perm) = iter.next_slice() {
            ret.clear();
            ret.extend_from_slice(perm);
            f(&ret)?;
        }
        ControlFlow::Continue(())
    }
}
