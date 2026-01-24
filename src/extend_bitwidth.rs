use crate::all_permutations::Iter as PermutationIter;
use crate::isa::{ArgType, Inst};
use crate::word::prelude::*;
use rustc_hash::FxHashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Extender<WBig: Word, WSmall: Word>(FxHashMap<WSmall::Unsigned, Vec<WBig::Unsigned>>);

impl<WBig: Word, WSmall: Word> Extender<WBig, WSmall> {
    pub fn empty() -> Self {
        Self(FxHashMap::default())
    }

    pub fn new(values: impl Iterator<Item = WBig::Unsigned>) -> Self {
        let mut ret = Self::empty();
        for value in values {
            let small_value = WSmall::Unsigned::from_(value);
            ret.0
                .entry(small_value)
                .or_insert_with(Vec::new)
                .push(value);
        }
        ret
    }

    fn extend_value(&self, value: WSmall::Unsigned) -> &[WBig::Unsigned] {
        self.0.get(&value).map_or(&[], |v| v.as_slice())
    }

    fn extend_arg(
        &self,
        arg: WSmall::Unsigned,
        arg_type: ArgType,
    ) -> impl Iterator<Item = WBig::Unsigned> + Clone {
        // Implement an iterator that yields either a slice or a single value.
        #[derive(Clone, Debug)]
        enum Iter<'a, WBig: Word> {
            Slice(&'a [WBig::Unsigned]),
            Single(WBig::Unsigned),
        }
        impl<'a, W: Word> Iterator for Iter<'a, W> {
            type Item = W::Unsigned;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Iter::Slice([]) => None,
                    Iter::Slice([h, t @ ..]) => {
                        *self = Iter::Slice(t);
                        Some(*h)
                    }
                    Iter::Single(x) => {
                        let x = *x;
                        *self = Iter::Slice(&[]);
                        Some(x)
                    }
                }
            }
        }

        match arg_type {
            ArgType::Imm => Iter::Slice(self.extend_value(arg)),
            ArgType::Reg | ArgType::Unused => Iter::<WBig>::Single(arg.as_()),
        }
    }

    fn extend_inst(&self, inst: Inst<WSmall>) -> impl Iterator<Item = Inst<WBig>> + Clone {
        // If only we had do-notation for iterators...
        self.extend_arg(inst.args[0], inst.op_code.arg_types()[0])
            .into_iter()
            .flat_map(move |arg0| {
                self.extend_arg(inst.args[1], inst.op_code.arg_types()[1])
                    .into_iter()
                    .flat_map(move |arg1| {
                        self.extend_arg(inst.args[2], inst.op_code.arg_types()[2])
                            .into_iter()
                            .map(move |arg2| Inst {
                                op_code: inst.op_code,
                                cond_code: inst.cond_code,
                                args: [arg0, arg1, arg2],
                            })
                    })
            })
    }

    fn extend_program_for_each<F>(&self, program: &[Inst<WSmall>], mut f: F)
    where
        F: FnMut(&[Inst<WBig>]),
    {
        let mut ret = vec![];
        let iters: Vec<_> = program.iter().map(|inst| self.extend_inst(*inst)).collect();
        let mut iter = PermutationIter::new(iters.as_slice());
        while let Some(perm) = iter.next_slice() {
            ret.clear();
            ret.extend_from_slice(perm);
            f(&ret);
        }
    }
}
