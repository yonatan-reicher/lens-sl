//! This module defines the `Programs` type. This type is an efficient representation of programs
//! that allows fast concatenation of another instruction at the end and saves memory for us.

use crate::len::Len;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::ControlFlow;
use std::rc::Rc;

pub type Program<I> = Vec<I>;

/// A set of programs, stored in a leaf. Edge case: if the set is empty, it represents a set
/// containing only the empty program (not the empty set).
#[derive(Clone, Debug)]
pub struct Programs<I> {
    // Invariants:
    // - `len` is the amount of programs in the set.
    // - `len` is 0 if and only if `inner` is `Empty`. That means that `Concat` and `Extend` need
    //   to handle cases with `Empty` children.
    // - All programs in the set have the same depth.
    inner: Inner<I>,
    /// Amount of programs in the set.
    len: usize,
    /// The length of all programs in the set. If the set is empty, this is 0.
    depth: u8,
}

#[derive(Clone, Debug)]
enum Inner<I> {
    /// An empty set.
    Empty,
    /// A set containing only the empty program.
    EmptyProgram,
    /// A single program which is just a single instruction.
    Inst(I),
    /// A single program.
    Program(Rc<[I]>),
    /// The set of programs obtained by concatenating each program with the instruction.
    Concat(Rc<(Programs<I>, I)>),
    /// The set of programs obtained by concatenating each program with each instruction (Cartesian
    /// product like).
    ConcatMany(Rc<(Programs<I>, Vec<I>)>),
    /// The set of programs obtained by the union of all sets.
    Extend(Rc<Vec<Programs<I>>>),
}

impl<I> From<Inner<I>> for Programs<I> {
    fn from(inner: Inner<I>) -> Self {
        use Inner::{Concat, ConcatMany, Empty, EmptyProgram, Extend, Inst, Program};
        let (length, depth) = match &inner {
            Empty => (0, 0),
            EmptyProgram => (1, 0),
            Inst(_) => (1, 1),
            Program(p) => (1, p.len() as u8),
            Concat(rc) => {
                let (progs, _) = &**rc;
                debug_assert_ne!(
                    progs.len, 0,
                    "`Concat` should never have an empty left-hand side"
                );
                (progs.len(), progs.depth + 1)
            }
            ConcatMany(rc) => {
                let (lhs, insts) = &**rc;
                debug_assert_ne!(
                    lhs.len, 0,
                    "`ConcatMany` should never have an empty left-hand side"
                );
                debug_assert_ne!(
                    insts.len(),
                    0,
                    "`ConcatMany` should never have an empty right-hand side"
                );
                (lhs.len() * insts.len(), lhs.depth + 1)
            }
            Extend(progs) => {
                debug_assert!(!progs.is_empty(), "`Extend` should never be empty");
                debug_assert!(
                    progs.iter().all(|p| p.depth == progs[0].depth),
                    "`Extend` should never contain programs of different depths"
                );
                (progs.iter().map(|p| p.len).sum(), progs[0].depth)
            }
        };
        Self {
            inner,
            len: length,
            depth,
        }
    }
}

#[rustfmt::skip] impl<I> Len for Programs<I> {
    fn len(&self) -> usize { self.len }
    fn is_empty(&self) -> bool { self.len == 0 }
}

impl<I> Programs<I> {
    pub fn empty() -> Self {
        Self::from(Inner::Empty)
    }

    pub fn empty_program() -> Self {
        Self::from(Inner::EmptyProgram)
    }

    pub fn inst(i: I) -> Self {
        Self::from(Inner::Inst(i))
    }

    pub fn program(p: Vec<I>) -> Self {
        if p.is_empty() {
            Self::empty_program()
        } else if p.len() == 1 {
            Self::inst(p.into_iter().next().unwrap())
        } else {
            Self::from(Inner::Program(Rc::from(p)))
        }
    }

    pub fn concat(self, inst: I) -> Self {
        if self.is_empty() {
            Self::empty()
        } else {
            Self::from(Inner::Concat(Rc::new((self, inst))))
        }
    }

    pub fn concat_many(self, insts: Vec<I>) -> Self {
        if self.is_empty() || insts.is_empty() {
            Self::empty()
        } else {
            Self::from(Inner::ConcatMany(Rc::new((self, insts))))
        }
    }

    pub fn extend(mut self, x: Self) -> Self {
        if self.is_empty() {
            return x;
        }
        if x.is_empty() {
            return self;
        }
        debug_assert_eq!(
            self.depth, x.depth,
            "Can only extend sets of programs with the same depth"
        );
        if let Inner::Extend(rc) = &mut self.inner
            && let Some(progs) = Rc::get_mut(rc)
        {
            self.len += x.len;
            progs.push(x);
            return self;
        }
        Self::from(Inner::Extend(Rc::new(vec![self, x])))
    }

    pub fn extend_many(mut self, xs: impl IntoIterator<Item = Self>) -> Self {
        // We want to ignore empty sets! Not ignoring would be a bug.
        let xs = xs.into_iter().filter(|x| !x.is_empty());
        if self.is_empty() {
            let vec = xs.collect_vec();
            if vec.is_empty() {
                Self::empty()
            } else {
                Self::from(Inner::Extend(Rc::new(vec)))
            }
        } else if let Inner::Extend(rc) = &mut self.inner
            && let Some(progs) = Rc::get_mut(rc)
        {
            // Add the programs and update the length at the same time.
            progs.extend(xs.inspect(|x| {
                self.len += x.len;
            }));
            self
        } else {
            xs.fold(self, |acc, x| acc.extend(x))
        }
    }

    #[allow(unused)]
    // Get a single program from the collection, if there is any.
    pub fn sample<T>(&self, f: impl FnOnce(&mut dyn Iterator<Item = I>) -> T) -> Option<T>
    where
        I: Clone,
    {
        use std::iter::{empty, once};
        match &self.inner {
            Inner::Empty => None,
            Inner::EmptyProgram => Some(f(&mut empty())),
            Inner::Inst(i) => Some(f(&mut once(i.clone()))),
            Inner::Program(p) => Some(f(&mut p.iter().cloned())),
            Inner::Concat(rc) => {
                let (progs, inst) = &**rc;
                let g = |iter: &mut dyn Iterator<Item = I>| {
                    let mut iter = iter.chain(once(inst.clone()));
                    f(&mut iter)
                };
                progs.sample(g)
            }
            Inner::ConcatMany(rc) => {
                let (progs, insts) = &**rc;
                let inst = insts.first().unwrap();
                progs.sample(|iter| {
                    let mut iter = iter.chain(once(inst.clone()));
                    f(&mut iter)
                })
            }
            Inner::Extend(vec) => vec.first().unwrap().sample(f),
        }
    }

    /// Calls a callback on each program in the set, but gives the programs in reversed order (the
    /// last instruction is given first).
    pub fn try_each_reversed<B>(&self, mut f: impl FnMut(&[I]) -> ControlFlow<B>) -> ControlFlow<B>
    where
        I: Clone,
    {
        // Edge case!
        if self.is_empty() {
            return Continue(());
        }
        use ControlFlow::Continue;
        let mut program = Vec::with_capacity(self.depth as usize);
        return visit(self, &mut program, &mut f);

        fn visit<I: Clone, B>(
            this: &Programs<I>,
            program: &mut Vec<I>,
            f: &mut impl FnMut(&[I]) -> ControlFlow<B>,
        ) -> ControlFlow<B> {
            use Inner::{Concat, ConcatMany, Empty, EmptyProgram, Extend, Inst, Program};
            match &this.inner {
                Empty => unreachable!(),
                EmptyProgram => f(program)?,
                Inst(i) => {
                    program.push(i.clone());
                    f(program)?;
                    program.pop();
                }
                Program(p) => {
                    program.extend(p.iter().cloned().rev());
                    f(program)?;
                    program.truncate(program.len() - p.len());
                }
                Concat(rc) => {
                    let (progs, inst) = &**rc;
                    program.push(inst.clone());
                    visit(progs, program, f)?;
                    program.pop();
                }
                ConcatMany(rc) => {
                    let (progs, insts) = &**rc;
                    for inst in insts {
                        program.push(inst.clone());
                        visit(progs, program, f)?;
                        program.pop();
                    }
                }
                Extend(vec) => {
                    for p in vec.iter() {
                        visit(p, program, f)?;
                    }
                }
            }
            Continue(())
        }
    }

    /// Calls a callback on each program in the set.
    pub fn try_each<B>(
        &self,
        mut f: impl FnMut(std::iter::Rev<std::iter::Cloned<std::slice::Iter<I>>>) -> ControlFlow<B>,
    ) -> ControlFlow<B>
    where
        I: Clone,
    {
        self.try_each_reversed(|prog| f(prog.iter().cloned().rev()))
    }

    pub fn each(&self, mut f: impl FnMut(std::iter::Rev<std::iter::Cloned<std::slice::Iter<I>>>))
    where
        I: Clone,
    {
        let ret = self.try_each(|prog| {
            f(prog);
            ControlFlow::Continue::<()>(())
        });
        match ret {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(()) => unreachable!(),
        }
    }

    pub fn contains<Prog>(&self, program: Prog) -> bool
    where
        I: Clone + Eq,
        Prog: Clone + DoubleEndedIterator + ExactSizeIterator<Item = I>,
    {
        // First check the depth, then call another function that does the recursion without
        // checking the depth.
        return self.depth as usize == program.len() && visit(self, program);

        fn visit<I, Prog>(this: &Programs<I>, mut program: Prog) -> bool
        where
            I: Clone + Eq,
            Prog: Clone + DoubleEndedIterator + ExactSizeIterator<Item = I>,
        {
            use Inner::{Concat, ConcatMany, Empty, EmptyProgram, Extend, Inst, Program};
            match &this.inner {
                Empty => false,
                EmptyProgram => program.len() == 0,
                Inst(i) => program.next().as_ref() == Some(i),
                Program(other_program) => program.eq(other_program.iter().cloned()),
                Concat(rc) => {
                    let (progs, inst) = &**rc;
                    program.next_back().as_ref() == Some(inst) && visit(progs, program)
                }
                ConcatMany(rc) => {
                    let (progs, insts) = &**rc;
                    let last = program.next_back().unwrap();
                    insts.iter().contains(&last) && visit(progs, program)
                }
                Extend(progs) => progs.iter().any(|p| visit(p, program.clone())),
            }
        }
    }
}

impl<I> Default for Programs<I> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<I: std::fmt::Display + Clone> std::fmt::Display for Programs<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.each(|program| {
            if program.len() == 0 {
                writeln!(f, "· <empty program>").unwrap();
            }
            program.enumerate().for_each(|(i, inst)| {
                let prefix = if i == 0 { '·' } else { ' ' };
                writeln!(f, "{prefix} {inst}").unwrap();
            })
        });
        Ok(())
    }
}

impl<I: Clone> From<I> for Programs<I> {
    fn from(i: I) -> Self {
        Self::inst(i)
    }
}

impl<I: Clone + Debug + Eq + Hash> Extend<Programs<I>> for Programs<I> {
    fn extend<It: IntoIterator<Item = Self>>(&mut self, iter: It) {
        *self = std::mem::take(self).extend_many(iter);
    }
}

// -------------- tests ---------------------

#[cfg(test)]
use functionality::prelude::*;
use itertools::Itertools;
#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
#[derive(Clone, Debug, Default)]
pub struct ProgramsParams {
    depth: Option<usize>,
}

#[cfg(test)]
impl<I> Arbitrary for Programs<I>
where
    I: Arbitrary + Clone + Debug + Eq + Hash + 'static,
    I::Strategy: 'static,
{
    type Parameters = ProgramsParams;

    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(ProgramsParams { depth }: ProgramsParams) -> Self::Strategy {
        use prop::collection::{size_range, vec};
        if let Some(depth) = depth {
            match depth {
                0 => Just(Self::empty_program()).boxed(),
                1 => any::<I>().prop_map(Self::inst).boxed(),
                _ => {
                    prop_oneof![
                        // Program
                        vec(any::<I>(), size_range(depth - 1..=depth - 1)).prop_map(Self::program),
                        // Concat
                        (any::<Self>(), any::<I>()).prop_map(|(progs, inst)| progs.concat(inst)),
                        // ConcatMany
                        (any::<Self>(), vec(any::<I>(), size_range(1..=depth - 1)))
                            .prop_map(|(progs, insts)| progs.concat_many(insts)),
                        // Extend
                        vec(any::<Self>(), size_range(2..=depth - 1)).prop_map(|progs| progs
                            .into_iter()
                            .fold(Self::empty(), |acc, p| acc.extend(p)))
                    ]
                    .boxed()
                }
            }
        } else {
            (0..=5usize)
                .prop_flat_map(|depth| Self::arbitrary_with(ProgramsParams { depth: Some(depth) }))
                .boxed()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::property_test;

    #[property_test]
    fn len_is_amount_of_programs_in_each(programs: Programs<u8>) {
        let mut count = 0;
        programs.each(|_| count += 1);
        prop_assert_eq!(count, programs.len());
    }
}
