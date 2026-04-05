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
pub struct Programs<I>(Inner<I>);

#[derive(Clone, Debug)]
enum Inner<I> {
    /// A set containing only the empty program.
    Empty,
    EmptyProgram,
    Inst(I),
    Concat(Rc<(Programs<I>, Programs<I>)>),
    Extend(Rc<Vec<Programs<I>>>),
}

impl<I> Programs<I> {
    pub fn empty() -> Self {
        Self(Inner::Empty)
    }

    pub fn empty_program() -> Self {
        Self(Inner::EmptyProgram)
    }

    pub fn inst(i: I) -> Self {
        Self(Inner::Inst(i))
    }

    pub fn concat(self, rhs: Self) -> Self {
        Self(Inner::Concat(Rc::new((self, rhs))))
    }

    pub fn extend(mut self, x: Self) -> Self {
        match &mut self.0 {
            Inner::Empty => x,
            Inner::Extend(vec) => {
                if let Some(vec) = Rc::get_mut(vec) {
                    vec.push(x);
                    self
                } else {
                    Self(Inner::Extend(Rc::new(vec![self, x])))
                }
            }
            Inner::EmptyProgram | Inner::Inst(_) | Inner::Concat(_) => {
                Self(Inner::Extend(Rc::new(vec![self, x])))
            }
        }
    }

    // Get a single program from the collection, if there is any.
    pub fn sample(&self) -> Option<Program<I>>
    where
        I: Clone,
    {
        match &self.0 {
            Inner::Empty => None,
            Inner::Inst(i) => Some(vec![i.clone()]),
            Inner::EmptyProgram => Some(vec![]),
            Inner::Extend(rc) => {
                let vec = &**rc;
                vec.iter().filter_map(Self::sample).next()
            }
            Inner::Concat(rc) => {
                let (lhs, rhs) = &**rc;
                let lhs = lhs.sample()?;
                let mut rhs = rhs.sample()?;
                Some(lhs.mutate(|l| l.append(&mut rhs)))
            }
        }
    }

    pub fn try_each<B>(
        &self,
        f: &mut (impl ?Sized + FnMut(Program<I>) -> ControlFlow<B>),
    ) -> ControlFlow<B>
    where
        I: Clone,
    {
        use ControlFlow::Continue;
        match &self.0 {
            Inner::Empty => Continue(()),
            Inner::EmptyProgram => f(vec![]),
            Inner::Inst(inst) => f(vec![inst.clone()]),
            Inner::Extend(rc) => {
                let vec = &**rc;
                for x in vec {
                    x.try_each(f)?;
                }
                Continue(())
            }
            Inner::Concat(rc) => {
                let (lhs, rhs) = &**rc;
                // p.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(Box::new(
                lhs.try_each(
                    (&mut |mut lhs: Program<I>| {
                        let len = lhs.len();
                        rhs.try_each(
                            (&mut |mut rhs| {
                                lhs.append(&mut rhs);
                                f(lhs.clone())?;
                                lhs.truncate(len);
                                Continue(())
                            })
                                as &mut dyn FnMut(Program<I>) -> ControlFlow<B>,
                        )
                    }) as &mut dyn FnMut(Program<I>) -> ControlFlow<B>,
                )
            }
        }
    }

    pub fn each(&self, mut f: impl FnMut(Program<I>))
    where
        I: Clone,
    {
        let ret = self.try_each(&mut |prog| {
            f(prog);
            ControlFlow::Continue::<()>(())
        });
        match ret {
            ControlFlow::Continue(()) => (),
            ControlFlow::Break(()) => unreachable!(),
        }
    }

    pub fn to_vec(&self) -> Vec<Program<I>>
    where
        I: Clone,
    {
        let mut vec = Vec::with_capacity(self.len());
        self.each(|prog| vec.push(prog));
        vec
    }
}

impl<I> Default for Programs<I> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<I: Clone> From<Programs<I>> for Vec<Program<I>> {
    fn from(this: Programs<I>) -> Self {
        this.to_vec()
    }
}

impl<I> Len for Programs<I> {
    fn len(&self) -> usize {
        match &self.0 {
            Inner::Empty => 0,
            Inner::EmptyProgram => 1,
            Inner::Inst(..) => 1,
            Inner::Extend(rc) => rc.iter().map(Self::len).sum(),
            Inner::Concat(rc) => rc.0.len() * rc.1.len(),
        }
    }
    fn is_empty(&self) -> bool {
        match &self.0 {
            Inner::Empty => true,
            Inner::EmptyProgram => false,
            Inner::Inst(..) => false,
            Inner::Extend(rc) => rc.iter().all(Self::is_empty),
            Inner::Concat(rc) => rc.0.is_empty() || rc.1.is_empty(),
        }
    }
}

impl<I: std::fmt::Display + Clone> std::fmt::Display for Programs<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.each(|program| {
            if program.is_empty() {
                writeln!(f, "· <empty program>").unwrap();
            }
            program.iter().enumerate().for_each(|(i, inst)| {
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

impl<I: Clone> FromIterator<I> for Programs<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut ret = Programs::empty_program();
        for i in iter {
            ret = ret.concat(Self::inst(i));
        }
        ret
    }
}

impl<I: Clone + Debug + Eq + Hash> Extend<Programs<I>> for Programs<I> {
    fn extend<It: IntoIterator<Item = Self>>(&mut self, iter: It) {
        for p in iter {
            *self = Programs::extend(std::mem::take(self), p);
        }
    }
}

// -------------- tests ---------------------

use functionality::Mutate;
#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
impl<I> Arbitrary for Programs<I>
where
    I: Arbitrary + Clone + Debug + Eq + Hash + 'static,
    I::Strategy: 'static,
{
    type Parameters = ();

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let base = prop_oneof![
            Just(Self::empty()),
            Just(Self::empty_program()),
            any::<I>().prop_map(Self::inst),
        ];
        base.prop_recursive(
            8,  // maximum depth
            40, // total size
            5,  // amount of elements in a branch
            |inner| {
                prop_oneof![
                    // Concat
                    (inner.clone(), inner.clone()).prop_map(|(a, b)| a.concat(b)),
                    // Extend
                    (inner.clone(), inner.clone()).prop_map(|(a, b)| a.mutate(|a| a.extend([b])))
                ]
            },
        )
        .boxed()
    }

    type Strategy = BoxedStrategy<Self>;
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
