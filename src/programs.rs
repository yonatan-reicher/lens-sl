//! This module defines the `Programs` type. This type is an efficient representation of programs
//! that allows fast concatenation of another instruction at the end and saves memory for us.

use crate::len::Len;
use std::borrow::Borrow;
use std::fmt::Debug;
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
    EmptyProgram,
    Concat(Concat<I>),
    /// A set given by a vector of concatenations.
    /// TODO: We could keep these sorted for having a binary search, of use a hash-map. For now
    /// they are just stored in an arbitrary order!
    Concats(Vec<Rc<Concat<I>>>),
}

/// An instruction concatenated to the end of a program set.
#[derive(Clone, Debug)]
struct Concat<I>(Rc<Programs<I>>, I);

impl<I> Programs<I> {
    pub const fn empty() -> Self {
        Self(Inner::Concats(vec![]))
    }

    pub const fn empty_program() -> Self {
        Self(Inner::EmptyProgram)
    }

    pub fn concat(self: Rc<Self>, inst: I) -> Self {
        Self(Inner::Concat(Concat(self, inst)))
    }

    pub fn extend(&mut self, x: &Programs<I>)
    where
        I: Clone + Debug + Eq,
    {
        match &x.0 {
            Inner::EmptyProgram => match &self.0 {
                Inner::EmptyProgram => (),
                Inner::Concats(v) if v.is_empty() => {
                    *self = Self(Inner::EmptyProgram);
                }
                _ => unreachable!(""),
            },
            Inner::Concat(concat) => self.extend_concat(concat),
            Inner::Concats(concats) => {
                for c in concats {
                    self.extend_concat(c);
                }
            }
        }
    }

    fn extend_concat<C>(&mut self, x: &C)
    where
        I: Clone + Debug + Eq,
        // C should accept both `Concat<I>` and `Rc<Concat<I>>`
        C: Into<Rc<Concat<I>>> + Borrow<Concat<I>> + Clone + Debug,
    {
        use Inner::{Concat, Concats, EmptyProgram};
        match &mut self.0 {
            EmptyProgram => {
                unreachable!(
                    "The set of empty programs should never be extended with another set, because extending a set should only happen after expanding a program! and when all programs are expanded, they all should have at least one instruction. Was extended with the following set: {x:?}"
                );
            }
            Concat(c) => {
                self.0 = Concats(vec![Rc::new(c.clone())]);
                self.extend_concat(x)
            }
            Concats(vec) => {
                if let Some(y) = vec.iter_mut().find(|y| y.1 == x.borrow().1) {
                    let y = Rc::make_mut(y);
                    let y_children = Rc::make_mut(&mut y.0);
                    // Now we can recurse
                    y_children.extend(&x.borrow().0);
                } else {
                    // This is so much faster than the other branch! Why not always do this? This
                    // has a memory cost.
                    vec.push(x.clone().into());
                }
            }
        }
    }

    // Get a single program from the collection, if there is any.
    pub fn sample(&self) -> Option<Program<I>>
    where
        I: Clone,
    {
        match &self.0 {
            Inner::EmptyProgram => Some(vec![]),
            Inner::Concat(c) => {
                let mut prog = c.0.sample()?;
                prog.push(c.1.clone());
                Some(prog)
            }
            Inner::Concats(vec) => vec.first().and_then(|c| {
                let mut prog = c.0.sample()?;
                prog.push(c.1.clone());
                Some(prog)
            }),
        }
    }

    pub fn try_each<B, F>(&self, mut f: F) -> ControlFlow<B>
    where
        I: Clone,
        F: FnMut(Program<I>) -> ControlFlow<B>,
    {
        use ControlFlow::Continue;
        match &self.0 {
            Inner::EmptyProgram => f(vec![]),
            Inner::Concat(c) => {
                c.0.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(Box::new(
                    |mut prog| {
                        prog.push(c.1.clone());
                        f(prog)
                    },
                ))
            }
            Inner::Concats(vec) => {
                for Concat(x, y) in vec.iter().map(|x| x.as_ref()) {
                    x.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(Box::new(
                        |mut prog: Program<I>| {
                            prog.push(y.clone());
                            f(prog)
                        },
                    ))?;
                }
                Continue(())
            }
        }
    }

    pub fn each(&self, mut f: impl FnMut(Program<I>))
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
            Inner::EmptyProgram => 1,
            Inner::Concat(c) => c.0.len(),
            Inner::Concats(vec) => vec.iter().map(|c| c.0.len()).sum(),
        }
    }
}

impl<I: Clone + Debug + Eq> crate::graph::Programs for Rc<Programs<I>> {
    type Program = Program<I>;
    fn extend(&mut self, other: &Self) {
        Rc::make_mut(self).extend(other)
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

impl<I: Clone> FromIterator<I> for Programs<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut ret = Programs::empty_program();
        for i in iter {
            ret = Rc::new(ret).concat(i);
        }
        ret
    }
}

// -------------- tests ---------------------

#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
impl<I> Arbitrary for Programs<I>
where
    I: Arbitrary + Clone + Debug + 'static,
    I::Strategy: 'static,
{
    type Parameters = ();

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        let empty_program = Just(Self::empty_program());
        empty_program
            .prop_recursive(
                8,  // maximum depth
                40, // total size
                5,  // amount of elements in a branch
                |inner| {
                    prop_oneof![
                        // Concat
                        (inner.clone(), any::<I>()).prop_map(|(p, i)| Rc::new(p).concat(i)),
                        // Concats
                        prop::collection::vec((inner.clone(), any::<I>()), 1..10).prop_map(|vec| {
                            let concats = vec
                                .into_iter()
                                .map(|(p, i)| Rc::new(Concat(Rc::new(p), i)))
                                .collect();
                            Self(Inner::Concats(concats))
                        }),
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
