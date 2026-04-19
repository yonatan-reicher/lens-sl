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
    EmptyProgram,
    Concat(Rc<(Programs<I>, I)>),
    #[allow(unused)]
    ConcatsMap(Rc<FxHashMap<I, Programs<I>>>),
    /// A set given by a vector of concatenations.
    ConcatsVec(Rc<Vec<(Programs<I>, I)>>),
    // Add an option for an 'extend', i.e., that one or this one, something like that. Or
    // maybe a linked list or who knows what.
}

impl<I> Programs<I> {
    pub fn empty() -> Self {
        Self(Inner::ConcatsVec(Rc::new(vec![])))
    }

    pub fn empty_program() -> Self {
        Self(Inner::EmptyProgram)
    }

    pub fn concat(self, inst: I) -> Self {
        Self(Inner::Concat(Rc::new((self, inst))))
    }

    pub fn extend(&mut self, x: &Programs<I>)
    where
        I: Clone + Debug + Eq + Hash,
    {
        match &x.0 {
            Inner::EmptyProgram => match &self.0 {
                Inner::EmptyProgram => (),
                Inner::ConcatsVec(v) if v.is_empty() => *self = Self(Inner::EmptyProgram),
                Inner::ConcatsMap(m) if m.is_empty() => *self = Self(Inner::EmptyProgram),
                _ => unreachable!(""),
            },
            Inner::Concat(rc) => self.extend_concat(&rc.0, &rc.1),
            Inner::ConcatsVec(vec) => {
                self.reserve(vec.len());
                for (p, i) in vec.iter() {
                    self.extend_concat(p, i);
                }
            }
            Inner::ConcatsMap(map) => {
                self.reserve(map.len());
                for (i, p) in map.iter() {
                    self.extend_concat(p, i);
                }
            }
        }
    }

    fn reserve(&mut self, n: usize)
    where
        I: Clone + Eq + Hash,
    {
        match &mut self.0 {
            Inner::EmptyProgram => (),
            Inner::Concat(rc) => {
                let mut vec = Vec::with_capacity(n + 1);
                vec.push((rc.0.clone(), rc.1.clone()));
                self.0 = Inner::ConcatsVec(Rc::new(vec));
            }
            Inner::ConcatsMap(map) => Rc::make_mut(map).reserve(n),
            Inner::ConcatsVec(vec) => Rc::make_mut(vec).reserve(n),
        }
    }

    fn extend_concat(&mut self, progs: &Self, inst: &I)
    where
        I: Clone + Debug + Eq + Hash,
    {
        use Inner::{Concat, ConcatsMap, ConcatsVec, EmptyProgram};
        match &mut self.0 {
            EmptyProgram => {
                unreachable!(
                    "The set of empty programs should never be extended with another set, because extending a set should only happen after expanding a program! and when all programs are expanded, they all should have at least one instruction. Was extended with Concat({progs:?}, {inst:?})"
                );
            }
            Concat(..) => {
                self.reserve(1);
                self.extend_concat(progs, inst)
            }
            ConcatsMap(map) => {
                if let Some(y) = Rc::make_mut(map).get_mut(inst) {
                    // Now we can recurse
                    y.extend(progs);
                } else {
                    // This is so much faster than the other branch! Why not always do this? This
                    // has a memory cost.
                    Rc::make_mut(map).insert(inst.clone(), progs.clone());
                }
            }
            ConcatsVec(vec) => Rc::make_mut(vec).push((progs.clone(), inst.clone())),
        }
    }

    pub fn contains(&self, i: &[I]) -> bool
    where
        I: Eq,
    {
        match &self.0 {
            Inner::EmptyProgram => i == &[],
            Inner::Concat(rc) => {
                let (start, last) = rc.as_ref();
                i.last() == Some(last) && start.contains(&i[..i.len() - 1])
            }
            Inner::ConcatsMap(_) => todo!(),
            Inner::ConcatsVec(items) => items
                .iter()
                .any(|(start, last)| i.last() == Some(last) && start.contains(&i[..i.len() - 1])),
        }
    }

    // Get a single program from the collection, if there is any.
    pub fn sample(&self) -> Option<Program<I>>
    where
        I: Clone,
    {
        match &self.0 {
            Inner::EmptyProgram => Some(vec![]),
            Inner::Concat(rc) => {
                let (p, inst) = &**rc;
                let mut prog = p.sample()?;
                prog.push(inst.clone());
                Some(prog)
            }
            Inner::ConcatsMap(map) => map.iter().next().and_then(|(i, p)| {
                let mut prog = p.sample()?;
                prog.push(i.clone());
                Some(prog)
            }),
            Inner::ConcatsVec(vec) => vec.first().and_then(|(p, i)| {
                let mut prog = p.sample()?;
                prog.push(i.clone());
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
            Inner::Concat(rc) => {
                let (p, i) = &**rc;
                p.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(Box::new(
                    |mut prog| {
                        prog.push(i.clone());
                        f(prog)
                    },
                ))
            }
            Inner::ConcatsMap(map) => {
                for (y, x) in map.iter() {
                    x.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(Box::new(
                        |mut prog: Program<I>| {
                            prog.push(y.clone());
                            f(prog)
                        },
                    ))?;
                }
                Continue(())
            }
            Inner::ConcatsVec(vec) => {
                for (x, y) in vec.iter() {
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
            Inner::Concat(rc) => rc.0.len(),
            Inner::ConcatsMap(map) => map.values().map(|p| p.len()).sum(),
            Inner::ConcatsVec(vec) => vec.iter().map(|(p, _)| p.len()).sum(),
        }
    }
    fn is_empty(&self) -> bool {
        match &self.0 {
            Inner::EmptyProgram => false,
            Inner::Concat(rc) => rc.0.is_empty(),
            Inner::ConcatsMap(map) => map.values().all(|p| p.is_empty()),
            Inner::ConcatsVec(vec) => vec.iter().all(|(p, _)| p.is_empty()),
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

impl<I: Clone> FromIterator<I> for Programs<I> {
    fn from_iter<T: IntoIterator<Item = I>>(iter: T) -> Self {
        let mut ret = Programs::empty_program();
        for i in iter {
            ret = ret.concat(i);
        }
        ret
    }
}

impl<I: Clone + Debug + Eq + Hash> Extend<Programs<I>> for Programs<I> {
    fn extend<It: IntoIterator<Item = Self>>(&mut self, iter: It) {
        for p in iter {
            self.extend(&p);
        }
    }
}

// -------------- tests ---------------------

#[cfg(test)]
use proptest::prelude::*;
use rustc_hash::FxHashMap;

#[cfg(test)]
impl<I> Arbitrary for Programs<I>
where
    I: Arbitrary + Clone + Debug + Eq + Hash + 'static,
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
                        (inner.clone(), any::<I>()).prop_map(|(p, i)| p.concat(i)),
                        // Concats
                        prop::collection::vec((inner.clone(), any::<I>()), 1..10).prop_map(|vec| {
                            let concats = vec.into_iter().collect();
                            Self(Inner::ConcatsVec(Rc::new(concats)))
                        }),
                        prop::collection::vec((inner.clone(), any::<I>()), 1..10).prop_map(|vec| {
                            let concats = vec.into_iter().map(|(p, i)| (i, p)).collect();
                            Self(Inner::ConcatsMap(Rc::new(concats)))
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
