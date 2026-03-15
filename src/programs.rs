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
    Concat(Rc<Programs<I>>, I),
    #[allow(unused)]
    ConcatsMap(FxHashMap<I, Rc<Programs<I>>>),
    /// A set given by a vector of concatenations.
    ConcatsVec(Vec<(Rc<Programs<I>>, I)>),
}

impl<I> Programs<I> {
    pub fn empty() -> Self {
        Self(Inner::ConcatsVec(vec![]))
    }

    pub fn empty_program() -> Self {
        Self(Inner::EmptyProgram)
    }

    pub fn concat(self: Rc<Self>, inst: I) -> Self {
        Self(Inner::Concat(self, inst))
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
            Inner::Concat(p, i) => self.extend_concat(p, i),
            Inner::ConcatsVec(vec) => {
                self.reserve(vec.len());
                for (p, i) in vec {
                    self.extend_concat(p, i);
                }
            }
            Inner::ConcatsMap(map) => {
                self.reserve(map.len());
                for (i, p) in map {
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
            Inner::Concat(p, i) => {
                let mut vec = Vec::with_capacity(n + 1);
                vec.push((p.clone(), i.clone()));
                self.0 = Inner::ConcatsVec(vec);
            }
            Inner::ConcatsMap(map) => map.reserve(n),
            Inner::ConcatsVec(vec) => vec.reserve(n),
        }
    }

    fn extend_concat(&mut self, progs: &Rc<Self>, inst: &I)
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
                if let Some(y) = map.get_mut(inst) {
                    let y = Rc::make_mut(y);
                    // Now we can recurse
                    y.extend(progs);
                } else {
                    // This is so much faster than the other branch! Why not always do this? This
                    // has a memory cost.
                    map.insert(inst.clone(), progs.clone());
                }
            }
            ConcatsVec(vec) => vec.push((progs.clone(), inst.clone())),
        }
    }

    // Get a single program from the collection, if there is any.
    pub fn sample(&self) -> Option<Program<I>>
    where
        I: Clone,
    {
        match &self.0 {
            Inner::EmptyProgram => Some(vec![]),
            Inner::Concat(p, inst) => {
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
            Inner::Concat(p, i) => p.try_each::<B, Box<dyn FnMut(Program<I>) -> ControlFlow<B>>>(
                Box::new(|mut prog| {
                    prog.push(i.clone());
                    f(prog)
                }),
            ),
            Inner::ConcatsMap(map) => {
                for (y, x) in map {
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
                for (x, y) in vec {
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
            Inner::Concat(p, _) => p.len(),
            Inner::ConcatsMap(map) => map.values().map(|p| p.len()).sum(),
            Inner::ConcatsVec(vec) => vec.iter().map(|(p, _)| p.len()).sum(),
        }
    }
    fn is_empty(&self) -> bool {
        match &self.0 {
            Inner::EmptyProgram => false,
            Inner::Concat(p, _) => p.is_empty(),
            Inner::ConcatsMap(map) => map.values().all(|p| p.is_empty()),
            Inner::ConcatsVec(vec) => vec.iter().all(|(p, _)| p.is_empty()),
        }
    }
}

impl<I: Clone + Debug + Eq + Hash> crate::graph::Programs for Rc<Programs<I>> {
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
                        (inner.clone(), any::<I>()).prop_map(|(p, i)| Rc::new(p).concat(i)),
                        // Concats
                        prop::collection::vec((inner.clone(), any::<I>()), 1..10).prop_map(|vec| {
                            let concats = vec.into_iter().map(|(p, i)| (Rc::new(p), i)).collect();
                            Self(Inner::ConcatsVec(concats))
                        }),
                        prop::collection::vec((inner.clone(), any::<I>()), 1..10).prop_map(|vec| {
                            let concats = vec.into_iter().map(|(p, i)| (i, Rc::new(p))).collect();
                            Self(Inner::ConcatsMap(concats))
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
