//! This module defines the `Programs` type. This type is an efficient representation of programs
//! that allows fast concatenation of another instruction at the end and saves memory for us.

use std::rc::Rc;

pub type Program<I> = Vec<I>;

/// See module documentation.
#[derive(Debug)]
pub enum Programs<I> {
    /// A single program.
    Program(Program<I>),
    /// A list of programs.
    List(Vec<Programs<I>>),
    /// Appends the instruction to the end of each program in the inner `Programs`.
    Concat(Rc<Programs<I>>, I),
}

impl<I> Programs<I> {
    pub const fn new() -> Self {
        Self::List(Vec::new())
    }

    pub const fn program(program: Program<I>) -> Self {
        Self::Program(program)
    }

    pub const fn concat(self: Rc<Self>, inst: I) -> Self {
        Self::Concat(self, inst)
    }

    /// The number of programs stored.
    pub fn len(&self) -> usize {
        match self {
            Self::Program(_) => 1,
            Self::List(vec) => vec.len(),
            Self::Concat(inner, _) => inner.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn extend(&mut self, other: Self) {
        *self = std::mem::take(self).extend_take(other);
    }

    fn extend_take(self, other: Self) -> Self {
        use Programs::{Concat, List, Program};
        match (self, other) {
            (List(mut vec1), List(vec2)) => {
                vec1.extend(vec2);
                List(vec1)
            }
            (List(mut vec), x) | (x, List(mut vec)) => {
                vec.push(x);
                List(vec)
            }
            (a @ (Concat(..) | Program(..)), b @ (Concat(..) | Program(..))) => List(vec![a, b]),
        }
    }

    // Get a single program from the collection, if there is any.
    pub fn sample(&self) -> Option<Program<I>>
    where
        I: Clone,
    {
        match self {
            Programs::Program(prog) => Some(prog.clone()),
            Programs::List(vec) => vec.first().and_then(|p| p.sample()),
            Programs::Concat(inner, inst) => inner.sample().map(|mut prog| {
                prog.push(inst.clone());
                prog
            }),
        }
    }

    /// Run a function on each program.
    pub fn for_each<F>(self, mut f: F)
    where
        F: FnMut(Program<I>),
        I: Clone,
    {
        match self {
            Programs::Program(prog) => f(prog),
            Programs::List(vec) => {
                for p in vec {
                    p.for_each(&mut f);
                }
            }
            Programs::Concat(inner, inst) => {
                inner.for_each_ref(&mut |mut prog| {
                    prog.push(inst.clone());
                    f(prog);
                });
            }
        }
    }

    /// Run a function on each program, without consuming self.
    pub fn for_each_ref<F>(&self, f: &mut F)
    where
        // This still takes ownership of the given programs, because we need to construct them, not
        // all of them are actually stored in memory.
        F: FnMut(Program<I>),
        I: Clone,
    {
        match self {
            Programs::Program(prog) => f(prog.clone()),
            Programs::List(vec) => {
                for p in vec {
                    p.for_each_ref(f);
                }
            }
            Programs::Concat(inner, inst) => {
                inner.for_each_ref(
                    &mut (Box::new(|mut prog: Program<I>| {
                        prog.push(inst.clone());
                        f(prog);
                    }) as Box<dyn FnMut(Program<I>)>),
                );
            }
        }
    }

    /// Just like `for_each_ref`, but the closure can stop the iteration early.
    pub fn try_for_each_ref<F, B>(&self, f: &mut F) -> std::ops::ControlFlow<B>
    where
        F: FnMut(Program<I>) -> std::ops::ControlFlow<B>,
        I: Clone,
    {
        use std::ops::ControlFlow::{self, Continue};
        match self {
            Programs::Program(prog) => f(prog.clone()),
            Programs::List(vec) => {
                for p in vec {
                    p.try_for_each_ref(f)?;
                }
                Continue(())
            }
            Programs::Concat(inner, inst) => inner.try_for_each_ref(
                &mut (Box::new(|mut prog: Program<I>| {
                    prog.push(inst.clone());
                    f(prog)
                }) as Box<dyn FnMut(Program<I>) -> ControlFlow<_>>),
            ),
        }
    }

    pub fn to_vec(&self) -> Vec<Program<I>>
    where
        I: Clone,
    {
        let mut vec = Vec::with_capacity(self.len());
        self.for_each_ref(&mut |prog| vec.push(prog));
        vec
    }
}

impl<I> Default for Programs<I> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: Clone> From<Programs<I>> for Vec<Program<I>> {
    fn from(this: Programs<I>) -> Self {
        this.to_vec()
    }
}

impl<I: Clone> crate::graph::Programs for Programs<I> {
    type Program = Program<I>;
    fn len(&self) -> usize {
        self.len()
    }
    fn extend(&mut self, other: Self) {
        self.extend(other)
    }
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<I: std::fmt::Display + Clone> std::fmt::Display for Programs<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.for_each_ref(&mut |program| {
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
