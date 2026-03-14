//! This module defines the `PrettyPrinter` type and `PrettyPrint` trait, and holds some utilities
//! to make printing things easier.

use std::fmt::{self, Debug, Write};
use std::mem;

pub trait PrettyPrint {
    fn pretty_print(&self) -> PrettyPrinter;
}

#[derive(Debug, Default)]
pub struct PrettyPrinter(PPInner);

// ---- Pretty Printer -------------

#[derive(Debug, Default)]
enum PPInner {
    #[default]
    Empty,
    /// Must not have a newline.
    Str(String),
    Indent(usize, Box<PrettyPrinter>),
    Many(Vec<PrettyPrinter>),
}

impl PrettyPrinter {
    pub const fn empty() -> Self {
        Self(PPInner::Empty)
    }

    /// In contrast to [Self::string], does not allow newlines.
    pub fn line(s: String) -> Self {
        assert!(s.lines().count() < 2);
        Self(PPInner::Str(s))
    }

    pub fn many(v: Vec<PrettyPrinter>) -> Self {
        Self(PPInner::Many(v))
    }

    pub fn str(s: &str) -> Self {
        let mut v = s
            .lines()
            .map(|l| Self::line(l.to_string()))
            .collect::<Vec<_>>();
        match v.as_mut_slice() {
            [] => Self::empty(),
            [l] => mem::take(l),
            _ => Self::many(v),
        }
    }

    pub fn write(self, w: &mut impl Write) -> fmt::Result {
        let mut state = State {
            prefix: String::new(),
        };
        return write_inner(self, w, &mut state);

        struct State {
            prefix: String,
        }

        fn write_inner(this: PrettyPrinter, w: &mut impl Write, state: &mut State) -> fmt::Result {
            use PPInner::*;
            match this.0 {
                Empty => (),
                Str(s) => writeln!(w, "{}{}", state.prefix, s)?,
                Indent(i, x) => {
                    let og_len = state.prefix.len();
                    state.prefix.extend((0..i).map(|_| ' '));
                    write_inner(*x, w, state)?;
                    state.prefix.drain(og_len..);
                }
                Many(v) => {
                    for x in v {
                        write_inner(x, w, state)?;
                    }
                }
            }
            Ok(())
        }
    }
}

// --- Some implementations ---

impl PrettyPrint for str {
    fn pretty_print(&self) -> PrettyPrinter {
        PrettyPrinter::str(self)
    }
}

impl<T: PrettyPrint> PrettyPrint for [T] {
    fn pretty_print(&self) -> PrettyPrinter {
        let v = self.iter().map(PrettyPrint::pretty_print).collect();
        PrettyPrinter::many(v)
    }
}
