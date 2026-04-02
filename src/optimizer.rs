use crate::arm::{Flags, Inst, Register, State, state};
use crate::word::prelude::*;
use std::ops::ControlFlow::{self, Break, Continue};

pub fn optimize<C: Config>(config: C, for_each: impl FnMut(Vec<Inst<C::WSmall>>)) {}

pub trait Config {
    type WBig: Word + HasBitWord;
    type WSmall: Word + HasBitWord;
    fn regs(&self) -> impl Iterator<Item = Register>;
    fn immediates(&self) -> impl Iterator<Item = Self::WBig>;
}

#[derive(Debug)]
struct Optimizer<C: Config> {
    c: C,
}
