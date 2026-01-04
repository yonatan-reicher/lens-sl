use num_traits::PrimInt;
use rustc_hash::FxHashMap;

use crate::isa::{Inst, Register, Word};

pub struct State<W: Word> {
    // TODO: Benchmark allocating this in a bump allocator instead.
    registers: Vec<(Register, W::Unsigned)>,
}

pub struct Graph<W: Word> {
    /// Maps a starting state and an ending state to instructions that transform
    /// that starting state into that ending state.
    forward: FxHashMap<(State<W>, State<W>), Vec<Inst<W>>>,
}
