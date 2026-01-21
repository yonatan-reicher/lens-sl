mod isa;
mod enumerate;
mod programs;
mod graph;
/// This module collects registers and immediates that appear in a program.
mod collect;
mod main_loop;
mod shortest_path;

pub use isa::{
    Inst,
    OpCode,
    CondCode,
    Register,
    Flags,
    Word,
    /* Word4, */ Word8, Word64,
};

pub use main_loop::optimize;

