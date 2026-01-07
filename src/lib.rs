mod isa;
mod enumerate;
mod graph;
mod algorithm;
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

pub use algorithm::optimize;

