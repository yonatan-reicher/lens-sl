mod isa;
mod enumerate;
mod graph;
mod algorithm;

pub use isa::{
    Inst,
    OpCode,
    CondCode,
    Register,
    Word,
    /* Word4, */ Word8, Word64,
};

pub use algorithm::optimize;

