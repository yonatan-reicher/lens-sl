/// Definitions of different word sizes, for example 4-bit, 8-bit, 64-bit.
mod word;
mod isa;
mod enumerate;
mod programs;
mod graph;
/// This module collects registers and immediates that appear in a program.
mod collect;
mod main_loop;
mod shortest_path;

pub use isa::{CondCode, Flags, Inst, OpCode, Register};
pub use word::{Word, Word4, Word8, Word64};

pub use main_loop::optimize;
