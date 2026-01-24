/// --- Main Files ---
/// Definitions of different word sizes, for example 4-bit, 8-bit, 64-bit.
mod word;
/// The instruction set.
mod isa;
/// Enumerating all instructions and whatever.
mod enumerate;
/// Definitions of how we represent programs in an efficient way.
mod programs;
/// The search graph that we use for forward and backward search.
mod graph;
/// This module collects registers and immediates that appear in a program.
mod collect;
/// Main loop where the big picture stuff happens.
mod main_loop;

// --- Utilities ---
/// TODO: Remove this probably.
mod shortest_path;

// Let's expose just the necessary items.

pub use isa::{CondCode, Flags, Inst, OpCode, Register};
pub use main_loop::optimize;
pub use word::{Word, Word4, Word8, Word64};
