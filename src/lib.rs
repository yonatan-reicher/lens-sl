// --- The Actual Algorithm ---
/// Main loop where the big picture stuff happens.
mod main_loop;
/// Rewrite of main loop with separation logic.
mod main_loop_sl;

// --- Main Files ---
// I tried to make them as independent as possible.
/// The search graph that we use for forward and backward search.
mod graph;
/// Definitions of how we represent programs in an efficient way.
mod programs;
/// Rewrite of programs module for separation logic!
mod programs_sl;
/// Converting programs to equivalent ones with a reduced bit-width.
mod reduce_bit_width;
/// The instruction set.
mod arm;
/// Responsible for things which find counter examples when checking programs for equivalence to
/// the original.
mod oracle;
/// This module collects registers that we consider for synthesis.
mod collect_registers;
mod tui;

// --- Utilities ---
/// Return all permutations of a slice of iterators.
mod all_permutations;
/// TODO: Remove this probably.
mod shortest_path;
mod iter_slice_or_single;
/// Helpers for using the `smtlib` crate.
mod smtlib_utils;
/// Helpers for dealing with booleans.
mod bool;
/// Defines a trait for things which have length
mod len;
mod all;
/// Definitions of different word sizes, for example 4-bit, 8-bit, 64-bit.
mod word;
mod direction;

// Let's expose just the necessary items.

pub use arm::parse;
pub use arm::parse::{LiveValue, info_from_file, liveness_from_file};
pub use arm::{
    BackwardMap, CondCode, Flags, FlagsBitField, Inst, OpCode, Register, ShiftCode, State,
};
pub use main_loop::optimize;
pub use main_loop_sl::optimize as optimize_sl;
pub use tui::{NoTui, Tui, TuiHook};
pub use word::{Word, Word4, Word8, Word64};
