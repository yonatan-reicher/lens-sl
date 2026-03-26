/// --- Main Files ---
/// Definitions of different word sizes, for example 4-bit, 8-bit, 64-bit.
mod word;
/// The search graph that we use for forward and backward search.
mod graph;
/// Definitions of how we represent programs in an efficient way.
mod programs;
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
/// Main loop where the big picture stuff happens.
mod main_loop;

// Here is a diagram of the dependency structure between some of the main modules:
//
//                              +-----------+
//                     +------> | main_loop | <---------------------------+
//                    /         +-----------+                              \
//                   /           ^  ^  ^  ^                                 \
//                  /           /   |   \  \                                 \
//                 /           /    \    \  +--------------+                  \
//                /           /      \    \                 \                  \
//          +-------+  +----------+   \    \  +-----------+  |             +--------+
//          | graph |  | programs |    \    +-| enumerate |  |             | Oracle |
//          +-------+  +----------+     \     +-----------+  |             +--------+
//                                      |                    |
//                                   +-----+                 |
//                                   | arm |                 |
//                                   +-----+                 |
//                                      ^                    |
//                                       \                   |
//                                        \   +------------------+
//                                         +--| reduce_bit_width |
//                                            +------------------+
//

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

#[derive(Clone, Copy, Debug, derive_more::Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    #[display("Forward")]
    Forward,
    #[display("Backward")]
    Backward,
}

impl Direction {
    pub const fn from_is_forward(is_forward: bool) -> Self {
        if is_forward {
            Self::Forward
        } else {
            Self::Backward
        }
    }
}

// Let's expose just the necessary items.

pub use arm::parse;
pub use arm::{BackwardMap, CondCode, Flags, FlagsBitField, Inst, OpCode, Register, State};
pub use main_loop::optimize;
pub use tui::{NoTui, Tui, TuiHook};
pub use word::{Word, Word4, Word8, Word64};
