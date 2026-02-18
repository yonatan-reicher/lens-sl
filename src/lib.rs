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
mod isa;
/// Enumerating all instructions and whatever.
mod enumerate;
// TODO:
// /// Representation of the state of the program execution.
// mod state;
/// This module collects registers that we consider for synthesis.
mod collect_registers;
/// Main loop where the big picture stuff happens.
mod main_loop;

// Here is a diagram of the dependency structure between some of the main modules:
//
//                              +-----------+
//                     +------> | main_loop | <---------------+
//                    /         +-----------+                  \
//                   /           ^  ^  ^  ^                     \
//                  /           /   |   \  \                     \
//                 /           /    \    \  +--------------+      \
//                /           /      \    \                 \      \
//          +-------+  +----------+   \    \  +-----------+  |  +-------+
//          | graph |  | programs |    \    +-| enumerate |  |  | state |
//          +-------+  +----------+     \     +-----------+  |  +-------+
//                                      |                    |      ^
//                                   +-----+                 |      |
//                                   | isa |                 |      |
//                                   +-----+                 |      |
//                                      ^                    |      |
//                                       \                   |      |
//                                        \   +------------------+  /
//                                         +--| reduce_bit_width |-+
//                                            +------------------+
//

// --- Utilities ---
/// Return all permutations of a slice of iterators.
mod all_permutations;
/// TODO: Remove this probably.
mod shortest_path;
mod iter_slice_or_single;

// Let's expose just the necessary items.

pub use isa::{CondCode, Flags, Inst, OpCode, Register, State};
pub use main_loop::optimize;
pub use word::{Word, Word4, Word8, Word64};
