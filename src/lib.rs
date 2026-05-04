// --- The Actual Algorithm ---
/// Main loop where the big picture stuff happens.
mod main_loop;
/// Rewrite of main loop with separation logic.
mod main_loop_sl;
mod main_loop_sl_v2;
mod main_loop_sl_v1;

// --- Main Files ---
// I tried to make them as independent as possible.
/// The search graph that we use for forward and backward search.
mod graph;
mod backward_graph;
mod backward_graph_sl;
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
/// The `verify` function from Lens.
mod verify;
/// A mapping from input states and output states to the instructions which take one to the other.
mod bank;
mod inst_input_table;

// --- Utilities ---
/// Return all permutations of a slice of iterators.
mod all_permutations;
/// TODO: Remove this probably.
mod shortest_path;
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
mod intersect_all;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Cancelled;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Algorithm {
    #[default]
    Lens,
    MonocleV3,
    MonocleV2,
    MonocleV1,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Config<'a, WBig, WShiftBig = crate::word::BitWord<WBig>> {
    pub algorithm: Algorithm,
    pub program: &'a [Inst<WBig, WShiftBig>],
    pub additional_registers: &'a [Register],
    pub additional_immediates: &'a [WBig],
    pub should_cancel: ShouldCancel,
    pub forward_only: bool,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub enum ShouldCancel {
    #[default]
    Never,
    At(std::time::Instant),
    Timeout(std::time::Duration),
}

impl ShouldCancel {
    pub fn resolve_timeout(&self, now: std::time::Instant) -> Self {
        use ShouldCancel::*;
        match self {
            Timeout(d) => At(now.checked_add(*d).unwrap_or(now)),
            _ => *self,
        }
    }

    pub fn check(&self) -> bool {
        use ShouldCancel::*;
        match self {
            Never => false,
            At(t) => std::time::Instant::now() >= *t,
            Timeout(_) => panic!(
                "ShouldCancel::Timeout must be resolved before checking with resolve_timeout"
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OptimizeOutcome<W, WShift = crate::word::BitWord<W>> {
    Program(Vec<Inst<W, WShift>>),
    NoProgram,
    Cancelled,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OptimizeResult<W, WShift = crate::word::BitWord<W>> {
    pub outcome: OptimizeOutcome<W, WShift>,
    pub elapsed: std::time::Duration,
    pub last_iteration_completion_percent: (usize, usize),
}

// Let's expose just the necessary items.

pub use arm::parse;
pub use arm::parse::{LiveValue, info_from_file, liveness_from_file};
pub use arm::{
    BackwardMap, CondCode, Flags, FlagsBitField, Inst, OpCode, Register, ShiftCode, State,
};
pub use tui::{NoTui, Tui, TuiHook};
pub use word::prelude::*;

pub fn optimize<WT, WS>(
    c: Config<WT>,
    tui: &impl for<'g> TuiHook<
        &'g crate::graph::Graph<State<WS>, crate::programs::Programs<Inst<WS>>>,
        State<WS>,
    >,
) -> OptimizeResult<WT>
where
    WT: Word + HasBitWord,
    WS: Word + HasBitWord + serde::de::DeserializeOwned,
    BitWord<WS>: serde::de::DeserializeOwned,
{
    match c.algorithm {
        Algorithm::Lens => main_loop::optimize(c, tui),
        Algorithm::MonocleV3 => main_loop_sl::optimize(c, tui),
        Algorithm::MonocleV2 => main_loop_sl_v2::optimize(c, tui),
        Algorithm::MonocleV1 => main_loop_sl_v1::optimize(c, tui),
    }
}
