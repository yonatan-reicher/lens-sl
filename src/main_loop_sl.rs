//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::{Mask, Masked as MaskedState, State};
use crate::arm::{BackwardMap, Inst, Register};
use crate::collect_registers::Collector;
use crate::direction::Direction::{self, Backward, Forward};
use crate::intersect_all::intersect_all;
use crate::oracle::{Oracle, SmtOracle};
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{graph, OptimizeOutcome, OptimizeResult, ShouldCancel};
use crate::{programs, Config};

// std imports
use std::cell::{Ref, RefCell};
use std::ops::ControlFlow::{self, Break, Continue};
use std::time::{Duration, Instant};

use rustc_hash::{FxHashMap, FxHashSet};
use serde::de::DeserializeOwned;

use functionality::prelude::*;

use itertools::Itertools;

// =================================================================================================
//                                          Short-hands
// =================================================================================================

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Programs<W>>;

// =================================================================================================
//                                         Implementation
// =================================================================================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word + HasBitWord, WS: Word + HasBitWord + serde::de::DeserializeOwned>(
    c: Config<WT>,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
) -> OptimizeResult<WT>
where
    BitWord<WS>: DeserializeOwned,
    <WS as All>::Iter: Clone,
{
    if c.program.is_empty() {
        return OptimizeResult {
            outcome: OptimizeOutcome::NoProgram,
            elapsed: Duration::ZERO,
        };
    }

    let mut reducer = Reducer::<WT, WS>::default();
    let mut reduced_program = Vec::with_capacity(c.program.len());
    for inst in c.program {
        // This puts the original unreduced constants into the reducer.
        reduced_program.push(inst.reduce(&mut reducer));
    }
    let additional_immediates_reduced: Vec<WS> = c
        .additional_immediates
        .iter()
        .map(|i| reducer.reduce(*i, &ImmediateInfo { is_shift: false }))
        .collect();

    // Collect all the registers and immediates that might be useful for synthesis.
    let registers = Collector::new()
        .mutate(|col| col.program(c.program))
        .pipe(|c| c.registers)
        .mutate(|r| r.extend(c.additional_registers))
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());
    let immediates: Vec<WS> = reducer
        .immediates()
        .chain(additional_immediates_reduced)
        .collect::<Vec<WS>>()
        .mutate(|r| r.sort())
        .mutate(|r| r.dedup());

    let oracle = SmtOracle::new(c.program.to_vec());
    let oracle_reduced = SmtOracle::new(reduced_program.clone());

    synthesize::<WT, WS>(
        &registers,
        &immediates,
        oracle,
        oracle_reduced,
        reducer,
        reduced_program,
        c.should_cancel,
        tui,
        c,
    )
}

type Bank<W> = FxHashMap<MaskedState<W>, FxHashMap<MaskedState<W>, FxHashSet<Inst<W>>>>;

#[allow(clippy::too_many_arguments)]
fn synthesize<WT, W>(
    registers: &[Register],
    immediates: &[W],
    mut oracle: impl Oracle<[Inst<WT>], State<WT>>,
    mut oracle_reduced: impl Oracle<[Inst<W>], State<W>>,
    reducer: Reducer<WT, W>,
    original_reduced: Program<W>,
    should_cancel: ShouldCancel,
    tui: &impl for<'g> TuiHook<&'g Graph<W>, State<W>>,
    c: Config<WT>,
) -> OptimizeResult<WT>
where
    WT: Word + HasBitWord,
    W: Word + HasBitWord + DeserializeOwned,
    BitWord<W>: DeserializeOwned,
    <W as All>::Iter: Clone,
{
    // ----- Initialization ------------------------------------------------------------------------
    let enumeration_info = &EnumerationInfo::<W> {
        registers: EnumerationInfoOptions::Limited(registers),
        immediates: EnumerationInfoOptions::Limited(immediates),
        include_nop: false,
        skip_cond_code: false,
    };
    let mut forward_seen = FxHashSet::default();
    let mut backward_seen = FxHashSet::default();
    // The frontier is the set of discovered-but-unvisited states. Kind of like the current layer in
    // a BFS.
    let mut forward_frontier = vec![];
    let mut next_forward_frontier = vec![];
    let mut backward_frontier = vec![];
    let mut next_backward_frontier = vec![];
    let mut forward_bank = Bank::default();
    let mut backward_bank = Bank::default();
    let counter_examples = &CounterExamplesCell::default();
    let mut oracle = ReducedProgramOracle {
        oracle: &mut oracle,
        oracle_reduced: &mut oracle_reduced,
        reducer: &reducer,
        tui,
        counter_examples,
    };
    let bm = if !c.forward_only {
        BackwardMap::new(registers).unwrap()
    } else {
        BackwardMap::default()
    };
    let started_at = Instant::now();
    let should_cancel = should_cancel.resolve_timeout(started_at);
    let mut stats = Stats {
        n_instructions: Inst::enumerate(*enumeration_info).count(),
        ..Stats::default()
    };
    // The mask that contains all the things in the state we might care about.
    let top_mask = registers
        .iter()
        .cloned()
        .fold(Mask::JUST_FLAGS, |m, r| m | Mask::just_register(r));
    // ----- The Actual Loop -----------------------------------------------------------------------
    // We must start with at least one input...
    match oracle.verify(&[]) {
        Continue(()) => todo!(""),
        Break(ProgramOrRetry::Program(p)) => {
            return OptimizeResult {
                outcome: OptimizeOutcome::Program(p),
                elapsed: started_at.elapsed(),
            };
        }
        Break(ProgramOrRetry::Retry) => (),
    }
    'restart: loop {
        // forward
        forward_seen.clear();
        forward_seen.insert(counter_examples.inputs().to_vec());
        forward_frontier.clear();
        forward_frontier.push((counter_examples.inputs().to_vec(), vec![]));
        next_forward_frontier.clear();
        // backward
        backward_seen.clear();
        backward_seen.insert(counter_examples.outputs().to_vec());
        backward_frontier.clear();
        backward_frontier.push((counter_examples.outputs().to_vec(), vec![]));
        next_backward_frontier.clear();
        //
        tui.reset_lengths();
        for _length in 0..original_reduced.len() {
            tui.searching();
            tui.progress(0, stats.n_instructions);
            let direction = if c.forward_only || forward_frontier.len() < backward_frontier.len() {
                Forward
            } else {
                Backward
            };
            let (bank, seen, other_side_seen, frontier, next_frontier, other_side_frontier) =
                match direction {
                    Forward => (
                        &mut forward_bank,
                        &mut forward_seen,
                        &backward_seen,
                        &mut forward_frontier,
                        &mut next_forward_frontier,
                        &backward_frontier,
                    ),
                    Backward => (
                        &mut backward_bank,
                        &mut backward_seen,
                        &forward_seen,
                        &mut backward_frontier,
                        &mut next_backward_frontier,
                        &forward_frontier,
                    ),
                };
            let len = frontier.len();
            for (i, (states, prog)) in frontier.iter().cloned().enumerate() {
                tui.progress(i, len);
                // Should we stop?
                if should_cancel.check() {
                    return OptimizeResult {
                        outcome: OptimizeOutcome::Cancelled,
                        elapsed: started_at.elapsed(),
                    };
                }
                // First we need to check that all the states are properly represented in the bank.
                for s in states.iter().cloned() {
                    if !bank.contains_key(&s.masked(top_mask)) {
                        init_bank(bank, s.masked(top_mask), *enumeration_info, direction, &bm);
                    }
                }
                // The red code.
                let do_discard = false && direction == Forward;
                let mut discarded = FxHashSet::<Inst<W>>::default();
                for mask in top_mask.sub_masks() {
                    for inst in insts_with_precondtion(bank, &states, mask) {
                        // We can't do this filtering as part of the selecting the instructions
                        // because the discard set changes through the loop.
                        if discarded.contains(&inst) {
                            stats.n_discarded += 1;
                            stats.last_discard_size = discarded.len();
                            continue;
                        }
                        let next_states = states
                            .iter()
                            .map(|s| (*s).mutate(|s| inst.run(s)))
                            .collect::<Vec<_>>();
                        // Did we succeed?
                        if other_side_seen.contains(&next_states) {
                            // Find the full program!
                            let mut prog = prog.clone().mutate(|p| p.push(inst)).mutate(|p| {
                                p.extend(
                                    other_side_frontier
                                        .iter()
                                        .find_map(|(s, p)| (*s == next_states).then_some(p))
                                        .unwrap()
                                        .iter()
                                        .cloned()
                                        .rev(),
                                )
                            });
                            match direction {
                                Forward => (),
                                Backward => prog.reverse(),
                            }
                            match oracle.verify(&prog) {
                                // Continue(()) => todo!("what do we do here? '{prog:?}'"),
                                Continue(()) => (),
                                Break(ProgramOrRetry::Program(p)) => {
                                    return OptimizeResult {
                                        outcome: OptimizeOutcome::Program(p),
                                        elapsed: started_at.elapsed(),
                                    };
                                }
                                Break(ProgramOrRetry::Retry) => continue 'restart,
                            }
                        }
                        if seen.contains(&next_states) {
                            if do_discard {
                                discarded.insert(inst);
                            }
                            continue;
                        }
                        seen.insert(next_states.clone());
                        // Extend Hila's discard set. Extend it by all the instructions which do the
                        // exact same thing as this instruction on the current inputs.
                        if do_discard {
                            discarded.extend(insts_with_same_effect(
                                top_mask,
                                bank,
                                mask,
                                states.as_slice(),
                                next_states.as_slice(),
                                &mut stats,
                            ));
                        }
                        // TODO: you know how to solve this memory allocation...
                        next_frontier.push((next_states, prog.clone().mutate(|p| p.push(inst))));
                    }
                }
                if do_discard {
                    assert_eq!(discarded.len(), stats.n_instructions);
                }
            }
            tui.progress(len, len);
            // ------------------------------ Expand Phase -----------------------------------------
            tui.expanding(direction);
            //expand(&mut todo!(), g.tui);
            frontier.clear();
            std::mem::swap(next_frontier, frontier);
        } // end of length loop
        // let lengths = next_frontier
        //     .iter()
        //     .map(|(_, p)| p)
        //     .chunk_by(|p| p.len())
        //     .into_iter()
        //     .map(|(len, progs)| format!("{len}: {}", progs.count()))
        //     .join("\n");
        // println!("Progs");
        // println!("{}", lengths);
        return OptimizeResult {
            outcome: OptimizeOutcome::NoProgram,
            elapsed: started_at.elapsed(),
        };
    }
}

fn init_bank<W: Word + HasBitWord>(
    bank: &mut Bank<W>,
    state: MaskedState<W>,
    ei: EnumerationInfo<W>,
    dir: Direction,
    bm: &BackwardMap<W>,
) {
    use itertools::Either;
    debug_assert!(!bank.contains_key(&state));
    // Make sure we don't re-initialize this state or a sub-state for no reason by making sure
    // they're already created.
    for s in state.sub_states() {
        bank.entry(s).or_default();
    }
    // Now to the thing!
    for inst in Inst::enumerate(ei) {
        let next_states = match dir {
            Forward => Either::Left(inst.run_masked(state)),
            Backward => Either::Right(inst.run_backward_masked(state, bm)),
        };
        for next_state in next_states.into_iter() {
            let (state_mask, next_state_mask) = match dir {
                Forward => (inst.potential_read_mask(), inst.potential_write_mask()),
                Backward => (
                    inst.potential_write_mask(),
                    inst.potential_read_mask() | inst.potential_write_mask(), /* Why both? Well, basically, we can't have the second index be smaller than the first, it must contain it. I don't remember why. */
                ),
            };
            let (state, next_state) = (state & state_mask, next_state & next_state_mask);
            let class = bank.get_mut(&state).unwrap().entry(next_state).or_default();
            class.insert(inst);
        }
    }
}

/// Gets all instructions which have the same input
/// Find all instructions (and their effects!) that can run from the current states.
/// Instead of doing this by iterating all instructions, do this by intersection of equivalence
/// classes that can run from the states.
fn insts_with_precondtion<'a, W: Word + HasBitWord>(
    bank: &'a Bank<W>,
    inputs: &'a [State<W>],
    input_mask: Mask,
) -> impl Iterator<Item = Inst<W>> + use<'a, W> {
    let empty = Default::default();
    inputs
        .iter()
        .map(|input| input.masked(input_mask))
        .map(|sub_input| {
            // For this state, return set of commands that can run from it.
            bank.get(&sub_input)
                .unwrap_or(&empty)
                .iter()
                .flat_map(|(_, set)| set.iter().copied())
                .collect::<FxHashSet<_>>()
        })
        .collect::<Vec<_>>()
        // Intersect!
        .as_slice()
        .pipe(|a| intersect_all(a.iter()))
        .cloned()
        .collect::<Vec<_>>()
        .into_iter()
}

/// Gets instructions which have the same effect on the input state
/// Top mask - the mask of the information relevant to the program search.
fn insts_with_same_effect<W: Word + HasBitWord>(
    top_mask: Mask,
    bank: &Bank<W>,
    sub_input_mask: Mask,
    inputs: &[State<W>],
    outputs: &[State<W>],
    stats: &mut Stats,
) -> impl Iterator<Item = Inst<W>> {
    let stats = &*std::cell::UnsafeCell::from_mut(stats);
    // Look at super-masks of the input
    // TODO: We can do this more efficiently
    top_mask
        .sub_masks()
        .filter(move |m| sub_input_mask.is_sub_mask(m))
        .flat_map(move |sub_input_mask| {
            let sub_inputs = inputs
                .iter()
                .map(|i| i.masked(sub_input_mask))
                .map(|i| {
                    (
                        //Also contains the buckets!
                        i,
                        bank.get(&i).expect("we have initialized this at the start"),
                    )
                })
                .collect::<Vec<_>>();
            // And masks of the output that are in the in the bank
            // TODO: We should filter them before asking the bank, because asking the bank is slow
            top_mask
                .sub_masks()
                .map(|sub_output_mask| {
                    (
                        sub_output_mask,
                        outputs.iter().map(move |s| s.masked(sub_output_mask)),
                    )
                })
                .filter({
                    let sub_inputs = sub_inputs.clone();
                    move |(_sub_output_mask, sub_outputs)| {
                        sub_inputs
                            .iter()
                            .cloned()
                            .zip(sub_outputs.clone())
                            .all(|((_, bucket), sub_output)| bucket.contains_key(&sub_output))
                    }
                })
                .flat_map(move |(_sub_output_mask, sub_outputs)| {
                    // We know these are ran one by one.
                    let stats = unsafe { &mut *stats.get() };
                    stats.n_intersections += 1;
                    intersect_all(
                        sub_inputs
                            .iter()
                            .cloned()
                            .zip(sub_outputs)
                            .map(|((_, bucket), sub_output)| bucket.get(&sub_output).unwrap())
                            .inspect(|s| stats.total_intersection_input_sizes += s.len())
                            .collect::<Vec<_>>()
                            .into_iter(),
                    )
                    .cloned()
                    .inspect(|_| stats.total_intersection_output_sizes += 1)
                })
        })
}

// =================================================================================================
//                                       Inputs And Outputs
// =================================================================================================

/// The list of counter examples. Shared mutably.
#[derive(Debug, Default)]
#[allow(clippy::type_complexity)]
struct CounterExamplesCell<W>(RefCell<(Vec<State<W>>, Vec<State<W>>)>);

impl<W: Word> CounterExamplesCell<W> {
    pub fn inputs(&self) -> Ref<'_, [State<W>]> {
        Ref::map(self.0.borrow(), |(inps, _)| inps.as_slice())
    }
    pub fn outputs(&self) -> Ref<'_, [State<W>]> {
        Ref::map(self.0.borrow(), |(_, outs)| outs.as_slice())
    }
    pub fn push(&self, inp: State<W>, out: State<W>) {
        let (inps, outs) = &mut *self.0.borrow_mut();
        inps.push(inp);
        outs.push(out);
    }
    pub fn contains(&self, inp: &State<W>, out: &State<W>) -> bool {
        let (inps, outs) = &*self.0.borrow();
        inps.iter().zip(outs).contains(&(inp, out))
    }
}

// =================================================================================================
//                                       Verifying Programs
// =================================================================================================

/// This struct bundles up all the information needed to verify a candidate program against the
/// actual real original program. Use [Self::verify] to do the verification!
struct ReducedProgramOracle<'a, WBig: HasBitWord, W: HasBitWord> {
    counter_examples: &'a CounterExamplesCell<W>,
    oracle: &'a mut dyn Oracle<[Inst<WBig>], State<WBig>>,
    oracle_reduced: &'a mut dyn Oracle<[Inst<W>], State<W>>,
    reducer: &'a Reducer<WBig, W>,
    tui: &'a dyn TuiHook<&'a Graph<W>, State<W>>,
}

impl<'a, WBig, W> ReducedProgramOracle<'a, WBig, W>
where
    WBig: Word + HasBitWord,
    W: Word + HasBitWord,
{
    fn verify(&mut self, prog: &[Inst<W>]) -> ControlFlow<ProgramOrRetry<WBig>>
    where
        WBig: Word + HasBitWord,
        W: Word + HasBitWord,
    {
        use crate::verify;
        match verify::verify(
            prog,
            self.reducer,
            self.oracle_reduced,
            self.oracle,
            |equivalent_prog| Break(ProgramOrRetry::Program(equivalent_prog.to_vec())),
        ) {
            verify::Result::CounterExample(inp, out) => {
                self.tui.found_counter_example(inp, out);
                assert!(
                    !self.counter_examples.contains(&inp, &out),
                    "Counter-example from reduced oracle should not have been seen before."
                );
                self.counter_examples.push(inp, out);
                Break(ProgramOrRetry::Retry)
            }
            verify::Result::Break(prog) => Break(prog),
            verify::Result::Continue => Continue(()),
        }
    }
}

// =================================================================================================
//                                          Other Types
// =================================================================================================

enum ProgramOrRetry<W: Word + HasBitWord> {
    Program(Program<W>),
    Retry,
}

#[derive(Copy, Clone, Debug, Default)]
struct Stats {
    n_intersections: usize,
    total_intersection_input_sizes: usize,
    total_intersection_output_sizes: usize,
    n_discarded: usize,
    last_discard_size: usize,
    n_instructions: usize,
}
