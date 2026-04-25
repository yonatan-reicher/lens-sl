//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::{Mask, Masked as MaskedState, State};
use crate::arm::{BackwardMap, Inst};
use crate::collect_registers::Collector;
use crate::direction::Direction::{self, Backward, Forward};
use crate::graph;
use crate::intersect_all::intersect_all;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{Cancelled, Config, OptimizeOutcome, OptimizeResult, ShouldCancel};

// std imports
use std::cell::{Ref, RefCell};
use std::ops::ControlFlow::{self, Break, Continue};
use std::time::{Duration, Instant};

use rustc_hash::{FxHashMap, FxHashSet};
use serde::de::DeserializeOwned;

use functionality::prelude::*;

use itertools::Itertools;

// =================================================================================================
//                                            Explanation
// =================================================================================================

/*
 * How does this work?
 * This is an incremental search algorithm, similar to Lens. The key differences are:
 * 1. A flat data-structure.
 * 2. Pruning instructions that produce the same effect on the current inputs.
 */

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

    let oracle = &mut SmtOracle::new(c.program.to_vec());
    let oracle_reduced = &mut SmtOracle::new(reduced_program.clone());

    let counter_examples = &CounterExamplesCell::default();

    let bm = if !c.forward_only {
        BackwardMap::new(&registers).unwrap()
    } else {
        BackwardMap::default()
    };

    let enumeration_info = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&registers),
        immediates: EnumerationInfoOptions::Limited(&immediates),
        include_nop: false,
        skip_cond_code: false,
    };

    let n_instructions = Inst::enumerate(enumeration_info).count();

    // This should (obviously) be the very last thing initialized.
    let started_at = Instant::now();

    let optimizer = Optimizer {
        config: c,
        original_reduced: reduced_program,
        enumeration_info,
        forward_seen: default(),
        forward_frontier: default(),
        forward_frontier_ce_0: default(),
        backward_frontier: default(),
        next_forward_frontier: default(),
        next_forward_frontier_ce_0: default(),
        next_backward_frontier: default(),
        bank: default(),
        counter_examples,
        oracle: &mut ReducedProgramOracle {
            counter_examples,
            oracle,
            oracle_reduced,
            reducer: &reducer,
            tui,
        },
        bm,
        started_at,
        should_cancel: c.should_cancel.resolve_timeout(started_at),
        stats: Stats {
            n_instructions,
            ..Stats::default()
        },
        top_mask: registers
            .iter()
            .cloned()
            .fold(Mask::JUST_FLAGS, |m, r| m | Mask::just_register(r)),
        tui,
        postfix_len: 0,
        prefix_len: 0,
    };

    optimizer.optimize()
}

type Bank<W> = FxHashMap<MaskedState<W>, FxHashMap<MaskedState<W>, FxHashSet<Inst<W>>>>;

struct Optimizer<'a, WBig: Word + HasBitWord, W: Word + HasBitWord> {
    /// The configuration this optimizer was started with.
    config: Config<'a, WBig>,
    /// The original program, but bit-width reduced.
    original_reduced: Program<W>,
    // TODO: This should be a method.
    enumeration_info: EnumerationInfo<'a, W>,
    /// The set of state vectors we've already visited, and don't want to visit again.
    forward_seen: FxHashSet<Vec<State<W>>>,
    /// Set of discovered-but-unvisited state vectors and their corresponding prefixes. Kind of like
    /// the current layer in Breadth-First-Search.
    forward_frontier: FxHashMap<Vec<State<W>>, Programs<W>>,
    /// Set of reached-but-not-visited states in the backward search, and their corresponding programs.
    /// This is only on the first counter-example.
    backward_frontier: FxHashMap<State<W>, Programs<W>>,
    forward_frontier_ce_0: FxHashMap<State<W>, Programs<W>>,
    /// Swapping buffer for the forward frontier.
    next_forward_frontier: FxHashMap<Vec<State<W>>, Programs<W>>,
    next_forward_frontier_ce_0: FxHashMap<State<W>, Programs<W>>,
    /// Swapping buffer for the backward frontier.
    next_backward_frontier: FxHashMap<State<W>, Programs<W>>,
    /// Computes equivalence classes in a semi-lazy way.
    bank: Bank<W>,
    /// List of counter-examples generated by the oracle. Updated mutably by the oracle.
    counter_examples: &'a CounterExamplesCell<W>,
    /// The oracle! Tells us whether or not a program is correct.
    oracle: &'a mut ReducedProgramOracle<'a, WBig, W>,
    /// This is needed to run instructions backwards in time.
    bm: BackwardMap<W>,
    /// When did the search actually start? We need this to return an elapsed time at the end.
    started_at: Instant,
    /// A condition on when to stop the search and give up. Note that the config already contains a
    /// 'should cancel' field, but this one is what should be used instead.
    should_cancel: ShouldCancel,
    /// Various things. Might do something with this later.
    stats: Stats,
    /// The mask containing everything we care about in the search. It is the ⊤ (top) of the
    /// lattice of masks that are relevant to the search.
    top_mask: Mask,
    tui: &'a dyn TuiHook<&'a Graph<W>, State<W>>,
    postfix_len: usize,
    prefix_len: usize,
}

impl<'a, WBig: Word + HasBitWord, W: Word + HasBitWord> Optimizer<'a, WBig, W> {
    pub fn optimize(mut self) -> OptimizeResult<WBig> {
        // ----- The Actual Loop -----------------------------------------------------------------------
        // We must start with at least one input...
        match self.oracle.verify(&[]) {
            Continue(()) => todo!(""),
            Break(ProgramOrRetry::Program(p)) => {
                return OptimizeResult {
                    outcome: OptimizeOutcome::Program(p),
                    elapsed: self.started_at.elapsed(),
                };
            }
            Break(ProgramOrRetry::Retry) => (),
        }
        if self.counter_examples.inputs().is_empty() {
            unimplemented!(
                "we do not deal with the case where the oracle does not give a counter-example for the empty program."
            );
        }
        let empty_program = Programs::empty_program();
        self.backward_frontier
            .insert(self.counter_examples.outputs()[0], empty_program.clone());
        self.next_backward_frontier.clear();
        'restart: loop {
            // forward
            self.forward_seen.clear();
            self.forward_seen
                .insert(self.counter_examples.inputs().to_vec());
            self.forward_frontier.clear();
            self.forward_frontier.insert(
                self.counter_examples.inputs().to_vec(),
                empty_program.clone(),
            );
            self.forward_frontier_ce_0.clear();
            self.forward_frontier_ce_0
                .insert(self.counter_examples.inputs()[0], empty_program.clone());
            self.next_forward_frontier.clear();
            self.next_forward_frontier_ce_0.clear();
            self.prefix_len = 0;
            //
            self.tui.report_length(Direction::Forward, self.prefix_len);
            self.tui
                .report_length(Direction::Backward, self.postfix_len);
            while self.postfix_len + self.prefix_len < self.original_reduced.len() {
                let length = self.postfix_len + self.prefix_len;
                self.tui.progress(0, self.stats.n_instructions);
                let direction = Direction::from_is_forward(
                    self.config.forward_only
                        || self.forward_frontier_ce_0.len() <= self.backward_frontier.len(),
                );
                self.tui.expanding(direction);
                let ret = match direction {
                    Forward => self.expand_forward(),
                    Backward => self.expand_backward(),
                };
                match ret {
                    Continue(()) => {
                        match direction {
                            Forward => self.prefix_len += 1,
                            Backward => self.postfix_len += 1,
                        }
                        continue;
                    }
                    Break(Err(Cancelled)) => {
                        return OptimizeResult {
                            outcome: OptimizeOutcome::Cancelled,
                            elapsed: self.started_at.elapsed(),
                        };
                    }
                    Break(Ok(ProgramOrRetry::Program(p))) => {
                        return OptimizeResult {
                            outcome: OptimizeOutcome::Program(p),
                            elapsed: self.started_at.elapsed(),
                        };
                    }
                    Break(Ok(ProgramOrRetry::Retry)) => continue 'restart,
                }
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
                elapsed: self.started_at.elapsed(),
            };
        }
    }

    fn expand_forward(&mut self) -> ControlFlow<Result<ProgramOrRetry<WBig>, Cancelled>> {
        let len = self.forward_frontier.len();
        for (i, (states, prog)) in self.forward_frontier.iter().enumerate() {
            self.tui.progress(i, len);
            // Should we stop?
            if self.should_cancel.check() {
                return Break(Err(Cancelled));
            }
            // First we need to check that all the states are properly represented in the bank.
            for s in states.iter().cloned() {
                if !self.bank.contains_key(&s.masked(self.top_mask)) {
                    init_bank(
                        &mut self.bank,
                        s.masked(self.top_mask),
                        self.enumeration_info,
                        Direction::Forward,
                        &self.bm,
                    );
                }
            }
            // The red code.
            let do_discard = true;
            let mut discarded = FxHashSet::<Inst<W>>::default();
            for mask in Self::input_sub_masks(self.top_mask) {
                for inst in insts_with_precondtion(&self.bank, states, mask) {
                    // We can't do this filtering as part of the selecting the instructions
                    // because the discard set changes through the loop.
                    if discarded.contains(&inst) {
                        self.stats.n_discarded += 1;
                        self.stats.last_discard_size = discarded.len();
                        continue;
                    }
                    let next_states = states
                        .iter()
                        .map(|s| (*s).mutate(|s| inst.run(s)))
                        .collect::<Vec<_>>();
                    // Did we succeed?
                    {
                        let prefixes = prog.clone().concat(inst);
                        Self::try_each_matching_postfix(
                            &self.backward_frontier,
                            self.counter_examples,
                            &next_states,
                            |postfix| {
                                prefixes.try_each(|prefix| {
                                    // Find the full program!
                                    let prog = prefix.mutate(|p| p.extend(postfix.iter().rev()));
                                    self.oracle.verify(&prog)
                                })
                            },
                        )
                        .map_break(Ok)?;
                    }
                    if self.forward_seen.contains(&next_states) {
                        if do_discard {
                            discarded.insert(inst);
                        }
                        continue;
                    }
                    self.forward_seen.insert(next_states.clone());
                    // Extend Hila's discard set. Extend it by all the instructions which do the
                    // exact same thing as this instruction on the current inputs.
                    let do_subsumption = true;
                    if do_discard {
                        if do_subsumption {
                            discarded.extend(insts_with_same_effect(
                                self.top_mask,
                                &self.bank,
                                mask,
                                states.as_slice(),
                                next_states.as_slice(),
                                &mut self.stats,
                            ));
                        } else {
                            discarded.extend(intersect_all(states.iter().zip(&next_states).map(
                                |(s, next_s)| {
                                    self.bank
                                        .get(&s.masked(mask))
                                        .unwrap()
                                        .get(&next_s.masked(inst.potential_write_mask()))
                                        .unwrap()
                                },
                            )));
                        }
                    }
                    let prog = prog.clone().concat(inst);
                    self.next_forward_frontier_ce_0
                        .entry(next_states[0])
                        .or_default()
                        .extend(&prog);
                    self.next_forward_frontier
                        .entry(next_states)
                        .or_default()
                        .extend(&prog);
                }
            }
            if do_discard {
                assert_eq!(discarded.len(), self.stats.n_instructions);
            }
        }
        self.tui.progress(len, len);
        self.forward_frontier.clear();
        std::mem::swap(&mut self.next_forward_frontier, &mut self.forward_frontier);
        self.forward_frontier_ce_0.clear();
        std::mem::swap(
            &mut self.next_forward_frontier_ce_0,
            &mut self.forward_frontier_ce_0,
        );
        self.prefix_len += 1;
        Continue(())
    }

    /// Expands the backward frontier by one more instruction. When reaches an state that looks fit
    /// for it, it builds a program and sends to the verifier, which means this can add counter
    /// examples or end the search, or do nothing.
    /// I am still not sure what to do if we added a counter-example, need to see how we will handle
    /// it.
    /// TODO: above.
    fn expand_backward(&mut self) -> ControlFlow<Result<ProgramOrRetry<WBig>, Cancelled>> {
        // For each instruction,
        for (i_inst, inst) in Inst::enumerate(self.enumeration_info).enumerate() {
            self.tui.progress(i_inst, self.stats.n_instructions);
            // TODO: Move should_cancel checks more inside to make sure we don't do slow down the
            // testing process
            if self.should_cancel.check() {
                return Break(Err(Cancelled));
            }
            // And for each state,
            for (state, postfixes) in self.backward_frontier.iter() {
                // Calculate the next state, and add it!
                let next_postfixes = postfixes.clone().concat(inst);
                for next_state in inst.run_backward(*state, &self.bm) {
                    self.next_backward_frontier
                        .entry(next_state)
                        .or_default()
                        .extend(&next_postfixes);
                    // And if it's a winner, we may be done!
                    match next_postfixes.try_each(|mut postfix| {
                        postfix.reverse();
                        Self::try_each_matching_prefix(
                            &self.forward_frontier_ce_0,
                            self.counter_examples,
                            &next_state,
                            &postfix,
                            |prefix| {
                                // Here we already reversed the postfix, so we don't again.
                                let prog = prefix.mutate(|p| p.extend(&postfix));
                                self.oracle.verify(&prog)
                            },
                        )
                    }) {
                        Continue(()) => (),
                        Break(ProgramOrRetry::Program(p)) => {
                            return Break(Ok(ProgramOrRetry::Program(p)));
                        }
                        Break(ProgramOrRetry::Retry) => {
                            // This is a tough one. We need to add the counter-example, reset the
                            // forward search, but keep the state of this backward search.
                            // We actually can't do that, for a number of reasons. So we must sort
                            // of restart this backward search too.
                            self.next_backward_frontier.clear();
                            return Break(Ok(ProgramOrRetry::Retry));
                        }
                    }
                }
            }
        }
        self.tui
            .progress(self.stats.n_instructions, self.stats.n_instructions);
        // Done! Switch the buffers.
        self.backward_frontier.clear();
        std::mem::swap(
            &mut self.next_backward_frontier,
            &mut self.backward_frontier,
        );
        Continue(())
    }

    /// Run on all postfixes that given the states to run from, output the same state as their
    /// matching counter-example's output.
    fn try_each_matching_postfix<T>(
        backward_frontier: &FxHashMap<State<W>, Programs<W>>,
        counter_examples: &CounterExamplesCell<W>,
        inputs: &[State<W>],
        mut f: impl FnMut(Program<W>) -> ControlFlow<T>,
    ) -> ControlFlow<T> {
        match inputs {
            [] => {
                // No inputs! That means no counter-examples. Everything is correct, bob ross style.
                debug_assert!(counter_examples.inputs().is_empty());
                backward_frontier
                    .values()
                    .try_for_each(|postfixes| postfixes.try_each(&mut f))
            }
            [first, rest @ ..] => {
                let Some(good_on_first_input) = backward_frontier.get(first) else {
                    return Continue(());
                };
                good_on_first_input.try_each(|postfix| {
                    if rest.iter().enumerate().any(|(i, input)| {
                        let ce = i + 1; // Because we skipped the first one.
                        let output = postfix
                            .iter()
                            .rev()
                            .fold(*input, |s, i| s.mutate(|s| i.run(s)));
                        let expected_output = counter_examples.outputs()[ce];
                        output != expected_output
                    }) {
                        return Continue(());
                    }
                    f(postfix)
                })
            }
        }
    }

    /// Given a postfix, and a state that when ran the postfix on it gives the output of the first
    /// counter-example, run a function on each prefix that that when combined with the postfix
    /// gives the correct output on all counter-examples.
    /// The postfix should be in normal order, not reversed.
    fn try_each_matching_prefix<T>(
        forward_frontier: &FxHashMap<State<W>, Programs<W>>,
        counter_examples: &CounterExamplesCell<W>,
        state: &State<W>,
        postfix: &[Inst<W>],
        mut f: impl FnMut(Program<W>) -> ControlFlow<T>,
    ) -> ControlFlow<T> {
        debug_assert_eq!(
            postfix.iter().fold(*state, |s, i| s.mutate(|s| i.run(s))),
            counter_examples.outputs()[0]
        );
        let Some(prefixes) = forward_frontier.get(state) else {
            return Continue(());
        };
        prefixes.try_each(|prefix| {
            let (inputs, outputs) = (counter_examples.inputs(), counter_examples.outputs());
            let (other_inputs, expected_outputs) = (&inputs[1..], &outputs[1..]);
            let prog = prefix.iter().chain(postfix);
            let other_outputs = other_inputs
                .iter()
                .map(|input| prog.clone().fold(*input, |s, i| s.mutate(|s| i.run(s))));
            let good = other_outputs.zip(expected_outputs).all(|(a, b)| a == *b);
            std::mem::drop((inputs, outputs)); // Must drop these before calling the function.
            if good { f(prefix) } else { Continue(()) }
        })
    }

    fn input_sub_masks(mask: Mask) -> impl Iterator<Item = Mask> {
        let flags = mask.flags;
        let bit_mask_no_flags = Mask {
            flags: false,
            ..mask
        }
        .into_bit_mask();
        (0..=2).flat_map(move |n_regs| {
            if flags {
                [false, true].as_slice()
            } else {
                [false].as_slice()
            }
            .iter()
            .flat_map(move |&include_flags| {
                bit_mask_no_flags
                    .sub_masks_with_len(n_regs)
                    .map(|m| m.into_mask())
                    .map(move |m| {
                        m | Mask {
                            flags: include_flags,
                            ..Mask::EMPTY
                        }
                    })
            })
        })
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
) -> impl IntoIterator<Item = Inst<W>> + use<'a, W> {
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
        .pipe(|a| intersect_all(a.iter())) // TODO: Change this with a syntactic lookup
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
    sub_input_mask
        .masks_between(top_mask)
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
                    .pipe(|s| {
                        let _ = s
                            .iter()
                            .inspect(|_| stats.total_intersection_output_sizes += 1);
                        s
                    })
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
                    "Counter-example from reduced oracle should not have been seen before.
                    Program: {}",
                    prog.iter().map(|i| format!("{i:?}")).join("\n")
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
