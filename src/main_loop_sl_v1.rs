//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::{Mask, State};
use crate::arm::{BackwardMap, Inst};
use crate::backward_graph_sl;
use crate::bank::Bank;
use crate::collect_registers::Collector;
use crate::direction::Direction::{self, Backward, Forward};
use crate::graph;
use crate::intersect_all::intersect_all;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs;
use crate::programs_sl;
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

use functionality::RefIter;
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

type Program<W> = programs_sl::Program<Inst<W>>;

type Programs<W> = programs_sl::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, programs::Programs<Inst<W>>>;

type BackwardGraph<W> = backward_graph_sl::BackwardGraph<State<W>, Inst<W>>;

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
            last_iteration_completion_percent: (0, 0),
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
        inp_registers: EnumerationInfoOptions::Limited(&registers),
        out_registers: EnumerationInfoOptions::Limited(&registers),
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
        next_forward_frontier: default(),
        bank: Bank::new(enumeration_info),
        counter_examples,
        oracle: &mut ReducedProgramOracle {
            counter_examples,
            oracle,
            oracle_reduced,
            reducer: &reducer,
            tui,
        },
        backward_graph: default(),
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
        last_iteration_completion_percent: (0, 0),
    };

    optimizer.optimize()
}

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
    /// Swapping buffer for the forward frontier.
    next_forward_frontier: FxHashMap<Vec<State<W>>, Programs<W>>,
    /// Computes equivalence classes in a semi-lazy way.
    bank: Bank<'a, W>,
    /// List of counter-examples generated by the oracle. Updated mutably by the oracle.
    counter_examples: &'a CounterExamplesCell<W>,
    /// The oracle! Tells us whether or not a program is correct.
    oracle: &'a mut ReducedProgramOracle<'a, WBig, W>,
    backward_graph: BackwardGraph<W>,
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
    last_iteration_completion_percent: (usize, usize),
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
                    last_iteration_completion_percent: (0, 0),
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
        self.backward_graph.0.push(vec![
            [(self.counter_examples.outputs()[0], empty_program.clone())]
                .into_iter()
                .collect(),
        ]);
        self.postfix_len = 0;
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
            self.next_forward_frontier.clear();
            self.prefix_len = 0;
            //
            while self.postfix_len + self.prefix_len < self.original_reduced.len() {
                let _length = self.postfix_len + self.prefix_len;
                self.tui.progress(0, self.stats.n_instructions);
                let direction = self.decide_direction();
                self.tui.expanding(direction);
                self.tui.report_length(Forward, self.prefix_len);
                self.tui.report_length(Backward, self.postfix_len);
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
                            last_iteration_completion_percent: self
                                .last_iteration_completion_percent,
                        };
                    }
                    Break(Ok(ProgramOrRetry::Program(p))) => {
                        return OptimizeResult {
                            outcome: OptimizeOutcome::Program(p),
                            elapsed: self.started_at.elapsed(),
                            last_iteration_completion_percent: self
                                .last_iteration_completion_percent,
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
                last_iteration_completion_percent: self.last_iteration_completion_percent,
            };
        }
    }

    fn expand_forward(&mut self) -> ControlFlow<Result<ProgramOrRetry<WBig>, Cancelled>> {
        let len = self.forward_frontier.len();
        for (i, (states, prog)) in self.forward_frontier.iter().enumerate() {
            self.tui.progress(i, len);
            self.last_iteration_completion_percent = (i, len);
            // Should we stop?
            if self.should_cancel.check() {
                return Break(Err(Cancelled));
            }
            // The red code.
            let do_discard = true;
            let do_subsumption = true;
            let mut discarded = FxHashSet::<Inst<W>>::default();
            self.tui.progress_push();
            for (i_inst, inst) in Inst::enumerate(self.enumeration_info).enumerate() {
                self.tui.progress(i_inst, self.stats.n_instructions);
                let mask = inst.potential_input_mask();
                {
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
                            &mut self.backward_graph,
                            self.postfix_len,
                            &self.should_cancel,
                            self.enumeration_info,
                            &self.bm,
                            self.counter_examples,
                            &next_states,
                            |postfix| {
                                prefixes.try_each(|prefix| {
                                    // Find the full program!
                                    let prog = prefix.chain(postfix.iter().cloned()).collect_vec();
                                    self.oracle.verify(&prog)
                                })
                            },
                        ).map_break(|b| {
                            self.tui.progress_pop();
                            b
                        })?;
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
                                        .get(&next_s.masked(inst.potential_write_mask()))
                                        .borrow()
                                },
                            )));
                        }
                    }
                    let prog = prog.clone().concat(inst);
                    self.next_forward_frontier
                        .entry(next_states)
                        .or_default()
                        .extend([prog]);
                }
            }
            self.tui.progress_pop();
        }
        self.tui.progress(len, len);
        self.forward_frontier.clear();
        std::mem::swap(&mut self.next_forward_frontier, &mut self.forward_frontier);
        Continue(())
    }

    fn expand_backward(&mut self) -> ControlFlow<Result<ProgramOrRetry<WBig>, Cancelled>> {
        // For each state in the forwards frontier, try to split it without expanding it.
        let len = self.forward_frontier.len();
        self.tui.progress(0, len);
        for (i, (states, prefixes)) in self.forward_frontier.iter().enumerate() {
            self.last_iteration_completion_percent = (i, len);
            self.tui.progress(i, len);
            // Should we stop?
            if self.should_cancel.check() {
                return Break(Err(Cancelled));
            }
            Self::try_each_matching_postfix(
                &mut self.backward_graph,
                self.postfix_len + 1, // +1 beacuse we are expanding!
                &self.should_cancel,
                self.enumeration_info,
                &self.bm,
                self.counter_examples,
                states,
                |postfix| {
                    prefixes.try_each(|prefix| {
                        // Find the full program!
                        let prog = prefix.chain(postfix.iter().cloned()).collect_vec();
                        self.oracle.verify(&prog)
                    })
                },
            )?;
        }
        self.tui.progress(len, len);
        Continue(())
    }

    /// Run on all postfixes that given the states to run from, output the same state as their
    /// matching counter-example's output.
    fn try_each_matching_postfix<T>(
        backward_graph: &mut BackwardGraph<W>,
        postfix_len: usize,
        should_cancel: &ShouldCancel,
        ei: EnumerationInfo<W>,
        bm: &BackwardMap<W>,
        counter_examples: &CounterExamplesCell<W>,
        inputs: &[State<W>],
        f: impl FnMut(&[Inst<W>]) -> ControlFlow<T>,
    ) -> ControlFlow<Result<T, Cancelled>> {
        assert_eq!(counter_examples.len(), inputs.len());
        for ce in 0..inputs.len() {
            Self::update_backward_graph(
                ei,
                bm,
                backward_graph,
                &counter_examples.outputs(),
                postfix_len,
                ce,
                should_cancel,
            )
            .map_break(Err)?;
        }
        // Find the states in the postfix inputs.
        let Ok(path) = inputs
            .iter()
            .try_fold(backward_graph.root(), |mut path, s| {
                path.try_descend(*s)?;
                Ok::<_, ()>(path)
            })
        else {
            return Continue(());
        };
        assert!(path.ended());
        path.get(f).map_continue(|_| ()).map_break(Ok)
    }

    fn update_backward_graph(
        ei: EnumerationInfo<W>,
        bm: &BackwardMap<W>,
        backward_graph: &mut BackwardGraph<W>,
        outputs: &[State<W>],
        postfix_length: usize,
        ce: usize,
        should_cancel: &ShouldCancel,
    ) -> ControlFlow<Cancelled> {
        // Implementation: treat the backward graph as a matrix, where the row index is length, and the
        // column index is counter example index. Go through column `ce` top to bottom, assuming all 0
        // <= i < ce have been initialized. For each cell, if we need to, initialize it. Once we
        // initialized a cell, we never have to mutate it again.
        let output = outputs[ce];
        let same_as = (0..ce).find(|i| outputs[*i] == output);
        // Initializing the first row (postfixes of length 0)
        if backward_graph.0[0].len() == ce {
            let cell = FxHashMap::default().mutate(|m| {
                m.insert(outputs[ce], Programs::empty_program());
            });
            backward_graph.0[0].push(cell);
        }
        // Initializing the rest!
        for len in 1..=postfix_length {
            debug_assert!(
                backward_graph.0.len() >= len,
                "we are initializing this one by one"
            );
            if backward_graph.0.len() == len {
                backward_graph.0.push(vec![]);
            }
            let [.., prev_row, curr_row] = backward_graph.0.as_mut_slice() else {
                panic!();
            };
            let n_ces_ran_on = curr_row.len();
            debug_assert!(
                n_ces_ran_on >= ce,
                "assuming that we created the previous columns"
            );
            if n_ces_ran_on == ce {
                // We haven't created this one yet!
                curr_row.push(FxHashMap::default());
                let (prev_cell, curr_cell) = (&mut prev_row[ce], &mut curr_row[ce]);
                if let Some(ce_before) = same_as {
                    // We already calculated this...
                    curr_row[ce] = curr_row[ce_before].clone();
                } else {
                    Self::update_backward_graph_cell(ei, prev_cell, curr_cell, bm, should_cancel)?;
                }
            }
        }
        Continue(())
    }

    fn update_backward_graph_cell(
        ei: EnumerationInfo<W>,
        prev: &mut FxHashMap<State<W>, Programs<W>>,
        current: &mut FxHashMap<State<W>, Programs<W>>,
        bm: &BackwardMap<W>,
        should_cancel: &ShouldCancel,
    ) -> ControlFlow<Cancelled> {
        for inst in Inst::enumerate(ei) {
            for (input, programs) in prev.iter() {
                if should_cancel.check() {
                    return Break(Cancelled);
                }
                let inp_list = inst.run_backward(*input, bm);
                let new_progs = programs.clone().concat(inst);
                for new_input in inp_list {
                    current
                        .entry(new_input)
                        .or_default()
                        .extend([new_progs.clone()]);
                }
            }
        }
        Continue(())
    }

    fn decide_direction(&self) -> Direction {
        Direction::from_is_forward(
            self.config.forward_only
                || (2u32.pow((self.postfix_len + 1) as u32) as usize > self.prefix_len + 1),
        )
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
    inputs
        .iter()
        .map(|input| input.masked(input_mask))
        .map(|sub_input| {
            // For this state, return set of commands that can run from it.
            bank.get(&sub_input)
                .iter()
                .flat_map(|(_, set)| RefIter::new(set.borrow(), |x| *x))
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
                        bank.get(&i),
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
                            .map(|((_, bucket), sub_output)| bucket.get(&sub_output).borrow())
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
    pub fn len(&self) -> usize {
        self.0.borrow().0.len()
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
