//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::State;
use crate::arm::{BackwardMap, Inst};
use crate::backward_graph_sl;
use crate::bank::Bank;
use crate::collect_registers::Collector;
use crate::direction::Direction::{self, Backward, Forward};
use crate::graph;
use crate::intersect_all::intersect_all;
use crate::len::Len;
use crate::oracle::{Oracle, SmtOracle};
use crate::programs_sl;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{Cancelled, Config, OptimizeOutcome, OptimizeResult, ShouldCancel};

// std imports
use std::cell::{Ref, RefCell};
use std::mem::swap;
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

type Program<W> = programs_sl::Program<Inst<W>>;

type Programs<W> = programs_sl::Programs<Inst<W>>;

/// This is actually not used, it's just needed as an argument to the TUI, which we don't use.
/// Basically this is deprecated and here just as glue.
type Graph<W> = graph::Graph<State<W>, crate::programs::Programs<Inst<W>>>;

type BackwardGraph<W> = backward_graph_sl::BackwardGraph<State<W>, Inst<W>>;
type BackwardGraphPath<'a, W> = backward_graph_sl::BackwardGraphPath<'a, State<W>, Inst<W>>;

// =================================================================================================
//                                         Implementation
// =================================================================================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word + HasBitWord, WS: Word + HasBitWord + DeserializeOwned>(
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

    let enumeration_info = EnumerationInfo {
        inp_registers: EnumerationInfoOptions::Limited(&registers),
        out_registers: EnumerationInfoOptions::Limited(&registers),
        immediates: EnumerationInfoOptions::Limited(&immediates),
        include_nop: false,
        skip_cond_code: false,
    };

    let n_instructions = Inst::enumerate(enumeration_info).count();

    let bm = if !c.forward_only {
        BackwardMap::new(&registers).unwrap()
    } else {
        BackwardMap::default()
    };

    // This should (obviously) be the very last thing initialized.
    let started_at = Instant::now();

    let optimizer = Optimizer {
        config: c,
        original_reduced: reduced_program,
        enumeration_info,
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
        tui,
        postfix_len: 0,
        prefix_len: 0,
        splitting_buffer: vec![],
        discard_sets: vec![],
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
    tui: &'a dyn TuiHook<&'a Graph<W>, State<W>>,
    postfix_len: usize,
    prefix_len: usize,
    /// A buffer for saving states to add to the next frontier that you still haven't checked if
    /// they need splitting.
    /// TODO: Is this forward only?
    splitting_buffer: Vec<(Vec<State<W>>, Programs<W>)>,
    /// Index is frontier index.
    discard_sets: Vec<FxHashSet<Inst<W>>>,
    last_iteration_completion_percent: (usize, usize),
}

impl<WBig, W> Optimizer<'_, WBig, W>
where
    WBig: Word + HasBitWord,
    W: Word + HasBitWord<BitWord: DeserializeOwned> + DeserializeOwned,
{
    pub fn optimize(mut self) -> OptimizeResult<WBig> {
        // Plan: Do some setup, then start the optimization loop.
        // We have to start the optimization with at least one counter-example, so we deal with that
        // first.
        match self.oracle.verify(&[]) {
            Continue(()) => unimplemented!(
                "we do not deal with the case where the empty program is correct on the reduced oracle."
            ),
            Break(ProgramOrRetry::Program(p)) => {
                return OptimizeResult {
                    outcome: OptimizeOutcome::Program(p),
                    elapsed: self.started_at.elapsed(),
                    last_iteration_completion_percent: (0, 0),
                };
            }
            Break(ProgramOrRetry::Retry) => (),
        }
        assert_eq!(self.counter_examples.inputs().len(), 1);
        // ------ Initialization -------------------------------------------------------------------
        let empty_program = Programs::empty_program();
        // backwards
        self.backward_graph.0.push(vec![
            [(self.counter_examples.outputs()[0], empty_program.clone())]
                .into_iter()
                .collect(),
        ]);
        self.postfix_len = 0;
        // forward
        self.forward_frontier.clear();
        self.forward_frontier.insert(
            self.counter_examples.inputs().to_vec(),
            empty_program.clone(),
        );
        self.next_forward_frontier.clear();
        self.prefix_len = 0;
        // ------ Main Loop ------------------------------------------------------------------------
        while self.postfix_len + self.prefix_len + 1 < self.original_reduced.len() {
            let _length = self.postfix_len + self.prefix_len;
            self.tui.progress(0, self.stats.n_instructions);
            let direction = self.decide_direction();
            self.tui.expanding(direction);
            let ret = match direction {
                // This is where the magic actually happens.
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
                        last_iteration_completion_percent: self.last_iteration_completion_percent,
                    };
                }
                Break(Ok(p)) => {
                    return OptimizeResult {
                        outcome: OptimizeOutcome::Program(p),
                        elapsed: self.started_at.elapsed(),
                        last_iteration_completion_percent: self.last_iteration_completion_percent,
                    };
                }
            }
        } // end of length loop
        OptimizeResult {
            outcome: OptimizeOutcome::NoProgram,
            elapsed: self.started_at.elapsed(),
            last_iteration_completion_percent: (1, 1),
        }
    }

    fn expand_forward(&mut self) -> ControlFlow<Result<Program<WBig>, Cancelled>> {
        let n_inst = self.stats.n_instructions;
        let n_states = self.forward_frontier.len();
        //
        let do_discard = true;
        if do_discard {
            // self.discard_sets.clear();
            self.discard_sets.iter_mut().for_each(|s| s.clear());
            self.discard_sets.resize(n_states, FxHashSet::default());
        }
        // For each instruction, go through the whole frontier and search for a connection!
        for (i_inst, inst) in Inst::enumerate(self.enumeration_info).enumerate() {
            self.last_iteration_completion_percent = (i_inst, n_inst);
            self.tui.progress(i_inst, n_inst);
            let mask = inst.potential_input_mask();
            //
            self.tui.progress_push();
            for (i_states, (states, prefixes)) in self.forward_frontier.iter().enumerate() {
                self.tui.progress(i_states, n_states);
                // Should we stop?
                if self.should_cancel.check() {
                    return Break(Err(Cancelled));
                }
                // Have we gone through an equivalent instruction already? If so, skip!
                if do_discard && self.discard_sets[i_states].contains(&inst) {
                    self.stats.n_discarded += 1;
                    continue;
                }
                // If I had structured the code better, this would have been a function called
                // `extend_and_split` or something.
                let next_states = states
                    .iter()
                    .map(|s| (*s).mutate(|s| inst.run(s)))
                    .collect::<Vec<_>>();
                let equivalent_insts = if do_discard {
                    intersect_all(states.iter().zip(&next_states).map(|(s, next_s)| {
                        self.bank
                            .get(&s.masked(mask))
                            .get(&next_s.masked(inst.potential_output_mask()))
                            .borrow()
                    }))
                } else {
                    FxHashSet::default().mutate(|s| {
                        s.insert(inst);
                    })
                };
                if do_discard {
                    self.discard_sets[i_states].extend(&equivalent_insts);
                }
                debug_assert!(
                        equivalent_insts.contains(&inst),
                        "on state {states:?} and mask {mask:?},
                        with next states {next_states:?},
                        inst {inst} was not contained in it's equivalent instructions set: {equivalent_insts:?}\n{:?}",
                        self.bank.get(&states[0].masked(mask)),
                    );
                let was_split = Self::split_prefix_class(
                    &mut self.splitting_buffer,
                    self.counter_examples,
                    self.oracle,
                    &mut self.backward_graph,
                    self.should_cancel,
                    prefixes
                        .clone()
                        .concat_many(equivalent_insts.iter().cloned().collect()),
                    next_states.clone(),
                    |next_states, progs| {
                        self.next_forward_frontier
                            .entry(next_states)
                            .or_default()
                            .extend([progs]);
                    },
                    self.tui,
                    self.enumeration_info,
                    &self.bm,
                    self.postfix_len,
                )?;
            }
            self.tui.progress_pop();
        }
        self.forward_frontier.clear();
        std::mem::swap(&mut self.next_forward_frontier, &mut self.forward_frontier);
        Continue(())
    }

    /// Expands the backward frontier by one more instruction. When reaches an state that looks fit
    /// for it, it builds a program and sends to the verifier, which means this can add counter
    /// examples or end the search, or do nothing.
    /// I am still not sure what to do if we added a counter-example, need to see how we will handle
    /// it.
    fn expand_backward(&mut self) -> ControlFlow<Result<Program<WBig>, Cancelled>> {
        // For each state in the forwards frontier, try to split it without expanding it.
        let len = self.forward_frontier.len();
        self.tui.progress(0, len);
        for (i, (states, progs)) in Self::reorder_frontier(&mut self.forward_frontier).enumerate() {
            self.last_iteration_completion_percent = (i, len);
            self.tui.progress(i, len);
            // Should we stop?
            if self.should_cancel.check() {
                return Break(Err(Cancelled));
            }
            let was_split = Self::split_prefix_class(
                &mut self.splitting_buffer,
                self.counter_examples,
                self.oracle,
                &mut self.backward_graph,
                self.should_cancel,
                progs.clone(),
                states.clone(),
                |states, progs| {
                    // We populate the next forward frontier, but this is just for splitting
                    // purposes. The programs are actually the same and don't have another
                    // instruction added.
                    self.next_forward_frontier
                        .entry(states)
                        .or_default()
                        .extend([progs]);
                },
                self.tui,
                self.enumeration_info,
                &self.bm,
                self.postfix_len + 1, // +1 because we are expanding!
            )?;
        }
        self.tui.progress(len, len);
        self.forward_frontier.clear();
        swap(&mut self.next_forward_frontier, &mut self.forward_frontier);
        // // For each instruction,
        // for (i_inst, inst) in Inst::enumerate(self.enumeration_info).enumerate() {
        //     self.tui.progress(i_inst, self.stats.n_instructions);
        //     // TODO: Move should_cancel checks more inside to make sure we don't do slow down the
        //     // testing process
        //     if self.should_cancel.check() {
        //         return Break(Err(Cancelled));
        //     }
        //     // And for each state,
        //     for (state, postfixes) in self.backward_frontier.iter() {
        //         // Calculate the next state, and add it!
        //         let next_postfixes = postfixes.clone().concat(inst);
        //         for next_state in inst.run_backward(*state, &self.bm) {
        //             self.next_backward_frontier
        //                 .entry(next_state)
        //                 .or_default()
        //                 .extend([next_postfixes.clone()]);
        //             // TODO: Do this before.
        //             // And if it's a winner, we may be done!
        //             next_postfixes
        //                 .try_each_reversed(|postfix| {
        //                     'retry: loop {
        //                         break match Self::try_each_matching_prefix(
        //                             &self.forward_frontier_ce_0,
        //                             self.counter_examples,
        //                             &next_state,
        //                             postfix,
        //                             |prefix| {
        //                                 let prog =
        //                                     prefix.chain(postfix.iter().cloned()).collect_vec();
        //                                 self.oracle.verify(&prog)
        //                             },
        //                         ) {
        //                             Continue(()) => Continue(()),
        //                             Break(ProgramOrRetry::Program(p)) => Break(p),
        //                             Break(ProgramOrRetry::Retry) => continue 'retry,
        //                         };
        //                     }
        //                 })
        //                 .map_break(Ok)?;
        //         }
        //     }
        // }
        // self.tui
        //     .progress(self.stats.n_instructions, self.stats.n_instructions);
        // // Done! Switch the buffers.
        // self.backward_frontier.clear();
        // std::mem::swap(
        //     &mut self.next_backward_frontier,
        //     &mut self.backward_frontier,
        // );
        //
        // let n_counter_examples = self.counter_examples.len();
        // for i in 0..n_counter_examples {
        //     self.tui.progress(i, n_counter_examples);
        //     Self::update_backward_graph(
        //         self.enumeration_info,
        //         &self.bm,
        //         &mut self.backward_graph,
        //         &self.counter_examples.outputs(),
        //         self.postfix_len,
        //         i,
        //         &self.should_cancel,
        //     )
        //     .map_break(Err)?;
        // }
        Continue(())
    }

    // On `Continue`, returns whether or not the class was split.
    fn split_prefix_class(
        splitting_buffer: &mut Vec<(Vec<State<W>>, Programs<W>)>,
        counter_examples: &CounterExamplesCell<W>,
        oracle: &mut ReducedProgramOracle<'_, WBig, W>,
        backward_graph: &mut BackwardGraph<W>,
        should_cancel: ShouldCancel,
        prefixes: Programs<W>,
        states: Vec<State<W>>,
        mut on_each_result: impl FnMut(Vec<State<W>>, Programs<W>),
        tui: &dyn TuiHook<&Graph<W>, State<W>>,
        ei: EnumerationInfo<W>,
        bm: &BackwardMap<W>,
        postfix_len: usize,
    ) -> ControlFlow<Result<Program<WBig>, Cancelled>, bool> {
        // Start with just the input.
        splitting_buffer.clear();
        splitting_buffer.push((states, prefixes));
        // While we still have things, get the next thing, and see if it needs splitting.
        // If not, mark it as good and remove it.
        let mut was_split = false;
        let mut new_state_possibilities = FxHashMap::<_, Programs<W>>::default();
        while let Some((next_states, next_prefixes)) = splitting_buffer.pop() {
            if should_cancel.check() {
                return Break(Err(Cancelled));
            }
            let up_to = (next_states.len() + 1).min(counter_examples.len());
            for ce in 0..up_to {
                Self::update_backward_graph(
                    ei,
                    bm,
                    backward_graph,
                    &counter_examples.outputs(),
                    postfix_len,
                    ce,
                    &should_cancel,
                )
                .map_break(Err)?;
            }
            // Find the states in the postfix inputs.
            let Ok(path) = next_states
                .iter()
                .try_fold(backward_graph.root(), |mut path, s| {
                    path.try_descend(*s)?;
                    Ok::<_, ()>(path)
                })
            else {
                // Not all were found, that means we are done with these prefixes.
                on_each_result(next_states, next_prefixes);
                continue;
            };
            // All states had matching postfixes, time to split. Splitting is either by running on
            // the next counter-example, or generating one if there isn't already.
            if !path.ended() {
                let next_ce = next_states.len();
                if let Some((inp, _)) = counter_examples.get(next_ce) {
                    // We still haven't ran on all of our counter-examples! Run and repeat.
                    let mut i = 0;
                    new_state_possibilities.clear();
                    tui.progress_push();
                    next_prefixes.each(|prefix| {
                        if next_prefixes.len() < 1_000_000 || i % 10_000 == 0 {
                            tui.progress(i, next_prefixes.len());
                        }
                        i += 1;
                        let prefix = prefix.collect_vec();
                        let out = prefix
                            .iter()
                            .cloned()
                            .fold(inp, |s, i| s.mutate(|s| i.run(s)));
                        new_state_possibilities
                            .entry(out)
                            .or_default()
                            .extend([Programs::program(prefix)]);
                    });
                    tui.progress_pop();
                    was_split = true;
                    // Add all the new classes to the buffer to check if they need further
                    // splitting!
                    for (new_state, its_prefixes) in new_state_possibilities.drain() {
                        // TODO: So can this
                        splitting_buffer.push((
                            next_states.iter().cloned().chain([new_state]).collect(),
                            its_prefixes,
                        ));
                    }
                }
            } else {
                match path.get(|postfix| {
                    // Find a counter example! Verify will stop the iteration when a
                    // counter-example is found, and the condition above will be true instead of
                    // false, causing this equivalence class will be split.
                    let mut i = 0;
                    tui.progress_push();
                    let ret = next_prefixes.try_each(|prefix| {
                        if next_prefixes.len() < 1_000_000 || i % 10_000 == 0 {
                            tui.progress(i, next_prefixes.len());
                        }
                        i += 1;
                        let prog = prefix.chain(postfix.iter().cloned()).collect_vec();
                        oracle.verify(&prog)
                    });
                    tui.progress_pop();
                    ret
                }) {
                    // Continue(any_postfix_matched) if any_postfix_matched => todo!(
                    //     "I think this only happens when we need to split but can't find a counter-example, but I'm not sure. Anyway, I don't know how to handle that case.\nNext states: {next_states:?}\nNext prefixes: {next_prefixes_len:?}",
                    //     next_prefixes_len=next_prefixes.len()
                    // ),
                    Continue(..) => on_each_result(next_states, next_prefixes),
                    Break(ProgramOrRetry::Program(p)) => return Break(Ok(p)),
                    Break(ProgramOrRetry::Retry) => {
                        splitting_buffer.push((next_states, next_prefixes));
                    }
                }
            }
        }
        Continue(was_split)
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

    fn reorder_frontier(
        frontier: &mut FxHashMap<Vec<State<W>>, Programs<W>>,
    ) -> impl Iterator<Item = (Vec<State<W>>, Programs<W>)> + '_ {
        frontier
            .drain()
        //  .sorted_by_key(|(_, progs)| usize::MAX - progs.len())
        // .sorted_by_key(|(_, progs)| progs.len())
    }

    fn decide_direction(&self) -> Direction {
        Direction::from_is_forward(
            self.config.forward_only
                || (2u32.pow((self.postfix_len + 1) as u32) as usize > self.prefix_len + 1),
        )
    }
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
        debug_assert_eq!(self.0.borrow().0.len(), self.0.borrow().1.len());
        self.0.borrow().0.len()
    }
    pub fn get(&self, i: usize) -> Option<(State<W>, State<W>)> {
        let (inps, outs) = &*self.0.borrow();
        Some((*inps.get(i)?, *outs.get(i)?))
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
                    "Counter-example from reduced oracle should not have been seen before.\nInput {}\nOutput {}\nProgram: {}",
                    inp,
                    out,
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
