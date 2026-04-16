//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::{Mask, Masked as MaskedState, State};
use crate::arm::{self, Register};
use crate::collect_registers::Collector;
use crate::direction::Direction;
use crate::intersect_all::intersect_all;
use crate::oracle::{Oracle, SmtOracle};
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{Cancelled, ShouldCancel, graph};
use crate::{Config, programs};

// std imports
use std::cell::{Ref, RefCell};
use std::ops::ControlFlow::{self, Break, Continue};
use std::rc::Rc;

use rustc_hash::{FxHashMap, FxHashSet};
use serde::de::DeserializeOwned;

use functionality::prelude::*;

use itertools::Itertools;

// =========================================== Graph ==============================================

type Inst<W> = arm::Inst<W, BitWord<W>>;

type Program<W> = programs::Program<Inst<W>>;

type Programs<W> = programs::Programs<Inst<W>>;

type Graph<W> = graph::Graph<State<W>, Programs<W>>;

// ====================================== Implementation ==========================================

// This is the main function that gets exposed.
/// `WT` for word size of the target program.
/// `WS` for word size of the synthesis process.
pub fn optimize<WT: Word + HasBitWord, WS: Word + HasBitWord + serde::de::DeserializeOwned>(
    c: Config<WT>,
    tui: &impl for<'g> TuiHook<&'g Graph<WS>, State<WS>>,
) -> Result<Option<Program<WT>>, Cancelled>
where
    BitWord<WS>: DeserializeOwned,
    <WS as All>::Iter: Clone,
{
    if c.program.is_empty() {
        return Ok(None);
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
    )
}

type Bank<W> = FxHashMap<MaskedState<W>, FxHashMap<MaskedState<W>, FxHashSet<Inst<W>>>>;
type BackwardBank<W> = FxHashMap<
    /* output */ MaskedState<W>,
    /* one input */
    FxHashMap<
        MaskedState<W>,
        FxHashMap</* the command */ Inst<W>, Rc<FxHashSet<MaskedState<W> /* all inputs */>>>,
    >,
>;

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
) -> Result<Option<Program<WT>>, Cancelled>
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
    let total_instructions = Inst::enumerate(*enumeration_info).count();
    let mut seen = FxHashSet::default();
    let mut current_states = vec![]; // TODO: rename to frontier.
    let mut next_states = vec![];
    let mut bank = Bank::default();
    let inputs_outputs = InputsOutputsCell::default();
    let mut oracle = ReducedProgramOracle {
        oracle: &mut oracle,
        oracle_reduced: &mut oracle_reduced,
        reducer: &reducer,
        tui,
        inputs_outputs: &inputs_outputs,
    };
    let biggest_mask = registers
        .iter()
        .cloned()
        .fold(Mask::JUST_FLAGS, |m, r| m | Mask::just_register(r));
    // ----- The Actual Loop -----------------------------------------------------------------------
    // We must start with at least one input...
    match oracle.verify(&[]) {
        Continue(()) => todo!(""),
        Break(ProgramOrRetry::Program(p)) => return Ok(Some(p)),
        Break(ProgramOrRetry::Retry) => (),
    }
    'restart: loop {
        current_states.clear();
        current_states.push((inputs_outputs.inputs().to_vec(), vec![]));
        next_states.clear();
        seen.clear();
        tui.reset_lengths();
        // ----- Reachability - Bfs loop to reach outputs ----------------------------------------
        for _length in 0..original_reduced.len() {
            tui.searching();
            tui.progress(0, total_instructions);
            let len = current_states.len();
            for (i, (inputs, prog)) in current_states.iter().cloned().enumerate() {
                tui.progress(i, len);
                // Should we stop?
                if should_cancel.check() {
                    return Err(Cancelled);
                }
                // First we need to check that all the states are properly represented in the bank.
                for inp in inputs.iter().cloned() {
                    if !bank.contains_key(&inp.masked(biggest_mask)) {
                        init_bank(&mut bank, inp.masked(biggest_mask), registers, immediates);
                    }
                }
                // The red code.
                let mut discarded = FxHashSet::<Inst<W>>::default();
                for sub_input_mask in biggest_mask.sub_masks() {
                    let sub_inputs = inputs.iter().map(|s| s.masked(sub_input_mask));
                    // Find all instructions (and their effects!) that can run from the current states.
                    // Instead of doing this by iterating all instructions, do this by intersection of equivalence
                    // classes that can run from the states.
                    let empty = Default::default();
                    let insts = sub_inputs
                        .clone()
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
                        .collect::<FxHashSet<_>>();
                    for inst in insts {
                        // We can't do this filtering as part of the intersection above because the
                        // discard set changes through the loop.
                        if discarded.contains(&inst) {
                            continue;
                        }
                        let outputs = inputs
                            .iter()
                            .map(|s| (*s).mutate(|s| inst.run(s)))
                            .collect::<Vec<_>>();
                        // Did we succeed?
                        if outputs == *inputs_outputs.outputs() {
                            let prog = prog.clone().mutate(|p| p.push(inst));
                            match oracle.verify(&prog) {
                                // Continue(()) => todo!("what do we do here? '{prog:?}'"),
                                Continue(()) => (),
                                Break(ProgramOrRetry::Program(p)) => return Ok(Some(p)),
                                Break(ProgramOrRetry::Retry) => continue 'restart,
                            }
                        }
                        if seen.contains(&outputs) {
                            continue;
                        }
                        seen.insert(outputs.clone());
                        // Extend Hila's discard set. Extend it by all the instructions which do the
                        // exact same thing as this instruction on the current inputs.
                        discarded.extend(insts_with_same_effect(
                            biggest_mask,
                            &bank,
                            sub_input_mask,
                            inputs.as_slice(),
                            outputs.as_slice(),
                        ));
                        // TODO: you know how to solve this memory allocation...
                        next_states.push((outputs, prog.clone().mutate(|p| p.push(inst))));
                    }
                }
            }
            tui.progress(len, len);
            // ------------------------------ Expand Phase --------------------------------------------
            let direction = Direction::Forward;
            tui.expanding(direction);
            //expand(&mut todo!(), g.tui);
            current_states.clear();
            std::mem::swap(&mut next_states, &mut current_states);
        } // end of length loop
        let lengths = next_states
            .iter()
            .map(|(_, p)| p)
            .chunk_by(|p| p.len())
            .into_iter()
            .map(|(len, progs)| format!("{len}: {}", progs.count()))
            .join("\n");
        println!("Progs");
        println!("{}", lengths);
        return Ok(None);
    }
}

enum ProgramOrRetry<W: Word + HasBitWord> {
    Program(Program<W>),
    Retry,
}

fn init_bank<W: Word + HasBitWord>(
    bank: &mut Bank<W>,
    inp: MaskedState<W>,
    regs: &[Register],
    imms: &[W],
) {
    debug_assert!(!bank.contains_key(&inp));
    // Make sure we don't re-initialize this state or a sub-state for no reason by making sure
    // they're already created.
    for s in inp.sub_states() {
        bank.entry(s).or_default();
    }
    // Now to the thing!
    for inst in Inst::enumerate(EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(regs),
        immediates: EnumerationInfoOptions::Limited(imms),
        include_nop: false,
        skip_cond_code: false,
    }) {
        let Some(_) = inst.run_masked(inp) else {
            continue;
        };
        let inp = inp & inst.read_mask(inp.state());
        let out = inst.run_masked(inp).unwrap();
        let class = bank.get_mut(&inp).unwrap().entry(out).or_default();
        class.insert(inst);
    }
}

#[derive(Debug, Default)]
#[allow(clippy::type_complexity)]
struct InputsOutputsCell<W>(RefCell<(Vec<State<W>>, Vec<State<W>>)>);

impl<W: Word> InputsOutputsCell<W> {
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

struct ReducedProgramOracle<'a, WBig: HasBitWord, W: HasBitWord> {
    inputs_outputs: &'a InputsOutputsCell<W>,
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
                    !self.inputs_outputs.contains(&inp, &out),
                    "Counter-example from reduced oracle should not have been seen before."
                );
                self.inputs_outputs.push(inp, out);
                Break(ProgramOrRetry::Retry)
            }
            verify::Result::Break(prog) => Break(prog),
            verify::Result::Continue => Continue(()),
        }
    }
}

/// Gets instructions which have the same effect on the input state
/// Top mask - the mask of the information relevant to the program search.
fn insts_with_same_effect<W: Word + HasBitWord>(
    top_mask: Mask,
    bank: &Bank<W>,
    sub_input_mask: Mask,
    inputs: &[State<W>],
    outputs: &[State<W>],
) -> impl Iterator<Item = Inst<W>> {
    // Look at super-masks of the input
    // TODO: We can do this more efficiently
    top_mask
        .sub_masks()
        .filter(move |m| sub_input_mask.is_sub_mask(m))
        .flat_map(move |sub_input_mask| {
            let sub_inputs = inputs.iter().map(move |s| s.masked(sub_input_mask));
            // And masks of the output that are in the in the bank
            // TODO: We should filter them before asking the bank, because asking the bank is slow
            top_mask
                .sub_masks()
                .filter({
                    let sub_inputs = sub_inputs.clone();
                    move |sub_output_mask| {
                        let sub_outputs = outputs.iter().map(|s| s.masked(*sub_output_mask));
                        sub_inputs.clone().zip(sub_outputs.clone()).all(
                            |(sub_input, sub_output)| {
                                bank.get(&sub_input)
                                    .expect("we have initialized this at the start")
                                    .contains_key(&sub_output)
                            },
                        )
                    }
                })
                .flat_map(move |sub_output_mask| {
                    let sub_outputs = outputs.iter().map(move |s| s.masked(sub_output_mask));
                    intersect_all(
                        sub_inputs
                            .clone()
                            .zip(sub_outputs)
                            .map(|(input, sub_output)| {
                                bank.get(&input)
                                    .expect("we have initialized this at the start")
                                    .get(&sub_output)
                                    .unwrap()
                            }),
                    )
                    .cloned()
                })
        })
}
