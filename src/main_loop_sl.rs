//! The main loop for synthesis and optimization.
//! Here we have basically the code that you would see in the actual paper that describes Lens.

use crate::all::All;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state::Masked as MaskedState;
use crate::arm::{self, Register, State, run_program_masked, what_program_reads};
use crate::collect_registers::Collector;
use crate::direction::Direction;
use crate::intersect_all::intersect_all;
use crate::oracle::{Oracle, SmtOracle};
use crate::{Config, programs};
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use crate::{Cancelled, ShouldCancel, graph};

// std imports
use std::cell::{Ref, RefCell};
use std::ops::ControlFlow::{self, Break, Continue};

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
    let additional_immediates_reduced: Vec<WS> = c.additional_immediates
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
        original_reduced: &original_reduced,
        reducer: &reducer,
        tui,
        inputs_outputs: &inputs_outputs,
    };
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
                    if !bank.contains_key(&inp) {
                        init_bank(&mut bank, inp, registers, immediates);
                    }
                }
                // The red code.
                let mut discarded = FxHashSet::<Inst<W>>::default();
                for sub_inputs in inputs
                    .iter()
                    .map(|s| s.sub_states())
                    .multi_cartesian_product()
                {
                    // Find all instructions (and their effects!) that can run from the current states.
                    // Instead of doing this by iterating all instructions, do this by intersection of equivalence
                    // classes that can run from the states.
                    let empty = Default::default();
                    let insts = sub_inputs
                        .iter()
                        .map(|sub_input| {
                            // For this state, return set of commands that can run from it.
                            bank.get(sub_input)
                                .unwrap_or(&empty)
                                .iter()
                                .flat_map(|(_, set)| set.iter().copied())
                                .collect::<FxHashSet<_>>()
                        })
                        .collect::<Vec<_>>()
                        // Intersect!
                        .as_slice()
                        .pipe(intersect_all)
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
                            .map(|s| inst.run_masked(*s))
                            .collect::<Option<Vec<_>>>()
                            .unwrap();
                        // Did we succeed?
                        if outputs == *inputs_outputs.outputs() {
                            let prog = prog.clone().mutate(|p| p.push(inst));
                            match oracle.verify(&prog) {
                                Continue(()) => todo!("what do we do here? '{prog:?}'"),
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
                        for sub_inputs in inputs
                            .iter()
                            .zip(&sub_inputs)
                            .map(|(s, sub_input)| {
                                s.sub_states().filter(|s| sub_input.is_sub_state(s))
                            })
                            .multi_cartesian_product()
                        {
                            for sub_outputs in outputs
                                .iter()
                                .map(|s| s.sub_states())
                                .multi_cartesian_product()
                            {
                                if !sub_inputs.iter().zip(sub_outputs.iter()).all(
                                    |(sub_input, sub_output)| {
                                        bank.get(sub_input)
                                            .expect("we have initialized this at the start")
                                            .contains_key(sub_output)
                                    },
                                ) {
                                    continue;
                                }
                                discarded.extend(intersect_all(
                                    // &inputs
                                    &sub_inputs
                                        .iter()
                                        .zip(sub_outputs.iter())
                                        .map(|(input, sub_output)| {
                                            bank.get(input)
                                                .expect("we have initialized this at the start")
                                                .get(sub_output)
                                                .unwrap()
                                                .clone()
                                        })
                                        .collect::<Vec<_>>(),
                                ));
                            }
                        }
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
struct InputsOutputsCell<W>(RefCell<(Vec<MaskedState<W>>, Vec<MaskedState<W>>)>);

impl<W: Word> InputsOutputsCell<W> {
    pub fn inputs(&self) -> Ref<'_, [MaskedState<W>]> {
        Ref::map(self.0.borrow(), |(inps, _)| inps.as_slice())
    }
    pub fn outputs(&self) -> Ref<'_, [MaskedState<W>]> {
        Ref::map(self.0.borrow(), |(_, outs)| outs.as_slice())
    }
    pub fn push(&self, inp: MaskedState<W>, out: MaskedState<W>) {
        let (inps, outs) = &mut *self.0.borrow_mut();
        inps.push(inp);
        outs.push(out);
    }
    pub fn contains(&self, inp: &MaskedState<W>, out: &MaskedState<W>) -> bool {
        let (inps, outs) = &*self.0.borrow();
        inps.iter().zip(outs).contains(&(inp, out))
    }
}

struct ReducedProgramOracle<'a, WBig: HasBitWord, W: HasBitWord> {
    inputs_outputs: &'a InputsOutputsCell<W>,
    oracle: &'a mut dyn Oracle<[Inst<WBig>], State<WBig>>,
    oracle_reduced: &'a mut dyn Oracle<[Inst<W>], State<W>>,
    original_reduced: &'a [Inst<W>],
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
            verify::Result::CounterExample(inp, _out) => {
                let read_mask = what_program_reads(self.original_reduced.iter().cloned(), &inp);
                let inp = inp.masked(read_mask.into());
                let out = run_program_masked(self.original_reduced.iter().cloned(), inp).expect("the counter example found by the oracle must be runnable and the input mask for the program must be enough for it to run");
                self.tui.found_counter_example(*inp.state(), *out.state());
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
