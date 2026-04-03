use serde::de::DeserializeOwned;

use crate::arm::{Flags, Inst, Register, State, state, what_program_reads};
use crate::oracle::Oracle;
use crate::word::prelude::*;
use std::ops::ControlFlow::{self, Break, Continue};

pub fn optimize<C: Config, T>(
    config: C,
    for_each: impl FnMut(Vec<Inst<C::WSmall>>) -> ControlFlow<T>,
) -> ControlFlow<T> {
    Optimizer { c: config }.run(for_each)
}

pub trait Config {
    // Number types
    type WBig: Word + HasBitWord;
    // Other types
    type Graph;
    type State;
    type WSmall: Word + HasBitWord<BitWord: DeserializeOwned> + DeserializeOwned;
    // Getters
    fn immediates(&self) -> impl Iterator<Item = Self::WBig>;
    fn regs(&self) -> impl Iterator<Item = Register>;
    fn oracle(&self) -> &mut impl Oracle<[Inst<Self::WBig>], Self::State>;
    fn oracle_reduced(&mut self) -> &mut impl Oracle<[Inst<Self::WSmall>], Self::State>;
    // Options
    fn should_stop(&self) -> bool;
}

#[derive(Debug)]
struct Optimizer<C: Config> {
    c: C,
}

/// Short-hand
type W<C> = <C as Config>::WSmall;

impl<C: Config> Optimizer<C> {
    fn run<T>(mut self, mut f: impl FnMut(Vec<Inst<W<C>>>) -> ControlFlow<T>) -> ControlFlow<T> {
        loop {
            // --- Searching Phase ---
            self.search(&mut f)?;
            if self.c.should_stop() {
                return Continue(());
            }
            // --- Expanding Phase ---
            self.expand();
        }
    }

    fn search<T>(
        &mut self,
        f: &mut impl FnMut(Vec<Inst<W<C>>>) -> ControlFlow<T>,
    ) -> ControlFlow<T> {
        self.verify(&[], f)
    }

    fn expand(&mut self) {
        todo!()
    }

    /// The 'Verify' procedure from Lens. Takes a reduced program, checks if it is equivalent to the
    /// reduced program, if not, it adds a counter example. If it is equivalent, tries to produce an
    /// equivalent unreduced program.
    fn verify<T>(
        &mut self,
        prog: &[Inst<W<C>>],
        f: &mut impl FnMut(Vec<Inst<W<C>>>) -> ControlFlow<T>,
    ) -> ControlFlow<T> {
        match self.c.oracle_reduced().check_program(prog) {
            Ok(()) => todo!(),
            Err((inp, _out)) => {
                let read_mask = what_program_reads(globals.original_reduced.iter().cloned(), &inp);
                let inp = inp.masked(read_mask.into());
                let out = run_program_masked(globals.original_reduced.iter().cloned(), inp).expect("the counter example found by the oracle must be runnable and the input mask for the program must be enough for it to run");
                globals.tui.found_counter_example(inp, out);
                debug_assert!(
                    !has_counter_example_been_seen(globals, &inp, &out),
                    "Counter-example from reduced oracle should not have been seen before."
                );
                globals.inputs.push(inp);
                globals.outputs.push(out);
                Break(ProgramOrRetry::Retry) 
            },
        }
    }
}


