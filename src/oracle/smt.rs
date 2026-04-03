use super::Oracle;

use std::fmt::Debug;
use std::pin::Pin;

use smtlib::backend::cvc5_binary::Cvc5Binary;
use smtlib::{Bool, Model, SatResultWithModel, Solver, Storage};

pub struct SmtOracle<'st, I: Inst<S>, S> {
    _st: Pin<Box<Storage>>,
    solver: Solver<'st, Cvc5Binary>,
    initial_state: I::StateVars<'st>,
    expected_final_state: I::SymbolicState<'st>,
    target_program: Vec<I>,
    _marker: std::marker::PhantomData<*const S>,
}

impl<'st, I: Inst<S>, S> SmtOracle<'st, I, S> {
    pub fn new(target_program: Vec<I>) -> Self {
        let st = Box::pin(Storage::new());
        let st_ref: &'st Storage = unsafe { &*(st.as_ref().get_ref() as *const Storage) };
        let initial_state = I::new_state_vars(st_ref, "init");
        let mut expected_final_state = initial_state.clone().into();
        I::run_symbolic(&target_program, &mut expected_final_state);
        let solver = new_solver(st_ref);
        Self {
            _st: st,
            initial_state,
            expected_final_state,
            solver,
            target_program,
            _marker: Default::default(),
        }
    }
}

impl<'st, I: Inst<S>, S: Clone> Oracle<[I], S> for SmtOracle<'st, I, S> {
    fn check_program(&mut self, program: &[I]) -> Result<(), super::CounterExample<S>> {
        // Clone these before borrowing self.solver mutably via scope.
        let initial_state = self.initial_state.clone();
        let expected_output = self.expected_final_state.clone();
        // Open a solver scope to check the program in. This makes sure that everything we do in
        // this function doesn't affect the next calls to this function.
        let result = self
            .solver
            .scope(|solver| {
                let mut output = initial_state.clone().into();
                I::run_symbolic(program, &mut output);
                // Assert: candidate output != target output (look for a counter-example).
                solver.assert(I::state_neq(output, expected_output))?;
                match solver.check_sat_with_model()? {
                    SatResultWithModel::Unsat => Ok(None),
                    SatResultWithModel::Sat(model) => {
                        let input = I::extract_from_model(&model, initial_state);
                        let mut output = input.clone();
                        I::run(&self.target_program, &mut output);
                        Ok(Some((input, output)))
                    }
                    SatResultWithModel::Unknown => panic!("solver returned unknown"),
                }
            })
            .expect("solver error");
        match result {
            None => Ok(()),
            Some(counter_example) => Err(counter_example),
        }
    }
}

fn new_solver<'st>(st: &'st Storage) -> Solver<'st, Cvc5Binary> {
    let cvc5 = Cvc5Binary::new("cvc5")
        .or_else(|_| Cvc5Binary::new("./cvc5"))
        .unwrap_or_else(|e| {
            eprintln!(
                "Error: cvc5 executable not found. \n\
                 You must have cvc5 installed with executable permissions either on your path, or \
                 in the current working directory of this binary. You can download cvc5 on \
                 https://github.com/cvc5/cvc5/releases/. \n\
                 OS Error: {e}"
            );
            std::process::exit(1);
        });
    let mut solver = Solver::new(st, cvc5).expect("failed to initialize solver");
    solver
        .set_logic(smtlib::Logic::Custom("ALL".into()))
        .expect("failed to set logic");
    // solver.set_timeout(1000).expect("failed to set solver timeout");
    solver
}

pub trait Inst<State>: Sized + Debug {
    /// A representation of the state as SMT constants.
    type StateVars<'st>: Clone + Debug + Into<Self::SymbolicState<'st>> + 'st;
    /// A symbolic representation of the state.
    type SymbolicState<'st>: Clone + Debug + 'st;

    fn new_state_vars<'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st>;

    fn state_neq<'st>(s1: Self::SymbolicState<'st>, s2: Self::SymbolicState<'st>) -> Bool<'st>;

    fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>);

    fn step(&self, s: &mut State);

    fn extract_from_model<'st>(model: &Model<'st>, s: Self::StateVars<'st>) -> State;

    // -- default implementations --

    fn run_symbolic<'st>(program: &[Self], s: &mut Self::SymbolicState<'st>) {
        program.iter().for_each(|inst| inst.step_symbolic(s))
    }

    fn run(program: &[Self], s: &mut State) {
        program.iter().for_each(|inst| inst.step(s))
    }
}

// =================================================================================================
//                                   Inst Trait Implementation
// =================================================================================================

use crate::arm::{self, Register, what_program_reads, run_program_masked};
use crate::arm::state::{State, Masked as MaskedState, StateVars, SymbolicState, Flags};
use crate::smtlib_utils::bool_term_to_bool;
use crate::word::prelude::*;

impl<W: Word + HasBitWord> Inst<State<W>> for arm::Inst<W> {
    type StateVars<'st> = StateVars<'st, W::SmtWord<'st>>;

    type SymbolicState<'st> = SymbolicState<'st, W::SmtWord<'st>>;

    fn new_state_vars<'st>(st: &'st smtlib::Storage, name: &str) -> Self::StateVars<'st> {
        StateVars::new(st, name)
    }

    fn state_neq<'st>(
        s1: Self::SymbolicState<'st>,
        s2: Self::SymbolicState<'st>,
    ) -> smtlib::Bool<'st> {
        !s1.eq(s2)
    }

    fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>) {
        self.run_symbolic(s);
    }

    fn step<'st>(&self, s: &mut State<W>) {
        self.run(s);
    }

    fn extract_from_model<'st>(
        model: &smtlib::Model<'st>,
        s: StateVars<'st, W::SmtWord<'st>>,
    ) -> State<W> {
        // == Registers ==
        let mut state = State::default();
        for (i, var) in s.registers.iter().enumerate() {
            let reg = Register(i as u8);
            let val = model
                .eval(*var)
                .map(W::SmtWord::try_into_word)
                .unwrap_or_else(|| Some(0.into()))
                //.try_into()
                .unwrap_or_else(|| {
                    panic!(
                        "Failed to convert variable '{var:?}' to the right type in model {model}."
                    )
                });
            state.set_register(
                reg,
                val.into_word(), /* This is actually the same word type but whatever */
            );
        }
        // == Flags ==
        let load_bool = |b| {
            model
                .eval(b)
                .and_then(|b| bool_term_to_bool(b))
                .unwrap_or(false /* Arbitrary default, result did not matter */)
        };
        state.set_flags(
            Flags {
                z: load_bool(s.flags.z),
                n: load_bool(s.flags.n),
                c: load_bool(s.flags.c),
                v: load_bool(s.flags.v),
            }
            .into(),
        );
        state
    }
}

// =================================================================================================
//                                             Tests
// =================================================================================================

#[cfg(test)]
mod tests {
    use super::super::CounterExample;
    use super::*;
    use crate::smtlib_utils::int_term_to_i128;
    use smtlib::{
        Int, Sorted,
        terms::{Const, IntoWithStorage, StaticSorted},
    };

    const N: usize = 10;

    type Var = usize;

    #[derive(Clone, Debug)]
    enum I {
        Add(Var, Var),
    }

    #[derive(Clone, Debug)]
    struct StateVars<'st> {
        vars: [Const<'st, Int<'st>>; N],
    }

    impl<'st> From<StateVars<'st>> for [Int<'st>; N] {
        fn from(val: StateVars<'st>) -> Self {
            val.vars.map(|c| c.into())
        }
    }

    impl Inst<[i64; N]> for I {
        type StateVars<'st> = StateVars<'st>;

        type SymbolicState<'st> = [Int<'st>; N];

        fn new_state_vars<'a, 'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st> {
            let vars = std::array::from_fn(|i| Int::new_const(st, &format!("{}_v{}", name, i)));
            StateVars { vars }
        }

        fn state_neq<'st>(s1: Self::SymbolicState<'st>, s2: Self::SymbolicState<'st>) -> Bool<'st> {
            let st = s1[0].st();
            let mut eq = false.into_with_storage(st);
            for i in 0..N {
                eq |= s1[i]._neq(s2[i]);
            }
            eq
        }

        fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>) {
            match self {
                I::Add(x, y) => {
                    s[*x] += s[*y];
                }
            }
        }

        fn step(&self, s: &mut [i64; N]) {
            match self {
                I::Add(x, y) => s[*x] += s[*y],
            }
        }

        fn extract_from_model<'st>(model: &Model, s: Self::StateVars<'st>) -> [i64; N] {
            std::array::from_fn(|i| {
                let int = model.eval(s.vars[i]).expect("variable not found in model");
                int_term_to_i128(int).unwrap() as i64
            })
        }
    }

    #[allow(clippy::result_large_err)]
    fn test_equivalence(p1: &[I], p2: &[I]) -> Result<(), CounterExample<[i64; N]>> {
        let mut oracle = SmtOracle::new(p1.to_vec());
        oracle.check_program(p2)
    }

    #[test]
    fn test_empty_equiv() {
        let p1 = &[];
        let p2 = &[];
        assert!(test_equivalence(p1, p2).is_ok());
    }

    #[test]
    fn test_non_equive_non_empty() {
        use I::*;
        let p1 = &[Add(0, 1), Add(0, 1), Add(2, 0)];
        let p2 = &[Add(0, 1), Add(0, 1), Add(2, 1), Add(2, 1)];
        assert!(test_equivalence(p1, p2).is_err());
    }

    // #[test]
    // fn smt_oracle_cannot_be_moved() {
    //     let mut oracle = MaybeUninit::uninit();
    //     let oracle1 = unsafe { SmtOracle::<I>::init(&mut oracle, &[]) }

    //     let _v = vec![oracle];
    //     let _v = vec![oracle1];
    // }
}
