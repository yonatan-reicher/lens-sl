use crate::some_traits::{NewConst, Run};

use super::Oracle;

use std::pin::Pin;

use smtlib::backend::cvc5_binary::Cvc5Binary;
use smtlib::{Solver, Storage};

pub struct SmtOracle<'st, P, S: NewConst<'st>> {
    _st: Pin<Box<Storage>>,
    solver: Solver<'st, Cvc5Binary>,
    initial_state: S::Const,
    expected_final_state: S,
    target_program: P,
}

impl<'st, P, S> SmtOracle<'st, P, S>
where
    S: NewConst<'st>,
    S::Const: Clone,
    P: Run<S>,
{
    pub fn new(target_program: P) -> Self {
        let st = Box::pin(Storage::new());
        let st_ref: &'st Storage = unsafe { &*(st.as_ref().get_ref() as *const Storage) };
        let initial_state = S::new_const(st_ref, "init");
        let mut expected_final_state = initial_state.clone().into();
        target_program.run(&mut expected_final_state);
        let solver = new_solver(st_ref);
        Self {
            _st: st,
            initial_state,
            expected_final_state,
            solver,
            target_program,
        }
    }
}

impl<'st, I, S, ConcreteState> Oracle<[I], ConcreteState> for SmtOracle<'st, Vec<I>, S>
where S: Clone + NewConst<'st>, S::Const: Clone, I: Run<S>, I: Run<ConcreteState>
{
    fn check_program(&mut self, program: &[I]) -> Result<(), super::CounterExample<ConcreteState>> {
        // Clone these before borrowing self.solver mutably via scope.
        let initial_state = self.initial_state.clone();
        let expected_output = self.expected_final_state.clone();
        let result = self
            .solver
            .scope(|solver| {
                let mut output = initial_state.clone().into();
                program.run(&mut output);
                // Assert: candidate output != target output (look for a counter-example).
                let f = I::state_neq(output, expected_output);
                solver.assert(f)?;
                match solver.check_sat_with_model()? {
                    smtlib::SatResultWithModel::Unsat => Ok(None),
                    smtlib::SatResultWithModel::Sat(model) => {
                        let input = I::extract_from_model(&model, initial_state);
                        let mut output = input.clone();
                        self.target_program.run(&mut output);
                        Ok(Some((input, output)))
                    }
                    smtlib::SatResultWithModel::Unknown => panic!("solver returned unknown"),
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
        .expect("failed to initialize cvc5 - binary 'cvc5' not found on path");
    let mut solver = Solver::new(st, cvc5).expect("failed to initialize solver");
    solver
        .set_logic(smtlib::Logic::Custom("ALL".into()))
        .expect("failed to set logic");
    // solver.set_timeout(1000).expect("failed to set solver timeout");
    solver
}

// pub trait Inst: Sized {
//     type State: Clone + Debug;
//     /// A representation of the state as SMT constants.
//     type StateVars<'st>: Clone + Debug + Into<Self::SymbolicState<'st>> + 'st;
//     /// A symbolic representation of the state.
//     type SymbolicState<'st>: Clone + Debug + 'st;
//
//     fn new_state_vars<'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st>;
//
//     fn state_neq<'st>(s1: Self::SymbolicState<'st>, s2: Self::SymbolicState<'st>) -> Bool<'st>;
//
//     fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>);
//
//     fn step(&self, s: &mut Self::State);
//
//     fn extract_from_model<'st>(model: &Model<'st>, s: Self::StateVars<'st>) -> Self::State;
//
//     // -- default implementations --
//
//     fn run_symbolic<'st>(program: &[Self], s: &mut Self::SymbolicState<'st>) {
//         program.iter().for_each(|inst| inst.step_symbolic(s))
//     }
//
//     fn run(program: &[Self], s: &mut Self::State) {
//         program.iter().for_each(|inst| inst.step(s))
//     }
// }

#[cfg(test)]
mod tests {
    use super::super::CounterExample;
    use super::*;
    use crate::smtlib_utils::int_term_to_i128;
    use smtlib::{
        Int, Sorted, Model,
    };
    use smtlib::terms::{Const, IntoWithStorage, StaticSorted};

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

    // impl Inst for I {
    //     type State = [i64; N];

    //     type StateVars<'st> = StateVars<'st>;

    //     type SymbolicState<'st> = [Int<'st>; N];

    //     fn new_state_vars<'a, 'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st> {
    //         let vars = std::array::from_fn(|i| Int::new_const(st, &format!("{}_v{}", name, i)));
    //         StateVars { vars }
    //     }

    //     fn state_neq<'st>(s1: Self::SymbolicState<'st>, s2: Self::SymbolicState<'st>) -> Bool<'st> {
    //         let st = s1[0].st();
    //         let mut eq = false.into_with_storage(st);
    //         for i in 0..N {
    //             eq |= s1[i]._neq(s2[i]);
    //         }
    //         eq
    //     }

    //     fn step_symbolic<'st>(&self, s: &mut Self::SymbolicState<'st>) {
    //         match self {
    //             I::Add(x, y) => {
    //                 s[*x] += s[*y];
    //             }
    //         }
    //     }

    //     fn step(&self, s: &mut Self::State) {
    //         match self {
    //             I::Add(x, y) => s[*x] += s[*y],
    //         }
    //     }

    //     fn extract_from_model<'st>(model: &Model, s: Self::StateVars<'st>) -> Self::State {
    //         std::array::from_fn(|i| {
    //             let int = model.eval(s.vars[i]).expect("variable not found in model");
    //             int_term_to_i128(int).unwrap() as i64
    //         })
    //     }
    // }

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
