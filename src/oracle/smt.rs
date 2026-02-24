use super::Oracle;
use smtlib::{Bool, Model, Solver, Storage, backend::cvc5_binary::Cvc5Binary};
use std::fmt::Debug;
use std::marker::PhantomPinned;
use std::mem::MaybeUninit;
use std::ptr;

pub struct SmtOracle<'st, I: Inst> {
    st: Storage,
    solver: Solver<'st, Cvc5Binary>,
    initial_state: I::StateVars<'st>,
    expected_final_state: I::SymbolicState<'st>,
    // Mark this type as being !Unpin, that is to say, you cannot move this struct.
    _pin: PhantomPinned,
    // When adding more fields, make sure to update the `init` function.
}

impl<'st, I: Inst> SmtOracle<'st, I> {
    fn storage(&self) -> &'st Storage {
        // SAFETY: This struct is pinned (`PhantomPinned`), so `self.st` will never move.
        // `'st` is the lifetime tied to `self.st`, so extending the borrow to `'st` is sound.
        unsafe { &*((&self.st) as *const Storage) }
    }

    pub unsafe fn init(out: &mut MaybeUninit<Self>, target_program: &[I]) {
        unsafe {
            let ptr = out.as_mut_ptr();
            // Need to use pointer writes, so that destructors aren't called on uninitialized
            // memory.
            ptr::write(&mut (*ptr).st, Storage::new());
            let st = &(*ptr).st;
            let initial_state = I::new_state_vars(st, "init");
            let expected_final_state = I::run(target_program, st, initial_state.clone().into());
            ptr::write(&mut (*ptr).solver, new_solver(st));
            ptr::write(&mut (*ptr).initial_state, initial_state);
            ptr::write(&mut (*ptr).expected_final_state, expected_final_state);
            ptr::write(&mut (*ptr)._pin, PhantomPinned);
        }
    }
}

impl<'st, I: Inst> Oracle<[I], I::State> for SmtOracle<'st, I> {
    fn check_program(&mut self, program: &[I]) -> Result<(), super::CounterExample<I::State>> {
        let st = self.storage();
        // Clone these before borrowing self.solver mutably via scope.
        let initial_state = self.initial_state.clone();
        let expected_output = self.expected_final_state.clone();
        let result = self
            .solver
            .scope(|solver| {
                let output = I::run(program, st, initial_state.clone().into());
                // Introduce named output constants so we can extract them from the model.
                let output_vars = I::new_state_vars(st, "output");
                // Assert: output_vars == candidate program's output.
                solver.assert(!I::state_neq(
                    st,
                    output_vars.clone().into(),
                    output.clone(),
                ))?;
                // Assert: candidate output != target output (look for a counter-example).
                solver.assert(I::state_neq(st, output, expected_output))?;
                match solver.check_sat_with_model()? {
                    smtlib::SatResultWithModel::Unsat => Ok(None),
                    smtlib::SatResultWithModel::Sat(model) => {
                        let input = I::extract_from_model(st, &model, initial_state);
                        let output = I::extract_from_model(st, &model, output_vars);
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
        .expect("failed to initialize cvc5 - binary 'cvc5' not found on path");
    let mut solver = Solver::new(st, cvc5).expect("failed to initialize solver");
    solver
        .set_logic(smtlib::Logic::Custom("ALL".into()))
        .expect("failed to set logic");
    solver
}

pub trait Inst: Sized {
    type State: Debug;
    /// A representation of the state as SMT constants.
    type StateVars<'st>: Clone + Debug + Into<Self::SymbolicState<'st>> + 'st;
    /// A symbolic representation of the state.
    type SymbolicState<'st>: Clone + Debug + 'st;

    fn new_state_vars<'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st>;

    fn state_neq<'st>(
        st: &'st Storage,
        s1: Self::SymbolicState<'st>,
        s2: Self::SymbolicState<'st>,
    ) -> Bool<'st>;

    fn step<'st>(&self, st: &'st Storage, s: Self::SymbolicState<'st>) -> Self::SymbolicState<'st>;

    fn extract_from_model<'st>(
        st: &'st Storage,
        model: &Model,
        s: Self::StateVars<'st>,
    ) -> Self::State;

    // -- default implementations --

    fn run<'st>(
        program: &[Self],
        st: &'st Storage,
        s: Self::SymbolicState<'st>,
    ) -> Self::SymbolicState<'st> {
        program.iter().fold(s, |s, inst| inst.step(st, s))
    }
}

#[cfg(test)]
mod tests {
    use super::super::CounterExample;
    use super::*;
    use smtlib::{
        Int, Sorted,
        terms::{Const, IntoWithStorage, STerm, StaticSorted},
    };

    /// Extract a concrete i64 from a model-evaluated `Int` term.
    /// Handles plain numerals (positive) and `(- n)` (negative).
    fn int_term_to_i64(int: Int<'_>) -> i64 {
        use smtlib::lowlevel::ast::{Identifier, QualIdentifier, SpecConstant, Term};
        match STerm::from(int).term() {
            Term::SpecConstant(SpecConstant::Numeral(n)) => n.into_u128().unwrap() as i64,
            Term::Application(QualIdentifier::Identifier(Identifier::Simple(sym)), args)
                if sym.0 == "-" && args.len() == 1 =>
            {
                match args[0] {
                    Term::SpecConstant(SpecConstant::Numeral(n)) => {
                        -(n.into_u128().unwrap() as i64)
                    }
                    _ => panic!("unexpected negation argument in model: {:?}", args[0]),
                }
            }
            term => panic!("unexpected integer term in model: {:?}", term),
        }
    }

    const N: usize = 10;

    type Var = usize;

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

    impl Inst for I {
        type State = [i64; N];

        type StateVars<'st> = StateVars<'st>;

        type SymbolicState<'st> = [Int<'st>; N];

        fn new_state_vars<'a, 'st>(st: &'st Storage, name: &str) -> Self::StateVars<'st> {
            let vars = std::array::from_fn(|i| Int::new_const(st, &format!("{}_v{}", name, i)));
            StateVars { vars }
        }

        fn state_neq<'st>(
            st: &'st Storage,
            s1: Self::SymbolicState<'st>,
            s2: Self::SymbolicState<'st>,
        ) -> Bool<'st> {
            let mut eq = false.into_with_storage(st);
            for i in 0..N {
                eq |= s1[i]._neq(s2[i]);
            }
            eq
        }

        fn step<'st>(
            &self,
            _st: &'st Storage,
            s: Self::SymbolicState<'st>,
        ) -> Self::SymbolicState<'st> {
            match self {
                I::Add(x, y) => {
                    let mut new_state = s;
                    new_state[*x] += new_state[*y];
                    new_state
                }
            }
        }

        fn extract_from_model<'st>(
            _st: &'st Storage,
            model: &Model,
            s: Self::StateVars<'st>,
        ) -> Self::State {
            std::array::from_fn(|i| {
                let int = model.eval(s.vars[i]).expect("variable not found in model");
                int_term_to_i64(int)
            })
        }
    }

    fn test_equivalence(p1: &[I], p2: &[I]) -> Result<(), CounterExample<[i64; N]>> {
        let mut oracle = unsafe {
            let mut oracle = MaybeUninit::uninit();
            SmtOracle::init(&mut oracle, p1);
            oracle.assume_init()
        };
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
}
