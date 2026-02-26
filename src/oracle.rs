pub type CounterExample<S> = (S, S);

/// In the future, we will have a solver implement this trait.
pub trait Oracle<P, S> where P: ?Sized {
    fn check_program(&mut self, program: &P) -> Result<(), CounterExample<S>>;
}

#[allow(dead_code)]
pub mod test_cases;
pub mod smt;

#[allow(unused_imports)]
pub use test_cases::TestCasesOracle;
pub use smt::SmtOracle;
