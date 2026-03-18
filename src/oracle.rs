pub type CounterExample<S> = (S, S);

pub trait Oracle<P, S>
where
    P: ?Sized,
{
    fn check_program(&mut self, program: &P) -> Result<(), CounterExample<S>>;
}

#[allow(dead_code)]
pub mod test_cases;
pub mod smt;

pub use smt::SmtOracle;
#[allow(unused_imports)]
pub use test_cases::TestCasesOracle;
