use smtlib::terms::{Dynamic, STerm, StaticSorted};

use crate::smtlib_utils;
use std::ops::*;

pub type SmtBool<'st> = smtlib::Bool<'st>;

/// A trait for generalizing over both [bool] and [smtlib::Bool], shorthanded as [SmtBool]
pub trait Bool:
    Clone
    + Copy
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Not<Output = Self>
{
    fn r#true() -> Self;
    fn r#false() -> Self;

    fn from_bool(x: bool) -> Self {
        if x { Self::r#true() } else { Self::r#false() }
    }
}

#[rustfmt::skip]
impl Bool for bool {
    fn r#true() -> Self { true }
    fn r#false() -> Self { false }
    fn from_bool(x: bool) -> Self { x }
}

#[rustfmt::skip]
impl<'st> Bool for SmtBool<'st> {
    fn r#true() -> Self { smtlib_utils::static_true() }
    fn r#false() -> Self { smtlib_utils::static_false() }
}

/// A trait for a generic if-then-else for bool [bool] and [smtlib::Bool]
pub trait IfThenElse<T> {
    fn if_then_else(self, a: T, b: T) -> T;
}

impl<T> IfThenElse<T> for bool {
    fn if_then_else(self, a: T, b: T) -> T {
        if self { a } else { b }
    }
}

impl<'st, T> IfThenElse<T> for SmtBool<'st>
where
    T: StaticSorted<'st> + Into<Dynamic<'st>>,
{
    fn if_then_else(self, a: T, b: T) -> T {
        let a: Dynamic<'st> = a.into();
        let b: Dynamic<'st> = b.into();
        T::from(STerm::from(self.ite(a, b)))
    }
}

pub mod prelude {
    pub use super::{Bool, IfThenElse, SmtBool};
}
