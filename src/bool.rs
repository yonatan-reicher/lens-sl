use crate::smtlib_utils;

use smtlib::Sorted;
use smtlib::terms::{Dynamic, IntoWithStorage, STerm, StaticSorted};

use std::ops::*;

pub type SmtBool<'st> = smtlib::Bool<'st>;

// ==========================================================================================
//                                        Base Bool Trait
// ==========================================================================================

/// A trait for generalizing over both [bool] and [smtlib::Bool], shorthanded as [SmtBool]
pub trait Bool:
    Clone
    + Copy
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Not<Output = Self>
    + IfThenElse<Self>
    + BoolEq<Self>
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

// ==========================================================================================
//                                         If Then Else
// ==========================================================================================

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

// ==========================================================================================
//                                        Boolean Equality
// ==========================================================================================

#[rustfmt::skip]
pub trait BoolEq<B: Bool> {
    fn eq(&self, other: &Self) -> B;
    fn neq(&self, other: &Self) -> B { !self.eq(other) }
}

#[rustfmt::skip]
impl<T: Eq> BoolEq<bool> for T {
    fn eq(&self, other: &T) -> bool { self == other }
    fn neq(&self, other: &T) -> bool { self != other }
}

#[rustfmt::skip]
impl<'st, T> BoolEq<SmtBool<'st>> for T
where T: Copy + StaticSorted<'st> + IntoWithStorage<'st, T::Inner>
{
    fn eq(&self, other: &T) -> SmtBool<'st> { self._eq(*other) }
    fn neq(&self, other: &T) -> SmtBool<'st> { self._neq(*other) }
}

pub mod prelude {
    pub use super::{Bool, BoolEq, IfThenElse, SmtBool};
}
