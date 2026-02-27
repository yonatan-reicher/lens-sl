use smtlib::Sorted;
use std::ops::*;

use crate::smtlib_utils;

// === Run Trait ===

/// A thing that can run, modifying some state.
pub trait Run<State> {
    fn run(&self, state: &mut State);

    /// A version of [`Run::run`] that returns a new State, for pipeline style code.
    fn run_pipe(&self, mut state: State) -> State {
        self.run(&mut state);
        state
    }
}

// Run a series of things.
impl<T, S> Run<S> for T
where
    for<'a> &'a T: IntoIterator<Item: Run<S>>,
{
    fn run(&self, state: &mut S) {
        self.into_iter().for_each(|x| x.run(state));
    }
}

// === Bool Trait ===

/// This trait let's us be generic over both [bool] and [smtlib::Bool]
pub trait Bool :
    Clone
    + Copy
    + Sized
    // Operators
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Not<Output = Self>
    + BoolEq<Self>
{
    fn r#true() -> Self;
    fn r#false() -> Self;

    fn from(b: bool) -> Self {
        if b { Self::r#true() } else { Self::r#false() }
    }
}

pub trait IfThenElse<T>: Bool {
    fn if_then_else(self, a: T, b: T) -> T;
}

impl<T> IfThenElse<T> for bool {
    fn if_then_else(self, a: T, b: T) -> T {
        if self { a } else { b }
    }
}

impl<'st, T: smtlib::terms::StaticSorted<'st> + Into<smtlib::terms::Dynamic<'st>>> IfThenElse<T>
    for smtlib::Bool<'st>
{
    fn if_then_else(self, a: T, b: T) -> T {
        use smtlib::terms::{Dynamic, STerm};
        let a: Dynamic<'st> = a.into();
        let b: Dynamic<'st> = b.into();
        T::from(STerm::from(self.ite(a, b)))
    }
}

/// An equality trait that's generic over the boolean.
pub trait BoolEq<B: Bool> {
    fn eq(&self, other: &Self) -> B;
    fn neq(&self, other: &Self) -> B {
        !self.eq(other)
    }
}

#[rustfmt::skip]
impl Bool for bool {
    fn r#true() -> bool { true }
    fn r#false() -> bool { false }
    fn from(b: bool) -> bool { b }
}

#[rustfmt::skip]
impl<T> BoolEq<bool> for T where T: Eq {
    fn eq(&self, other: &T) -> bool { self == other }
    fn neq(&self, other: &T) -> bool { self != other }
}

#[rustfmt::skip]
impl<'st> Bool for smtlib::Bool<'st> {
    fn r#true() -> Self { smtlib_utils::static_true() }
    fn r#false() -> Self { smtlib_utils::static_false() }
}

#[rustfmt::skip]
impl<'st, T> BoolEq<smtlib::Bool<'st>> for T
where T: Copy + Sorted<'st> + smtlib::terms::IntoWithStorage<'st, T::Inner>
{
    fn eq(&self, other: &Self) -> smtlib::Bool<'st> { self._eq(*other) }
    fn neq(&self, other: &Self) -> smtlib::Bool<'st> { self._neq(*other) }
}

// === CloneTo trait ===

/// A more efficient implementation for cloning when overriding an existing value, allowing us to
/// reuse allocations.
pub trait CloneTo {
    fn clone_to(&self, output: &mut Self);
}

// === BitWidth trait ===

pub trait BitWidth {
    const BIT_WIDTH: usize;
}

impl BitWidth for i64 {
    const BIT_WIDTH: usize = 64;
}
impl<const N: usize> BitWidth for smtlib::BitVec<'_, N> {
    const BIT_WIDTH: usize = N;
}

// === More things ===

pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> Len for [T] {
    fn len(&self) -> usize {
        self.as_ref().len()
    }
}

impl<T> Len for Vec<T> {
    fn len(&self) -> usize {
        self.len()
    }
}

impl<T: Len> Len for &T {
    fn len(&self) -> usize {
        T::len(self)
    }
}

pub trait Append {
    fn append(&mut self, other: Self);
}

/// A trait for a type that can be constructed
pub trait NewConst<'st>: From<Self::Const> {
    type Const;
    fn new_const(st: &'st smtlib::Storage, name: &str) -> Self::Const;
}
