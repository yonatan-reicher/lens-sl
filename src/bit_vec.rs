use crate::some_traits::*;

use derive_more::{Display, Into};
use smtlib::terms::{Const, StaticSorted};
use smtlib::{Sorted, Storage};

use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::*;

pub trait BitVec :
    Sized
    + Copy
    // Arithmetic
    + Add<Output=Self>
    // + Sub<Output=Self> // for some reason, this is not implemented for [`smtlib::BitVec`]
    + Mul<Output=Self>
    + Div<Output=Self>
    + Neg<Output=Self>
    // Bitwise
    + BitAnd<Output=Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    // Logical
    + BoolEq<Self::Bool>
    // Other
    + Debug
    + Display
    + BitWidth
{
    type Bool: Bool + IfThenElse<Self> + IfThenElse<Self::Bool>;
    type FromContext: Clone + Copy;

    fn sub(self, other: Self) -> Self;
    fn unsigned_lt(self, other: Self) -> Self::Bool;
    fn unsigned_le(self, other: Self) -> Self::Bool;
    fn signed_lt(self, other: Self) -> Self::Bool;
    fn signed_le(self, other: Self) -> Self::Bool;
    fn is_zero(self) -> Self::Bool;
    fn is_negative(self) -> Self::Bool;

    fn from_i64(i: i64, c: Self::FromContext) -> Self;

    fn get_from_context(self) -> Self::FromContext;
}

pub trait ConcreteeeBitVec:
    BitVec<Bool = bool, FromContext = ()>
    + Default
    + Eq
    + From<i64>
    + Into<i64>
    + From<u64>
    + Into<u64>
    + Hash
{
    fn from_concrete_bit_vec(other: impl ConcreteeeBitVec) -> Self {
        Self::from(Into::<i64>::into(other))
    }
    fn into_concrete_bit_vec<C: ConcreteeeBitVec>(self) -> C {
        C::from_concrete_bit_vec(self)
    }
}

pub trait SmtBitVec<'st>:
    BitVec<Bool = smtlib::Bool<'st>, FromContext = &'st Storage>
    + Sorted<'st>
    + StaticSorted<'st>
    + From<Const<'st, Self>>
{
}

// Implement for `smtlib`.
#[rustfmt::skip]
impl<'st, const N: usize> BitVec for smtlib::BitVec<'st, N> {
    type Bool = smtlib::Bool<'st>;
    fn sub(self, other: Self) -> Self { self + -other }
    fn unsigned_lt(self, other: Self) -> smtlib::Bool<'st> { self.bvult(other) }
    fn unsigned_le(self, other: Self) -> smtlib::Bool<'st> { self.bvule(other) }
    fn signed_lt(self, other: Self) -> smtlib::Bool<'st> { self.bvslt(other) }
    fn signed_le(self, other: Self) -> smtlib::Bool<'st> { self.bvsle(other) }
    type FromContext = &'st Storage;
    fn from_i64(i: i64, st: &'st Storage) -> Self { Self::new(st, i) }
    fn get_from_context(self) -> &'st Storage { self.st() }
    fn is_zero(self) -> Self::Bool { self.eq(&Self::new(self.get_from_context(), 0)) }
    fn is_negative(self) -> Self::Bool { self.signed_lt(Self::new(self.get_from_context(), 0)) }
}

// Implement for a new type!

/// A concrete bit-vector that stores inside an unsigned int. Operations are unsigned by default.
#[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, Hash)]
#[display("{_0}")]
pub struct ConcreteBitVec<const N: usize>(u64);

impl<const N: usize> From<u64> for ConcreteBitVec<N> {
    fn from(i: u64) -> Self {
        Self(i & mask::<N>())
    }
}

impl<const N: usize> From<ConcreteBitVec<N>> for u64 {
    fn from(bv: ConcreteBitVec<N>) -> Self {
        bv.0
    }
}

impl<const N: usize> From<i64> for ConcreteBitVec<N> {
    fn from(i: i64) -> Self {
        if i >= 0 {
            Self::from(i as u64)
        } else {
            -Self::from_i64(-i, ())
        }
    }
}

impl<const N: usize> From<ConcreteBitVec<N>> for i64 {
    fn from(bv: ConcreteBitVec<N>) -> i64 {
        unsafe { std::mem::transmute(bv.0) }
    }
}

#[rustfmt::skip]
impl<const N: usize> BitVec for ConcreteBitVec<N> {
    type Bool = bool;
    fn sub(self, other: Self) -> Self { self - other }
    fn unsigned_lt(self, other: Self) -> bool { self.0 < other.0 }
    fn unsigned_le(self, other: Self) -> bool { self.0 <= other.0 }
    fn signed_lt(self, other: Self) -> bool { i64::from(self) < i64::from(other) }
    fn signed_le(self, other: Self) -> bool { i64::from(self) <= i64::from(other) }
    type FromContext = ();
    fn from_i64(i: i64, _: ()) -> Self { Self::from(i) }
    fn get_from_context(self) -> () {}
    fn is_zero(self) -> Self::Bool { self.0 == 0 }
    fn is_negative(self) -> Self::Bool { i64::from(self) < 0 }
}

impl<const N: usize> ConcreteeeBitVec for ConcreteBitVec<N> {}

impl<const N: usize> BitWidth for ConcreteBitVec<N> {
    const BIT_WIDTH: usize = N;
}

macro_rules! impl_ops {
    ($trait:ident $fn:ident $a:ident => $body:expr, $($rest:tt)*) => {
        impl<const N: usize> $trait for ConcreteBitVec<N> {
            type Output = Self;
            fn $fn(self) -> Self {
                let $a = self.0;
                Self($body)
            }
        }

        impl_ops!($($rest)*);
    };
    ($trait:ident $fn:ident $a:ident $b:ident => $body:expr, $($rest:tt)*) => {
        impl<const N: usize> $trait for ConcreteBitVec<N> {
            type Output = Self;
            fn $fn(self, other: Self) -> Self {
                let $a = self.0;
                let $b = other.0;
                Self($body)
            }
        }

        impl_ops!($($rest)*);
    };
    () => ();
}

fn mask<const N: usize>() -> u64 {
    (1u64 << N) - 1
}

impl_ops![
    Add add a b => a.wrapping_add(b) & mask::<N>(),
    Sub sub a b => a.wrapping_sub(b) & mask::<N>(),
    Mul mul a b => a.wrapping_mul(b) & mask::<N>(),
    Div div a b => a.wrapping_div(b) & mask::<N>(),
    BitAnd bitand a b => a & b,
    BitOr bitor a b =>   a | b,
    BitXor bitxor a b => (a ^ b) & mask::<N>(),
    Not not a => (a ^ 0) & mask::<N>(),
    Neg neg a => ((a ^ 0) & mask::<N>()).wrapping_add(1) & mask::<N>(), // -x = flip(x) + 1
];
