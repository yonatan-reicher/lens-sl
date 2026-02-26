use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::*;

use arbitrary_int::traits::{Integer, SignedInteger, UnsignedInteger};
use arbitrary_int::{i4, u4};

use crate::smtlib_utils::bit_vec_term_to_i128;
use smtlib::terms::{IntoWithStorage, StaticSorted};
use smtlib::{BitVec, Storage};

pub trait Word:
    Sized + Clone + Copy + Debug + Default + PartialEq + Eq + PartialOrd + Ord + Hash + 'static
{
    type Signed: Sized + Debug + Default + Display + Hash + SignedInteger + WordOps + 'static;
    type Unsigned: Sized + Debug + Default + Display + Hash + UnsignedInteger + WordOps + 'static;

    type SymbolicBitVec<'st>: Add<Output = Self::SymbolicBitVec<'st>>
        + BitAnd<Output = Self::SymbolicBitVec<'st>>
        + BitOr<Output = Self::SymbolicBitVec<'st>>
        + BitXor<Output = Self::SymbolicBitVec<'st>>
        + Clone
        + Copy
        + Debug
        + Div<Output = Self::SymbolicBitVec<'st>>
        + From<smtlib::terms::Const<'st, Self::SymbolicBitVec<'st>>>
        + IntoWithStorage<'st, Self::SymbolicBitVec<'st>>
        + Mul<Output = Self::SymbolicBitVec<'st>>
        + Neg<Output = Self::SymbolicBitVec<'st>>
        + Not<Output = Self::SymbolicBitVec<'st>>
        + Rem<Output = Self::SymbolicBitVec<'st>>
        + Shl<Output = Self::SymbolicBitVec<'st>>
        + Shr<Output = Self::SymbolicBitVec<'st>>
        + StaticSorted<'st, Inner = Self::SymbolicBitVec<'st>>
        // + TryInto<i64, Error: Debug> // This TryInto implementation is currently broken, so we use our own.
        // + Sub<Output = Self::SymbolicBitVec<'st>> // For some reason, this is unimplemented.
        + 'st;

    fn new_bit_vec<'st>(st: &'st Storage, value: Self::Unsigned) -> Self::SymbolicBitVec<'st>;

    fn bit_vec_try_into(value: Self::SymbolicBitVec<'_>) -> Option<Self::Unsigned>;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word64;
impl Word for Word64 {
    type Unsigned = u64;
    type Signed = i64;
    type SymbolicBitVec<'st> = BitVec<'st, 64>;

    fn new_bit_vec<'st>(st: &'st Storage, value: u64) -> Self::SymbolicBitVec<'st> {
        (value as i64).into_with_storage(st)
    }

    fn bit_vec_try_into(value: Self::SymbolicBitVec<'_>) -> Option<Self::Unsigned> {
        bit_vec_term_to_i128(value).map(|i| i as _)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word8;
impl Word for Word8 {
    type Unsigned = u8;
    type Signed = i8;
    type SymbolicBitVec<'st> = BitVec<'st, 8>;

    fn new_bit_vec<'st>(st: &'st Storage, value: u8) -> Self::SymbolicBitVec<'st> {
        (value as i64).into_with_storage(st)
    }

    fn bit_vec_try_into(value: Self::SymbolicBitVec<'_>) -> Option<Self::Unsigned> {
        bit_vec_term_to_i128(value).map(|i| i as _)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word4;
impl Word for Word4 {
    type Unsigned = u4;
    type Signed = i4;
    type SymbolicBitVec<'st> = BitVec<'st, 4>;

    fn new_bit_vec<'st>(st: &'st Storage, value: u4) -> Self::SymbolicBitVec<'st> {
        value.as_::<i64>().into_with_storage(st)
    }

    fn bit_vec_try_into(value: Self::SymbolicBitVec<'_>) -> Option<Self::Unsigned> {
        bit_vec_term_to_i128(value).map(|i| i.as_())
    }
}

pub trait WordOps: Sized {
    fn overflowing_add(self, rhs: Self) -> (Self, bool);
    fn overflowing_sub(self, rhs: Self) -> (Self, bool);
    fn overflowing_mul(self, rhs: Self) -> (Self, bool);
    fn is_zero(&self) -> bool;
}

macro_rules! define_word_ops {
    ($t:ty) => {
        impl WordOps for $t {
            fn overflowing_add(self, rhs: Self) -> (Self, bool) {
                self.overflowing_add(rhs)
            }
            fn overflowing_sub(self, rhs: Self) -> (Self, bool) {
                self.overflowing_sub(rhs)
            }
            fn overflowing_mul(self, rhs: Self) -> (Self, bool) {
                self.overflowing_mul(rhs)
            }
            fn is_zero(&self) -> bool {
                *self == Integer::ZERO
            }
        }
    };
}
define_word_ops!(u4);
define_word_ops!(i4);
define_word_ops!(u8);
define_word_ops!(i8);
define_word_ops!(u64);
define_word_ops!(i64);

pub mod prelude {
    #[allow(unused_imports)]
    pub use super::{Word, Word4, Word8, Word64};
    pub use arbitrary_int::prelude::*;
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;

    fn any_u4() -> impl Strategy<Value = u4> {
        proptest::num::u8::ANY.prop_map(|u| u.as_())
    }

    #[property_test]
    fn overflowing_add_u4_eq_add_i4(#[strategy = any_u4()] x: u4, #[strategy = any_u4()] y: u4) {
        let (sum_u4, _overflow_u4) = x.overflowing_add(y);
        let x_i4: i4 = x.as_();
        let y_i4: i4 = y.as_();
        let (sum_i4, _overflow_i4) = x_i4.overflowing_add(y_i4);
        prop_assert_eq!(sum_u4, sum_i4.as_());
    }

    #[property_test]
    fn overflowing_u4_always_overflows(
        #[strategy = any_u4()] x: u4,
        #[strategy = any_u4()] overflow_amount: u4,
    ) {
        prop_assume!(!overflow_amount.is_zero());
        prop_assume!(overflow_amount < x);
        let y = u32::from(overflow_amount).wrapping_sub(x.as_());
        let y: u4 = y.as_();
        println!("x: {}, y: {}, overflow_amount: {}", x, y, overflow_amount);
        let (sum_u4, overflow_u4) = x.overflowing_add(y);
        prop_assert!(overflow_u4);
        prop_assert_eq!(sum_u4, overflow_amount);
    }
}
