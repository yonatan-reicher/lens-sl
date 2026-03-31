//! Define an interface to generalize over different bit-widths and also over SMT bit-vectors.

use crate::all::All;
use crate::bool::prelude::*;
use crate::smtlib_utils::{BitVecExt, bit_vec_term_to_i128};
#[cfg(test)]
use proptest::prelude::*;
use serde::{Deserialize, Serialize};
use smtlib::terms::{IntoWithStorage, StaticSorted};
use smtlib::{BitVec, Storage};
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ops::*;

// ========== The Traits =====================================================================

/// A trait for unsigned integer types that have wrapping semantics.
#[rustfmt::skip]
pub trait Word
    : AbstractWord<Bool=bool, FromParam=()>
    + All
    + Debug
    + Default
    + Display
    + Hash
    + From<usize>
    + From<i32> // Because int constants default to `i32`!
    + Into<usize>
    + Serialize
    + Sized
    + Sub<Output = Self> // Bit-vectors actually do not implement this, so we put this here
    + PartialOrd
    + Ord
    // + DeserializeOwned
    + 'static
{
    const ZERO: Self;
    const ONE: Self;
    const MAX: Self;
    fn into_smt_word<'st>(self, st: &'st Storage) -> Self::SmtWord<'st>;
    fn into_word<W: Word>(self) -> W { Into::<usize>::into(self).into() }
    fn into_abstract_word<W: AbstractWord>(self, arg: W::FromParam) -> W { W::from_word(self, arg) }
}

/// Abstracts over SMT bit-vectors.
#[rustfmt::skip]
pub trait SmtWord<'st>
    : AbstractWord<Bool=SmtBool<'st>, FromParam=&'st Storage>
    + From<smtlib::terms::Const<'st, Self>>
    + Into<smtlib::terms::Dynamic<'st>>
    + IntoWithStorage<'st, Self>
    + StaticSorted<'st, Inner = Self>
    + BitVecExt<'st>
    + 'st
{
    fn try_into_word(self) -> Option<Self::Word>;
}

/// Generic trait for as base for [Word] and [SmtWord].
#[rustfmt::skip]
pub trait AbstractWord
    // General
    : Clone
    + Copy
    + Debug
    // Arithmetic
    + Add<Output = Self>
    // + Sub<Output = Self> // Bit-vectors actually do not implement this
    + Div<Output = Self>
    + Mul<Output = Self>
    + Neg<Output = Self>
    // Bitwise
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Not<Output = Self>
    + Shl<Output = Self>
    + Shr<Output = Self>
    // + Rem<Output = Self>
{
    type Bool: Bool + IfThenElse<Self>;
    // One of these shall equal `Self`. And obviously, if B is bool, it's Word, and if it's SmtBool
    // than it's SmtWord.
    type SmtWord<'st>: SmtWord<'st>;
    type Word: Word;
    const BITS: usize;
    fn is_zero(&self) -> Self::Bool;
    fn signed_lt(&self, other: &Self) -> Self::Bool;
    fn unsigned_lt(&self, other: &Self) -> Self::Bool;
    fn signed_positive(&self) -> Self::Bool;
    fn signed_negative(&self) -> Self::Bool;
    type FromParam;
    fn from_word<W: Word>(w: W, arg: Self::FromParam) -> Self;
    fn get_from_param(&self) -> Self::FromParam;
}

// ========== The Implementation ==================================================================

macro_rules! impl_word {
    ($name:ident($t:ty) bits $bits:literal signed $signed:ident mask $mask:literal) => {
        #[derive(
            Clone, Copy, Default,
            derive_more::Debug, derive_more::Display,
            Deserialize, Serialize,
            PartialEq, Eq, Hash,
            PartialOrd, Ord,
        )]
        #[debug("{_0:?}")]
        #[display("{_0}")]
        pub struct $name($t);

        // ----- Behaviour -----

        #[rustfmt::skip] impl From<usize> for $name { fn from(x: usize) -> Self { Self((x & $mask) as $t) } }
        #[rustfmt::skip] impl From<$name> for usize { fn from(x: $name) -> usize { x.0 as usize } }
        #[rustfmt::skip] impl From<$t> for $name { fn from(x: $t) -> Self { Self::from(x as usize) } }
        #[rustfmt::skip] impl From<i32> for $name { fn from(x: i32) -> Self { Self::from(x as usize) } }
        #[rustfmt::skip] impl From<$name> for $t { fn from(x: $name) -> $t { x.0 as $t } }

        impl Word for $name {
            fn into_smt_word<'st>(self, st: &'st Storage) -> Self::SmtWord<'st> {
                (self.0 as i64).into_with_storage(st)
            }
            const ZERO: $name = Self(0);
            const ONE: $name = Self(1);
            const MAX: $name = Self(<$t>::MAX & $mask as $t);
        }

        impl AbstractWord for $name {
            type Bool = bool;
            type Word = Self;
            type SmtWord<'st> = BitVec<'st, $bits>;
            const BITS: usize = $bits;

            fn is_zero(&self) -> bool {
                self.0 == 0
            }

            fn signed_positive(&self) -> bool {
                // Check the MSB!
                let msb = self.0 >> ($bits - 1);
                msb == 0 && self.0 != 0
            }

            fn signed_negative(&self) -> bool {
                let msb = self.0 >> ($bits - 1);
                msb == 1
            }

            fn signed_lt(&self, other: &Self) -> bool {
                match (self.signed_negative(), other.signed_negative()) {
                    (false, false) => self < other,
                    (true, true) => -*self > -*other,
                    (false, true) => false,
                    (true, false) => true,
                }
            }

            fn unsigned_lt(&self, other: &Self) -> bool {
                self < other
            }

            type FromParam = ();
            fn from_word<W: Word>(w: W, (): ()) -> Self {
                w.into_word()
            }

            fn get_from_param(&self) -> () { () }
        }

        // Arithmetic
        impl_op!(Add for $name fn add(a, b) = Self::from(a.0.wrapping_add(b.0)));
        impl_op!(Sub for $name fn sub(a, b) = Self::from(a.0.wrapping_sub(b.0)));
        impl_op!(Div for $name fn div(a, b) = Self::from(a.0.wrapping_div(b.0)));
        impl_op!(Mul for $name fn mul(a, b) = Self::from(a.0.wrapping_mul(b.0)));
        impl_op!(Neg for $name fn neg(a) = !a + Self::ONE);
        // // + Rem<Output = Self>
        // Bitwise
        impl_op!(BitAnd for $name fn bitand(a, b) = Self::from(a.0 & b.0));
        impl_op!(BitOr  for $name fn bitor(a, b) = Self::from(a.0 | b.0));
        impl_op!(BitXor for $name fn bitxor(a, b) = Self::from(a.0 ^ b.0));
        impl_op!(Not    for $name fn not(a) = Self::from(!a.0));
        // impl_op!(Shl    for $name fn shl(a, b) = Self::from(a.0 << b.0));
        // impl_op!(Shr    for $name fn shr(a, b) = Self::from(a.0 >> b.0));
        impl_op!(Shl    for $name fn shl(a, b) = Self::from(a.0.unbounded_shl(to_u32_saturating(b.0))));
        impl_op!(Shr    for $name fn shr(a, b) = Self::from(a.0.unbounded_shr(to_u32_saturating(b.0))));

        impl All for $name {
            type Iter = std::iter::Map<std::ops::RangeInclusive<$t>, fn($t) -> $name>;
            fn all() -> Self::Iter { (0..=Self::MAX.0).map(Self) }
        }

        #[cfg(test)]
        impl Arbitrary for $name {
            type Parameters = ();
            type Strategy = proptest::strategy::Map<std::ops::RangeInclusive<usize>, fn(usize) -> Self>;
            fn arbitrary_with((): ()) -> Self::Strategy {
                (0..=$mask).prop_map(Self::from)
            }
        }

        // Also implement things for the bit vector!

        impl<'st> SmtWord<'st> for BitVec<'st, $bits> {
            fn try_into_word(self) -> Option<Self::Word> {
                bit_vec_term_to_i128(self).map(|i| {
                    let u = i as u128;
                    Self::Word::from((u & $mask as u128) as $t)
                })
            }
        }

        impl<'st> AbstractWord for BitVec<'st, $bits> {
            type Bool = SmtBool<'st>;
            type Word = $name;
            type SmtWord<'st1> = BitVec<'st1, $bits>;
            const BITS: usize = $bits;

            fn is_zero(&self) -> SmtBool<'st> {
                BitVecExt::is_zero(self)
            }

            fn signed_lt(&self, other: &Self) -> SmtBool<'st> {
                BitVecExt::signed_lt(*self, *other)
            }

            fn unsigned_lt(&self, other: &Self) -> SmtBool<'st> {
                BitVecExt::unsigned_lt(*self, *other)
            }

            fn signed_positive(&self) -> SmtBool<'st> {
                self.is_positive()
            }

            fn signed_negative(&self) -> SmtBool<'st> {
                self.is_negative()
            }

            type FromParam = &'st Storage;
            fn from_word<W: Word>(w: W, st: &'st Storage) -> Self {
                w.into_word::<Self::Word>().into_smt_word(st)
            }

            fn get_from_param(&self) -> Self::FromParam {
                smtlib::Sorted::st(self)
            }
        }
    };
}

macro_rules! impl_op {
    // One argument
    ($trait:ident for $t:ident fn $f:ident($a:ident) = $e:expr) => {
        impl $trait for $t {
            type Output = Self;
            fn $f(self) -> Self {
                let $a = self;
                $e
            }
        }
    };
    // Two arguments
    ($trait:ident for $t:ident fn $f:ident($a:ident, $b:ident) = $e:expr) => {
        impl $trait for $t {
            type Output = Self;
            fn $f(self, other: Self) -> Self {
                let ($a, $b) = (self, other);
                $e
            }
        }
    };
}

/// Assumes `T` is an unsigned number type
fn to_u32_saturating<T: TryInto<u32>>(x: T) -> u32 {
    x.try_into().unwrap_or(u32::MAX)
}

impl_word!(Word2(u8)   bits 2  signed i8  mask 0x03);
impl_word!(Word4(u8)   bits 4  signed i8  mask 0x0F);
impl_word!(Word5(u8)   bits 5  signed i8  mask 0x1F);
impl_word!(Word6(u8)   bits 6  signed i8  mask 0x3F);
impl_word!(Word8(u8)   bits 8  signed i8  mask 0xFF);
impl_word!(Word32(u32) bits 32 signed i32 mask 0xFFFFFFFFusize);
impl_word!(Word64(u64) bits 64 signed i64 mask 0xFFFFFFFFFFFFFFFFusize);

// =================================================================================================
//                                            Bit Word
// =================================================================================================

/// Associates the exact word size needed to hold the number of bits in the word.
pub trait HasBitWord {
    type BitWord: Word;
}

#[rustfmt::skip] impl HasBitWord for Word4 { type BitWord = Word2; }
#[rustfmt::skip] impl HasBitWord for Word32 { type BitWord = Word5; }
#[rustfmt::skip] impl HasBitWord for Word64 { type BitWord = Word6; }

pub type BitWord<T: HasBitWord> = T::BitWord;

// =================================================================================================
//                                             Other
// =================================================================================================

pub mod prelude {
    #[allow(unused_imports)]
    pub use super::{
        AbstractWord, BitWord, HasBitWord, SmtWord, Word, Word2, Word4, Word5, Word6, Word8,
        Word32, Word64,
    };
    pub use crate::smtlib_utils::BitVecExt;
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::property_test;

    #[property_test]
    fn word4_add_word4_eq_neg_neg_word4_sub_word4(x: Word4, y: Word4) {
        prop_assert_eq!(x + y, -(-x - y));
    }

    #[test]
    fn shift_left_overflows_word8() {
        assert_eq!(Word8(200) << Word8(2), Word8(32));
    }

    #[test]
    fn shift_left_overflows_word64() {
        assert_eq!(Word64(!0u64) << Word64(2), Word64(!0u64 ^ 0b11));
    }
}
