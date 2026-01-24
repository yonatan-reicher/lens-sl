use std::fmt::{Debug, Display};
use std::hash::Hash;

use arbitrary_int::traits::{Integer, SignedInteger, UnsignedInteger};
use arbitrary_int::{i4, u4};

pub trait Word:
    Sized + Clone + Copy + Debug + Default + PartialEq + Eq + PartialOrd + Ord + Hash
{
    type Signed: Sized + Debug + Default + Display + Hash + SignedInteger + WordOps;
    type Unsigned: Sized + Debug + Default + Display + Hash + UnsignedInteger + WordOps;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word64;
impl Word for Word64 {
    type Unsigned = u64;
    type Signed = i64;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word8;
impl Word for Word8 {
    type Unsigned = u8;
    type Signed = i8;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word4;
impl Word for Word4 {
    type Unsigned = u4;
    type Signed = i4;
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
