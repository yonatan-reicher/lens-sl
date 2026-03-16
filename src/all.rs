use std::ops::{Range, RangeInclusive};

pub trait All {
    type Iter: Iterator<Item = Self>;
    fn all() -> Self::Iter;
}

// Implementations

macro_rules! impl_num_all {
    ($t:ty) => {
        impl All for $t {
            type Iter = RangeInclusive<Self>;
            fn all() -> Self::Iter {
                (0..=Self::MAX)
            }
        }
    };
}

impl_num_all!(u8);
impl_num_all!(u64);

use arbitrary_int::{traits::Integer, u4};
impl All for u4 {
    type Iter = std::iter::Map<Range<u8>, fn(u8) -> Self>;
    fn all() -> Self::Iter {
        // Use exclusive range instead of exclusive...because...we can...and it's one byte less
        // memory for storing the iterator 😭!
        (0..u4::MAX.as_::<u8>() + 1).map(|x| x.as_())
    }
}
