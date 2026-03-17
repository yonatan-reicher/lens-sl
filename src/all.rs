use std::ops::RangeInclusive;

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
