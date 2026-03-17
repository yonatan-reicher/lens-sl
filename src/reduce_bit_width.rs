use crate::iter_slice_or_single::Iter;
use crate::word::prelude::*;
use rustc_hash::FxHashMap;

/// What is this immediate used for? This decides what it's equivalent should be.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct ImmediateInfo {
    /// True if this immediate is used as a shift argument.
    pub is_shift: bool,
}

pub fn reduce<WBig: Word, WSmall: Word>(
    c: WBig,
    info: &ImmediateInfo,
) -> WSmall {
    if info.is_shift {
        let bit_width = WBig::from(WBig::BITS);
        let reduced_bit_width = WSmall::from(WSmall::BITS);
        return if c == bit_width {
            reduced_bit_width
        } else if c == bit_width - 1.into() {
            reduced_bit_width - 1.into()
        } else if bit_width / 2.into() <= c && c < bit_width - 1.into() {
            reduced_bit_width / 2.into()
        } else
        /* 0 <= arg && arg < bit_width / 2 */
        {
            1.into()
        };
    }
    Into::<usize>::into(c).into()
}

/// This structure remembers how constants were reduced, so that we can extend them back.
#[derive(Clone, Debug, Default, derive_more::PartialEq, derive_more::Eq)]
pub struct Reducer<WBig, WSmall>(FxHashMap<WSmall, Vec<WBig>>);

impl<WBig: Word, WSmall: Word> Reducer<WBig, WSmall> {
    #[inline]
    pub fn reduce(&mut self, value: WBig, info: &ImmediateInfo) -> WSmall {
        let reduced = reduce::<WBig, WSmall>(value, info);
        let bucket = self.0.entry(reduced).or_insert_with(|| vec![value]);
        if !bucket.contains(&value) {
            bucket.push(value);
        }
        reduced
    }

    #[inline]
    pub fn extend(&self, value: WSmall) -> Iter<'_, WBig> {
        self.0
            .get(&value)
            .map_or(Iter::Single(value.into().into()), |v| Iter::Slice(v.as_slice()))
    }

    pub fn immediates(&self) -> impl Iterator<Item = WSmall> + '_ {
        self.0.keys().cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;
    use std::collections::HashSet;

    #[property_test]
    fn test_reduce_extend(v: HashSet<Word64>) {
        prop_assume!(!v.is_empty());
        println!("v = {v:?}");
        // Initialize reducer
        let mut reducer = Reducer::<Word64, Word4>::default();
        let info = ImmediateInfo { is_shift: false };
        for &val in &v {
            reducer.reduce(val, &info);
        }
        let c = v.iter().next().copied().unwrap();
        let expected: HashSet<Word64> = v
            .iter()
            .copied()
            .filter(|val| c.into_word::<Word4>() == val.into_word::<Word4>())
            .collect();
        // Reduce and then extend
        let reduced = reducer.reduce(c, &info);
        let extended: HashSet<Word64> = reducer.extend(reduced).collect();
        // Assert
        dbg!(reducer);
        prop_assert_eq!(extended, expected);
    }
}
