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
    c: WBig::Unsigned,
    info: &ImmediateInfo,
) -> WSmall::Unsigned {
    if info.is_shift {
        let bit_width = WBig::Unsigned::from_(WBig::Unsigned::BITS as u64);
        let reduced_bit_width = WSmall::Unsigned::from_(WSmall::Unsigned::BITS as u64);
        return if c == bit_width {
            reduced_bit_width
        } else if c == bit_width - 1.as_() {
            reduced_bit_width - 1.as_()
        } else if bit_width / 2.as_() <= c && c < bit_width - 1.as_() {
            reduced_bit_width / 2.as_()
        } else
        /* 0 <= arg && arg < bit_width / 2 */
        {
            1.as_()
        };
    }
    c.as_()
}

/// This structure remembers how constants were reduced, so that we can extend them back.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Reducer<WBig: Word, WSmall: Word>(FxHashMap<WSmall::Unsigned, Vec<WBig::Unsigned>>);

impl<WBig: Word, WSmall: Word> Reducer<WBig, WSmall> {
    #[inline]
    pub fn reduce(&mut self, value: WBig::Unsigned, info: &ImmediateInfo) -> WSmall::Unsigned {
        let reduced = reduce::<WBig, WSmall>(value, info);
        let bucket = self.0
            .entry(reduced)
            .or_insert_with(|| vec![value]);
        if !bucket.contains(&value) {
            bucket.push(value);
        }
        reduced
    }

    #[inline]
    pub fn extend(&self, value: WSmall::Unsigned) -> Iter<'_, WBig::Unsigned> {
        self.0
            .get(&value)
            .map_or(Iter::Single(value.as_()), |v| Iter::Slice(v.as_slice()))
    }

    pub fn immediates(&self) -> impl Iterator<Item = WSmall::Unsigned> + '_ {
        self.0.keys().cloned()
    }
}
