use functionality::prelude::*;
use itertools::Either;
use std::collections::HashSet;
use std::hash::{BuildHasher, Hash};

pub fn intersect_all<T, S>(sets: &[HashSet<T, S>]) -> impl Iterator<Item = &T>
where
    HashSet<T, S>: Default,
    T: Eq + Hash,
    S: BuildHasher,
{
    let Some(smallest) = sets.iter().min_by_key(|s| s.len()) else {
        return Either::Left(std::iter::empty());
    };
    smallest
        .iter()
        .filter(|x| sets.iter().all(|s| s.contains(x)))
        .pipe(Either::Right)
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;
    use std::hash::RandomState;

    fn f<T: Clone + Eq + Hash>(
        inputs: impl IntoIterator<Item: IntoIterator<Item = T>>,
    ) -> HashSet<T> {
        intersect_all::<T, RandomState>(
            &inputs
                .into_iter()
                .map(|x| x.into_iter().collect())
                .collect::<Vec<_>>(),
        )
        .cloned()
        .collect()
    }

    #[test]
    fn empty() {
        let inputs: [[i32; 0]; 0] = [];
        assert_eq!(f(inputs), [].into());
    }

    #[property_test]
    fn lonely(set: HashSet<i32>) {
        prop_assert_eq!(f([set.iter().cloned()]), set);
    }

    #[property_test]
    fn two_sets(x: HashSet<u8>, y: HashSet<u8>) {
        prop_assert_eq!(
            f([x.clone(), y.clone()]),
            x.intersection(&y).cloned().collect()
        );
    }

    #[property_test]
    fn intersection_with_empty(sets: Vec<HashSet<u8>>) {
        prop_assert_eq!(f([[].into()].into_iter().chain(sets)), [].into(),);
    }
}
