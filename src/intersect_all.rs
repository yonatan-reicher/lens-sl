use functionality::prelude::*;
use std::collections::HashSet;
use std::hash::{BuildHasher, Hash};

pub fn intersect_all<'a, T, S>(mut sets: impl Iterator<Item = &'a HashSet<T, S>>) -> HashSet<T, S>
where
    HashSet<T, S>: Clone + Default,
    T: Eq + Hash + 'a,
    S: BuildHasher + 'a,
{
    let Some(mut current) = sets.next().cloned() else {
        return default();
    };
    for s in sets {
        if current.is_empty() || s.is_empty() {
            return default();
        }
        current
            // Remove any element that is not in the new set.
            .extract_if(|x| !s.contains(x))
            // Run through the entire set.
            .for_each(|_| ());
    }
    current
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
            inputs
                .into_iter()
                .map(|x| x.into_iter().collect())
                .collect::<Vec<_>>()
                .iter(),
        )
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
        println!("------------------------");
        println!("x {x:?}  y {y:?}");
        prop_assert_eq!(
            f([x.clone(), y.clone()]),
            x.intersection(&y).cloned().collect()
        );
    }

    #[property_test]
    fn intersection_with_empty(sets: Vec<HashSet<u8>>) {
        prop_assert_eq!(f([[].into()].into_iter().chain(sets)), [].into(),);
    }

    #[property_test]
    fn contains_all(sets: Vec<HashSet<u8>>) {
        println!("------------------------");
        println!("Sets:");
        for s in sets.iter() {
            println!("  {s:?}");
        }
        let s = intersect_all(sets.iter()).into_iter().collect::<Vec<_>>();
        println!("Intersection: \n{s:?}");
        for x in s {
            for s1 in sets.iter() {
                prop_assert!(s1.contains(&x));
            }
        }
    }
}
