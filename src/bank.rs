use rustc_hash::FxHashMap;
use std::hash::Hash;

/// A bank of programs, like in Sobeq. The Lens equivalent is the Graph. Unlike in Lens, we don't
/// store programs along their states, but we store them under their effects on a state. An effect
/// is just what the program outputs on certain inputs. So for example, a program like
/// p = `mov r0, r1; add r0, r0, 5` may be listed under the effects (r0=5, r1=10) ↦ (r0=15, r1=10)
/// and (r0=1, r1=8) ↦ (r0=13, r1=8). The difference is that the effects don't contain the entire
/// state, only what matters.
#[derive(Debug, Default)]
pub struct Bank<Effect, Programs> {
    classes: FxHashMap<Effect, Programs>,
}

impl<E, Ps> Bank<E, Ps>
where
    E: Eq + Hash,
    Ps: Default,
{
    pub fn insert<P>(&mut self, e: E, p: P)
    where
        Ps: Extend<P>,
    {
        let programs = self.classes.entry(e).or_default();
        programs.extend([p]);
    }
}

impl<E, P> Bank<E, P> {
    pub fn n_effects(&self) -> usize {
        self.classes.len()
    }

    /// This returns the sum of all programs in all effects. This doesn't return the number of
    /// programs in total, because programs can (and should?) appear under multiple effects.
    pub fn n_entries(&self) -> usize
    where
        P: crate::len::Len,
    {
        self.classes.values().map(|p| p.len()).sum()
    }

    pub fn iter(&self) -> Iter<'_, E, P> {
        self.into_iter()
    }
}

pub type Iter<'a, E, P> = <&'a FxHashMap<E, P> as IntoIterator>::IntoIter;

impl<'a, E, P> IntoIterator for &'a Bank<E, P> {
    type Item = (&'a E, &'a P);
    type IntoIter = Iter<'a, E, P>;
    fn into_iter(self) -> Self::IntoIter {
        self.classes.iter()
    }
}
