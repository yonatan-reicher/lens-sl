use rustc_hash::{FxHashMap, FxHashSet};
use std::hash::Hash;

/// I - A machine instruction.
/// S - A state of the machine.
#[derive(Clone, Debug)]
pub struct Graph<I, S>
where
    S: Eq + Hash,
{
    // All the states that we encountered.
    all_states: FxHashSet<S>,
    /// Maps a list of starting states ending states to instructions that
    /// transform those starting states into those ending state.
    forward: FxHashMap<S, FxHashMap<S, Vec<I>>>,
}

impl<I, S> Graph<I, S>
where
    S: Eq + Hash,
{
    pub fn new() -> Self {
        Self {
            all_states: Default::default(),
            forward: Default::default(),
        }
    }

    /// Returns an iterator of all states that appear together.
    pub fn all_states(&self) -> impl Iterator<Item = &S> {
        self.all_states.iter()
    }

    /// Does the graph contain all these states in the same point in the graph.
    pub fn contains_states(&self, state: &S) -> bool {
        self.all_states.contains(state)
    }
}

impl<I, S> Graph<I, S>
where
    S: Eq + Hash + Clone,
{
    pub fn insert_forward(&mut self, i: I, s1: S, s2: S) {
        self.all_states.insert(s1.clone());
        self.all_states.insert(s2.clone());
        self.forward
            .entry(s1.clone())
            .or_default()
            .entry(s2.clone())
            .or_default()
            .push(i);
    }

    pub fn get_forward(&self, s1: &S) -> Option<std::collections::hash_map::Iter<S, Vec<I>>> {
        let next = self.forward.get(s1)?;
        Some(next.iter())
    }
}

impl<I, S> Default for Graph<I, S>
where
    S: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}
