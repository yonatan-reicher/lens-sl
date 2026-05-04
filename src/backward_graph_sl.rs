use crate::len::Len;
use crate::programs_sl::Programs;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::ControlFlow;

/// BackwardsMap[postfix-length][test-index][input] = set of programs, that postfix-length
/// instructions before the end, on the given input, return output of the given test.
#[derive(Clone, Debug)]
pub struct BackwardGraph<S, I>(pub InputsByLen<S, I>);
pub type InputsByLen<S, I> = Vec<InputsByCounterExample<S, I>>;
pub type InputsByCounterExample<S, I> = Vec<FxHashMap<S, Programs<I>>>;

#[derive(Debug)]
pub struct BackwardGraphPath<'a, S, I> {
    pub g: &'a mut BackwardGraph<S, I>,
    pub inputs: Vec<S>,
}

// =================================================================================================
//                                        Implementations
// =================================================================================================

impl<S, I> BackwardGraph<S, I> {
    /// Returns a path starting at the root of the graph.
    pub fn root<'a>(&'a mut self) -> BackwardGraphPath<'a, S, I> {
        BackwardGraphPath {
            g: self,
            inputs: vec![],
        }
    }

    /// A reference to the last row of the graph.
    fn longest(&self) -> Option<&InputsByCounterExample<S, I>> {
        self.0.last()
    }
}

impl<'a, S, I> BackwardGraphPath<'a, S, I>
where
    S: Clone + Debug + Eq + Hash,
    I: Clone + Debug + Eq,
{
    pub fn ended(&self) -> bool {
        let Some(start) = self.g.longest() else {
            return true;
        };
        let counter_example_index = self.inputs.len();
        counter_example_index >= start.len()
    }

    // TODO: rename
    /// Returns true when there was a match.
    pub fn get<T>(&self, mut f: impl FnMut(&[I]) -> ControlFlow<T>) -> ControlFlow<T, bool> {
        use ControlFlow::Continue;
        if !self.ended() {
            return Continue(false);
        }
        let start = &self.g.longest().unwrap();
        let sets = self
            .inputs
            .iter()
            .zip(start.iter())
            .map(|(s, map)| map[s].clone())
            .sorted_by_key(|s| s.len())
            .collect_vec();
        let Some((i, smallest)) = sets.iter().enumerate().min_by_key(|(_, s)| s.len()) else {
            return Continue(false);
        };
        smallest
            .try_each_reversed(|p| {
                if sets
                    .iter()
                    .enumerate()
                    .all(|(j, ps)| j == i || ps.contains(p.iter().cloned()))
                {
                    f(p)?;
                }
                Continue(())
            })
            .map_continue(|()| true)
    }

    pub fn try_descend(&mut self, state: S) -> Result<(), ()> {
        if self.ended() {
            Err(())
        } else {
            let start = self.g.longest().unwrap();
            let map = &start[self.inputs.len()];
            if map.contains_key(&state) {
                self.inputs.push(state);
                Ok(())
            } else {
                Err(())
            }
        }
    }

    /// Like [Self::try_descend], but does not do any checks.
    pub fn force_descend(&mut self, state: S) {
        self.inputs.push(state);
    }

    pub fn ascend(&mut self) {
        self.inputs.pop();
    }
}

impl<S, I> Default for BackwardGraph<S, I> {
    fn default() -> Self {
        Self(vec![])
    }
}
