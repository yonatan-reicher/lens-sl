use crate::programs::Programs;
use functionality::prelude::*;
use rustc_hash::FxHashMap;
use std::hash::Hash;

/// Note a special case, when the graph is empty, it is treated as containing only the empty
/// program.
#[derive(Clone, Debug)]
pub struct BackwardGraph<S, I>(pub Vec<FxHashMap<S, Programs<I>>>);

#[derive(Debug)]
pub struct BackwardGraphPath<'a, S, I> {
    pub g: &'a mut BackwardGraph<S, I>,
    states: Vec<S>,
}

// =================================================================================================
//                                        Implementations
// =================================================================================================

impl<S, I> BackwardGraph<S, I> {
    pub fn root<'a>(&'a mut self) -> BackwardGraphPath<'a, S, I> {
        BackwardGraphPath {
            g: self,
            states: vec![],
        }
    }
}

impl<'a, S, I> BackwardGraphPath<'a, S, I>
where
    S: Clone + Eq + Hash,
    I: Clone + Eq,
{
    pub const fn is_leaf(&self) -> bool {
        self.g.0.len() == self.states.len()
    }

    /// If this path points at a leaf, return a mutable reference to it.
    pub fn try_leaf(&mut self) -> Option<Vec<Vec<I>>> {
        if self.is_leaf() {
            // TODO: This is a workaround for a bug. Fix this.
            if self.g.0.iter().all(|m| m.is_empty()) {
                return Some(vec![vec![]]);
            }
            Some(
                self.states
                    .iter()
                    .zip(&self.g.0)
                    .map(|(s, map)| &map[s])
                    // .pipe(|v| intersect_all(v))
                    .pipe(|mut iter| {
                        let Some(first) = iter.next() else {
                            return vec![];
                        };
                        let rest = iter.collect::<Vec<_>>();
                        let mut ret = vec![];
                        first.each(|p| {
                            if rest.iter().all(|ps| ps.contains(&p)) {
                                ret.push(p.clone());
                            }
                        });
                        ret
                    }),
            )
        } else {
            None
        }
    }

    pub fn try_descend(&mut self, state: S) -> Result<(), ()> {
        if self.is_leaf() {
            Err(())
        } else {
            let map = &self.g.0[self.states.len()];
            if map.contains_key(&state) {
                self.states.push(state);
                Ok(())
            } else {
                Err(())
            }
        }
    }

    pub fn ascend(&mut self) {
        self.states.pop();
    }
}

impl<S, I> Default for BackwardGraph<S, I> {
    fn default() -> Self {
        Self(vec![])
    }
}
