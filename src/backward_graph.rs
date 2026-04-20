use crate::programs::Programs;
use rustc_hash::FxHashMap;
use std::hash::Hash;
use std::fmt::Debug;

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
    pub fn root<'a>(&'a mut self) -> BackwardGraphPath<'a, S, I> {
        BackwardGraphPath {
            g: self,
            inputs: vec![],
        }
    }

    fn longest(&self) -> Option<&InputsByCounterExample<S, I>> {
        self.0.last()
    }
    fn longest_mut(&mut self) -> Option<&mut InputsByCounterExample<S, I>> {
        self.0.last_mut()
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
    pub fn get(&self) -> Option<Vec<Vec<I>>> {
        if !self.ended() {
            return None;
        }
        let start = &self.g.longest().unwrap();
        let mut sets = self.inputs.iter().zip(start.iter()).map(|(s, map)| &map[s]);
        let Some(first) = sets.next() else {
            return Some(vec![]);
        };
        let rest = sets.collect::<Vec<_>>();
        let mut ret = vec![];
        first.each(|p| {
            if rest.iter().all(|ps| ps.contains(&p)) {
                ret.push(p.clone());
            }
        });
        Some(ret)
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

    pub fn ascend(&mut self) {
        self.inputs.pop();
    }
}

impl<S, I> Default for BackwardGraph<S, I> {
    fn default() -> Self {
        Self(vec![])
    }
}
