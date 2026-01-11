use rustc_hash::FxHashMap;
use std::hash::Hash;

/// A search graph for programs and their outputs on some test cases.
/// For n test cases (input_n, output_n), the graph has n levels, including 0.
/// On the 0 < k ≤ n level, each edge with output_k connects to a sub-graph
/// where all programs produce output_k on input_k.
#[derive(Clone, Debug)]
pub enum Graph<State, Program> {
    /// A 0-tests graph. Just a series of programs.
    Leaf(Vec<Program>),
    /// For the corresponding test case (input, output), each program in the
    /// inner hash map outputs the output from the input.
    Nest(FxHashMap<State, Self>),
}

impl<S, P> Graph<S, P> {
    pub const fn new() -> Self {
        Self::Leaf(vec![])
    }
}

impl<S, P> Default for Graph<S, P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S, P> Graph<S, P>
where
    S: Eq + Hash + Clone,
{
    /// Returns the maximum depth of the graph.
    pub fn depth(&self) -> usize {
        match self {
            Self::Leaf(_) => 0,
            Self::Nest(hash_map) => {
                hash_map.values()
                    .map(|sub_graph| sub_graph.depth())
                    .max()
                    .unwrap_or(0)
                    + 1
            }
        }
    }

    /// Insert the given programs under the given set of states. The length of
    /// the slice of output states must be of the same depth as the graph.
    pub fn insert_all(&mut self, outputs: &[S], progs: impl IntoIterator<Item = P>) {
        match self {
            Self::Leaf(programs) => {
                debug_assert!(outputs.is_empty());
                programs.extend(progs);
            }
            Self::Nest(hash_map) => {
                let [output, rest @ ..] = outputs else {
                    println!("Graph depth: {}, outputs length: {}", self.depth(), outputs.len());
                    panic!("Mismatched graph depth and outputs length: graph depth > outputs length");
                };
                hash_map
                    .entry(output.clone())
                    .or_insert_with(|| match rest {
                        [] => Self::Leaf(vec![]),
                        _ => Self::Nest(Default::default()),
                    })
                    .insert_all(rest, progs);
            }
        }
    }
}
