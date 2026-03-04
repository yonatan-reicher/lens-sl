use rustc_hash::FxHashMap;
use std::fmt::Display;
use std::hash::Hash;

pub trait Programs: Default + Into<Vec<Self::Program>> {
    type Program;
    fn len(&self) -> usize;
    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn extend(&mut self, other: Self);
}

/// A search graph for programs and their outputs on some test cases.
/// For n test cases (input_n, output_n), the graph has n levels, including 0.
/// On the 0 < k ≤ n level, each edge with output_k connects to a sub-graph
/// where all programs produce output_k on input_k.
#[derive(Clone, Debug)]
pub enum Graph<State, P: Programs> {
    /// A 0-tests graph. Just a series of programs.
    Leaf(P),
    /// For the corresponding test case (input, output), each program in the
    /// inner hash map outputs the output from the input.
    Nest(FxHashMap<State, Self>),
}

impl<S, P: Programs> Graph<S, P>
where
    S: Eq + Hash + Clone,
{
    /// The number of programs stored in the graph.
    pub fn n_programs(&self) -> usize {
        match self {
            Self::Leaf(programs) => programs.len(),
            Self::Nest(hash_map) => hash_map
                .values()
                .map(|sub_graph| sub_graph.n_programs())
                .sum(),
        }
    }

    /// Number of leaves in the graph.
    pub fn n_leaves(&self) -> usize {
        match self {
            Self::Leaf(..) => 1,
            Self::Nest(hash_map) => hash_map
                .values()
                .map(|sub_graph| sub_graph.n_leaves())
                .sum(),
        }
    }

    /// The number of nodes in the graph. Includes both leaves and inner nodes.
    pub fn n_nodes(&self) -> usize {
        match self {
            Self::Leaf(_) => 1,
            Self::Nest(hash_map) => {
                1 + hash_map
                    .values()
                    .map(|sub_graph| sub_graph.n_nodes())
                    .sum::<usize>()
            }
        }
    }

    /// Returns the maximum depth of the graph.
    pub fn depth(&self) -> usize {
        match self {
            Self::Leaf(_) => 0,
            Self::Nest(hash_map) => {
                hash_map
                    .values()
                    .map(|sub_graph| sub_graph.depth())
                    .max()
                    .unwrap()
                    + 1
            }
        }
    }

    /// The number of direct children in the tree.
    pub fn n_children(&self) -> usize {
        match self {
            Graph::Leaf(_) => 0,
            Graph::Nest(hash_map) => hash_map.len(),
        }
    }

    /// Insert the given programs under the given set of states. The length of
    /// the slice of output states must be of the same depth as the graph.
    pub fn insert(&mut self, output: S, progs: P) {
        debug_assert!(matches!(self, Graph::Nest(..)));
        match self {
            Self::Leaf(..) => unreachable!(),
            Self::Nest(hash_map) => {
                // Find the sub-graph, or create it.
                let sub_graph_in_output = hash_map
                    .entry(output.clone())
                    .or_insert_with(|| Self::Leaf(P::default()));
                // It must be a leaf!
                let Graph::Leaf(sub_graph_programs) = sub_graph_in_output else {
                    unreachable!();
                };
                // Insert
                sub_graph_programs.extend(progs);
            }
        }
    }

    /// Insert the given programs under the given set of states. The length of
    /// the slice of output states must be of the same depth as the graph.
    pub fn insert_all(&mut self, outputs: &[S], progs: P) {
        match self {
            Self::Leaf(programs) => {
                debug_assert!(outputs.is_empty());
                programs.extend(progs);
            }
            Self::Nest(hash_map) => {
                let [output, rest @ ..] = outputs else {
                    println!(
                        "Graph depth: {}, outputs length: {}",
                        self.depth(),
                        outputs.len()
                    );
                    panic!(
                        "Mismatched graph depth and outputs length: graph depth > outputs length"
                    );
                };
                hash_map
                    .entry(output.clone())
                    .or_insert_with(|| match rest {
                        [] => Self::Leaf(P::default()),
                        _ => Self::Nest(Default::default()),
                    })
                    .insert_all(rest, progs);
            }
        }
    }

    /*
    pub fn into_iter(self) -> impl Iterator<Item = (Vec<S>, P)> {
        GraphIterator::new(self)
    }
    */

    pub fn for_each<F>(self, f: &mut F)
    where
        F: FnMut(P),
    {
        match self {
            Self::Leaf(programs) => f(programs),
            Self::Nest(hash_map) => {
                for sub_graph in hash_map.into_values() {
                    sub_graph.for_each(f);
                }
            }
        }
    }
}

impl<S, P> Graph<S, P>
where
    S: Eq + Hash + Display,
    P: Programs + Display,
{
    pub fn pretty_print_lines(&self) -> Vec<String> {
        match self {
            Graph::Leaf(programs) => programs.to_string().lines().map(String::from).collect(),
            Graph::Nest(hash_map) => {
                let mut lines = vec![];
                for (state, sub_graph) in hash_map {
                    let sub_lines = sub_graph.pretty_print_lines();
                    lines.push(format!("State: {state}"));
                    for sub_line in sub_lines {
                        lines.push(format!("  {sub_line}"));
                    }
                }
                lines
            }
        }
    }

    pub fn pretty_print(&self) -> String {
        self.pretty_print_lines().join("\n")
    }
}
