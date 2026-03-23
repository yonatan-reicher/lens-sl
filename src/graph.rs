use crate::len::Len;
use functionality::{Mutate, Pipe};
use rustc_hash::FxHashMap;
use std::fmt::Display;
use std::hash::Hash;

/// A search graph for programs and their outputs on some test cases.
/// For n test cases (input_n, output_n), the graph has n levels, including 0.
/// On the 0 < k ≤ n level, each edge with output_k connects to a sub-graph
/// where all programs produce output_k on input_k.
#[derive(Clone, Debug)]
pub enum Graph<State, P> {
    /// A 0-tests graph. Just a series of programs.
    Leaf(P),
    /// For the corresponding test case (input, output), each program in the
    /// inner hash map outputs the output from the input.
    Nest(FxHashMap<State, Self>),
}

impl<S, P> Graph<S, P>
where
    S: Eq + Hash + Clone,
{
    /// The number of programs stored in the graph.
    pub fn n_programs(&self) -> usize
    where
        P: Len,
    {
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
    pub fn depth(&self) -> Option<usize> {
        match self {
            Self::Leaf(_) => Some(0),
            Self::Nest(hash_map) => hash_map
                .values()
                .filter_map(|sub_graph| sub_graph.depth())
                .max()
                .map(|d| d + 1),
        }
    }

    /// The number of direct children in the tree.
    pub fn n_children(&self) -> usize {
        match self {
            Graph::Leaf(_) => 0,
            Graph::Nest(hash_map) => hash_map.len(),
        }
    }

    pub fn insert<A>(&mut self, output: S, progs: impl IntoIterator<Item = A>)
    where
        P: Default + Extend<A>,
    {
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
    pub fn insert_all<A>(&mut self, outputs: &[S], progs: impl IntoIterator<Item = A>)
    where
        P: Default + Extend<A> + Extend<P> + Len,
    {
        match self {
            Self::Leaf(programs) => {
                if !outputs.is_empty() && programs.is_empty() {
                    *self = Self::Nest(FxHashMap::default());
                    return self.insert_all(outputs, progs);
                }
                programs.extend(progs);
            }
            Self::Nest(hash_map) => {
                let [output, rest @ ..] = outputs else {
                    let p = std::mem::replace(self, Graph::Nest(Default::default())).flatten();
                    *self = Self::Leaf(p);
                    return self.insert_all(outputs, progs);
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

    pub fn flatten(self) -> P
    where
        P: Default + Extend<P>,
    {
        match self {
            Graph::Leaf(p) => p,
            Graph::Nest(map) => map
                .into_values()
                .map(|sub_graph| sub_graph.flatten())
                .pipe(|iter| P::default().mutate(|p| p.extend(iter))),
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

// ================================================================================================
//                                        Pretty Printing
// ================================================================================================

impl<S, P> Graph<S, P> {
    pub fn pretty_print_lines_with(
        &self,
        pretty_prog: &mut impl FnMut(&P) -> String,
        pretty_state: &mut impl FnMut(&S) -> String,
    ) -> Vec<String> {
        match self {
            Graph::Leaf(programs) => pretty_prog(programs).lines().map(String::from).collect(),
            Graph::Nest(hash_map) => {
                let mut lines = vec![];
                for (state, sub_graph) in hash_map {
                    let sub_lines = sub_graph.pretty_print_lines_with(pretty_prog, pretty_state);
                    lines.push(format!("State: {}", pretty_state(state)));
                    for sub_line in sub_lines {
                        lines.push(format!("  {sub_line}"));
                    }
                }
                lines
            }
        }
    }

    pub fn pretty_print_with(
        &self,
        mut p: impl FnMut(&P) -> String,
        mut s: impl FnMut(&S) -> String,
    ) -> String {
        self.pretty_print_lines_with(&mut p, &mut s).join("\n")
    }
}

impl<S, P> Graph<S, P>
where
    S: Display,
    P: Display,
{
    pub fn pretty_print_lines(&self) -> Vec<String> {
        self.pretty_print_lines_with(&mut |p| p.to_string(), &mut |s| s.to_string())
    }

    pub fn pretty_print(&self) -> String {
        self.pretty_print_lines().join("\n")
    }
}

// ==================== Printing Stats ==========================================

impl<S, P> Graph<S, P>
where
    S: Eq + Hash + Clone,
    P: Len,
{
    /// Print information that shows us growth and memory usage of the graphs.
    #[allow(dead_code)]
    fn print_states(forward_graph: &Self, backward_graph: &Self) {
        macro_rules! ignore {
            ( $a:tt, $b:tt ) => {
                $b
            };
        }
        macro_rules! print_row {
        ( $($e:expr),+ $(,)? ) => {
            println!(
                concat!( $( ignore!($e, "{:<15}") ),+ ),
                $( $e ),+
            );
        };
    }

        print_row!["Name", "Depth", "Nodes", "Leaves", "Programs"];
        print_row![
            "Forward",
            forward_graph.depth().map(|x| x as i64).unwrap_or(-1),
            forward_graph.n_nodes(),
            forward_graph.n_leaves(),
            forward_graph.n_programs(),
        ];
        print_row![
            "Backward",
            backward_graph.depth().map(|x| x as i64).unwrap_or(-1),
            backward_graph.n_nodes(),
            backward_graph.n_leaves(),
            backward_graph.n_programs(),
        ];
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct Programs(Vec<&'static str>);

    impl Len for Programs {
        fn len(&self) -> usize {
            self.0.len()
        }
    }

    impl Extend<Programs> for Programs {
        fn extend<I: IntoIterator<Item = Programs>>(&mut self, iter: I) {
            iter.into_iter().for_each(|x| {
                self.0.extend_from_slice(&x.0);
            })
        }
    }

    #[test]
    fn insert_actually_modifies_graph() {
        let mut graph: Graph<&str, Programs> = Graph::Nest(Default::default());
        graph.insert("output1", [Programs(vec!["prog1"])]);
        graph.insert("output2", [Programs(vec!["prog2"])]);
        assert_eq!(graph.n_programs(), 2);
        assert_eq!(graph.n_leaves(), 2);
    }

    #[test]
    fn insert_combines_when_it_should() {
        let mut graph: Graph<&str, Programs> = Graph::Nest(Default::default());
        graph.insert("output1", [Programs(vec!["prog1"])]);
        graph.insert("output1", [Programs(vec!["prog2"])]);
        assert_eq!(graph.n_programs(), 2);
        assert_eq!(graph.n_leaves(), 1);
    }

    #[test]
    fn insert_all_with_empty_slice() {
        let mut graph: Graph<&str, Programs> = Graph::Leaf(Programs::default());
        graph.insert_all(&[], [Programs(vec!["prog1"])]);
        graph.insert_all(&[], [Programs(vec!["prog2"])]);
        assert_eq!(graph.n_programs(), 2);
        assert_eq!(graph.n_leaves(), 1);
    }
}
