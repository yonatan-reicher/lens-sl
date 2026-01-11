use rustc_hash::FxHashMap;
use std::fmt::Display;
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

    pub fn into_iter(self) -> impl Iterator<Item = (Vec<S>, P)> {
        GraphIterator::new(self)
    }
}

impl<S, I> Graph<S, Vec<I>>
where
    S: Eq + Hash + Display,
    I: Display,
{
    pub fn pretty_print_lines(&self) -> Vec<String> {
        match self {
            Graph::Leaf(items) => items
                .iter()
                .flat_map(|item| {
                    if item.is_empty() {
                        return vec!["· <empty program>".to_string()];
                    }
                    item.iter()
                        .enumerate()
                        .map(|(i, inst)| {
                            let prefix = if i == 0 { '·' } else { ' ' };
                            format!("{prefix} {inst}")
                        })
                        .collect::<Vec<_>>()
                })
                .collect(),
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

struct GraphIterator<S, P> {
    graph_stack: Vec<Graph<S, P>>,
    state_stack: Vec<S>,
}

impl<S, P> GraphIterator<S, P> {
    fn new(graph: Graph<S, P>) -> Self {
        Self {
            graph_stack: vec![graph],
            state_stack: vec![],
        }
    }
}

impl<S: Clone + Eq + Hash, P> Iterator for GraphIterator<S, P> {
    type Item = (Vec<S>, P);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(top) = self.graph_stack.pop() {
            match top {
                Graph::Leaf(mut programs) => {
                    if let Some(prog) = programs.pop() {
                        let ret = Some((self.state_stack.clone(), prog));
                        if !programs.is_empty() {
                            self.graph_stack.push(Graph::Leaf(programs));
                        }
                        return ret;
                    } else {
                        self.state_stack.pop();
                    }
                }
                Graph::Nest(mut hash_map) => {
                    if let Some((state, sub_graph)) = hash_map.iter_mut().next() {
                        let state = state.clone();
                        let sub_graph = std::mem::take(sub_graph);
                        hash_map.remove(&state);
                        self.graph_stack.push(Graph::Nest(hash_map));
                        self.state_stack.push(state);
                        self.graph_stack.push(sub_graph);
                    } else {
                        self.state_stack.pop();
                    }
                }
            }
        }
        None
    }
}
