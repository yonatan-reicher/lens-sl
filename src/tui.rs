use crate::Direction;
use humantime;
use crate::len::Len;
use derive_more::Display;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::{Index, IndexMut};
use std::sync::mpsc::{Receiver, RecvError, Sender, channel};
use std::thread::{JoinHandle as ThreadHandle, spawn as spawn_thread};
use std::time::{Duration, Instant};

// ====== The Interface ======

/// State - state of the ISA, from the graph
pub trait TuiHook<Graph, State> {
    // Phases
    fn searching(&self);
    fn expanding(&self, direction: Direction);
    fn progress(&self, a: usize, b: usize);
    // Graph
    fn report_graph(&self, which_graph: Direction, graph: Graph);
    /// Called when expanding the graph by one instruction forward.
    fn found_counter_example(&self, input: State, output: State);
}

// ====== Null Implementation ======

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NoTui;

#[rustfmt::skip]
impl<G, S> TuiHook<G, S> for NoTui {
    // Phases
    fn searching(&self) {}
    fn expanding(&self, _: Direction) {}
    fn progress(&self, _: usize, _: usize) {}
    // Graph
    fn report_graph(&self, _: Direction, _: G) {}
    /// Called when expanding the graph by one instruction forward.
    fn found_counter_example(&self, _: S, _: S) {}
}

// ====== Actual Implementation ======

pub struct Tui<State> {
    _io_thread: ThreadHandle<()>,
    channel: Sender<Msg<State>>,
}

#[derive(Debug)]
enum Msg<State> {
    // Phases
    Searching(Instant),
    Expanding(Direction, Instant),
    Progress(usize, usize),
    // Graph
    GraphState(Direction, GraphState),
    /// Called when expanding the graph by one instruction forward.
    FoundCounterExample(State, State),
}

impl<State> Tui<State> {
    fn send(&self, msg: Msg<State>)
    where
        State: Debug,
    {
        match self.channel.send(msg) {
            Ok(()) => (),
            Err(std::sync::mpsc::SendError(msg)) => {
                eprintln!(
                    "ERROR: The debug-printer thread appears to have died and could not \
                    recieve the message: {msg:?}"
                );
                std::process::exit(1);
            }
        }
    }
}

impl<State: Debug + Display + Send + 'static> Default for Tui<State> {
    fn default() -> Self {
        let (sender, receiver) = channel();
        let io_thread = spawn_thread(io_thread_main(receiver));
        Self {
            _io_thread: io_thread,
            channel: sender,
        }
    }
}

impl<Graph, State: Debug> TuiHook<Graph, State> for Tui<State>
where
    Graph: Into<GraphState>,
{
    // Phases
    fn searching(&self) {
        self.send(Msg::Searching(Instant::now()));
    }
    fn expanding(&self, direction: Direction) {
        self.send(Msg::Expanding(direction, Instant::now()));
    }
    fn progress(&self, a: usize, b: usize) {
        self.send(Msg::Progress(a, b))
    }
    // Graph
    fn report_graph(&self, d: Direction, g: Graph) {
        self.send(Msg::GraphState(d, g.into()));
    }
    /// Called when expanding the graph by one instruction forward.
    fn found_counter_example(&self, input: State, output: State) {
        self.send(Msg::FoundCounterExample(input, output));
    }
}

fn io_thread_main<State: Debug + Display>(channel: Receiver<Msg<State>>) -> impl FnOnce() {
    move || {
        let mut state = IoThreadState::default();
        let mut msg_queue = vec![];
        loop {
            get_all_messages_or_block(&channel, &mut msg_queue).unwrap_or_else(|err| {
                panic!("{err}");
            });
            for msg in msg_queue.drain(..) {
                msg.run(&mut state)
            }
            clear_screen();
            // println!("Messages: {msg_queue:?}");
            println!("{state}");
            std::thread::sleep(std::time::Duration::from_secs_f64(0.16));
        }
    }
}

#[derive(Debug)]
struct IoThreadState<State> {
    pub iteration: usize,
    pub phase: Option<(Phase, Instant)>,
    pub progress: (usize, usize),
    pub phases: Vec<(Phase, usize, Duration)>,
    pub forward_graph: GraphState,
    pub backward_graph: GraphState,
    pub forward_len: usize,
    pub backward_len: usize,
    pub counter_examples: Vec<(State, State)>,
    // Just for debugging
    pub total_messages_received: usize,
    pub last_msg: Option<String>,
}

#[derive(Debug, Default)]
struct GraphState {
    layers: Vec<LayerInfo>,
    n_progs: usize,
    n_leaves: usize,
    min_depth: usize,
}

#[derive(Clone, Debug)]
struct LayerInfo {
    /// The amount of nodes in the layer.
    n_nodes: usize,
}

#[derive(Clone, Copy, Debug, Display, PartialEq, Eq, Hash)]
pub enum Phase {
    #[display("Search")]
    Search,
    #[display("Expand {_0}")]
    Expand(Direction),
}

impl<State> Msg<State> {
    pub fn run(self, state: &mut IoThreadState<State>)
    where
        State: Debug,
    {
        state.total_messages_received += 1;
        state.last_msg = Some(format!("{self:?}"));
        match self {
            Msg::Searching(t) => state.push_phase(Phase::Search, t),
            Msg::Expanding(dir, t) => state.push_phase(Phase::Expand(dir), t),
            Msg::Progress(a, b) => state.progress = (a, b),
            Msg::GraphState(d, graph_state) => state[d] = graph_state,
            Msg::FoundCounterExample(input, output) => {
                state.counter_examples.push((input, output));
            }
        }
    }
}

impl<State> IoThreadState<State> {
    fn push_phase(&mut self, p: Phase, t: Instant) {
        if let Some((prev_p, prev_t)) = self.phase {
            let dur = t - prev_t;
            self.phases.push((prev_p, self.iteration, dur));
            match prev_p {
                Phase::Search => (),
                Phase::Expand(Direction::Forward) => self.forward_len += 1,
                Phase::Expand(Direction::Backward) => self.backward_len += 1,
            }
        }
        self.phase = Some((p, t));
        if p == Phase::Search {
            self.iteration += 1;
        }
    }
}

impl<State> Default for IoThreadState<State> {
    fn default() -> Self {
        Self {
            iteration: Default::default(),
            phase: Default::default(),
            progress: (0, 1),
            phases: Default::default(),
            forward_graph: Default::default(),
            backward_graph: Default::default(),
            forward_len: 0,
            backward_len: 0,
            counter_examples: Default::default(),
            total_messages_received: Default::default(),
            last_msg: Default::default(),
        }
    }
}

impl<S> Index<Direction> for IoThreadState<S> {
    type Output = GraphState;
    fn index(&self, d: Direction) -> &Self::Output {
        match d {
            Direction::Forward => &self.forward_graph,
            Direction::Backward => &self.backward_graph,
        }
    }
}

impl<S> IndexMut<Direction> for IoThreadState<S> {
    fn index_mut(&mut self, d: Direction) -> &mut Self::Output {
        match d {
            Direction::Forward => &mut self.forward_graph,
            Direction::Backward => &mut self.backward_graph,
        }
    }
}

// How the TUI actually looks

impl<S: Display> Display for IoThreadState<S> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // Iteration 4. Phase Expand Forward.
        write!(f, "Iteration {}.", self.iteration)?;
        if let Some((p, _)) = self.phase {
            write!(f, " Phase {p} [{}/{}].", self.progress.0, self.progress.1)?;
        }
        writeln!(f)?;
        // Lengths 3, 2.
        writeln!(f, "Lengths {}, {}.", self.forward_len, self.backward_len)?;
        writeln!(f)?;
        //   1      254     1024   52682
        //   · ───── · ───── · ───── ◆    1189402
        for d in [Direction::Forward, Direction::Backward] {
            let graph = &self[d];
            for l in graph.layers.iter() {
                write!(f, "{:^6}  ", l.n_nodes)?;
            }
            writeln!(f)?;
            write!(f, " ")?;
            for (i, _) in graph.layers.iter().enumerate() {
                if i == graph.layers.len() - 1 {
                    writeln!(
                        f,
                        " ◆    {} programs  {} leaves",
                        graph.n_progs, graph.n_leaves
                    )?;
                } else if i == graph.min_depth {
                    write!(f, " ◆ ─────")?;
                } else {
                    write!(f, " · ─────")?;
                }
            }
        }
        writeln!(f)?;
        writeln!(f, "Times:")?;
        for (p, i, t) in &self.phases {
            writeln!(f, "  Iteration {i} Phase {:<15}   {}", p.to_string(), humantime::Duration::from(*t))?;
        }
        writeln!(f)?;
        writeln!(f, "Counter Examples:")?;
        for (i, o) in &self.counter_examples {
            let i = i.to_string();
            let o = o.to_string();
            writeln!(f, "  {i:<28} {o:<28}")?;
        }
        writeln!(f)?;
        writeln!(
            f,
            "Last message received {}.",
            self.last_msg.as_deref().unwrap_or("None")
        )?;
        writeln!(
            f,
            "Total messages received {}.",
            self.total_messages_received
        )?;
        Ok(())
    }
}

/// Get 1 or more messages from a channel.
fn get_all_messages_or_block<T>(channel: &Receiver<T>, out: &mut Vec<T>) -> Result<(), RecvError> {
    // Limit to some amount of messages, to make sure this function always returns.
    const LIMIT: usize = 1_000_000;
    let first = channel.recv()?;
    out.push(first);
    out.extend(channel.try_iter().take(LIMIT));
    Ok(())
}

fn clear_screen() {
    print!("{}[2J", 27 as char);
    print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
}

use crate::graph::Graph;
impl<'g, S, P: Len> From<&'g Graph<S, P>> for GraphState {
    fn from(g: &'g Graph<S, P>) -> Self {
        let mut ret = GraphState {
            layers: vec![],
            n_progs: 0,
            n_leaves: 0,
            min_depth: usize::MAX,
        };
        recurse(g, &mut ret, 0);
        return ret;

        fn recurse<S, P: Len>(g: &Graph<S, P>, ret: &mut GraphState, depth: usize) {
            if ret.layers.len() <= depth {
                ret.layers.push(LayerInfo { n_nodes: 0 });
            }
            ret.layers[depth].n_nodes += 1;
            match g {
                Graph::Leaf(p) => {
                    ret.n_progs += p.len();
                    ret.n_leaves += 1;
                    ret.min_depth = ret.min_depth.min(depth);
                }
                Graph::Nest(hash_map) => {
                    for sub_graph in hash_map.values() {
                        recurse(sub_graph, ret, depth + 1)
                    }
                }
            }
        }
    }
}
