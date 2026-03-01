use std::fmt::{self, Display, Formatter};
use std::sync::mpsc::{Receiver, RecvError, Sender, channel};
use std::thread::{JoinHandle as ThreadHandle, spawn as spawn_thread};

// ====== The Interface ======

pub trait DebugPrinter {
    /// Called when reaching a leaf, a set of programs that pass all the current counter-examples.
    fn visiting_leaf<T>(&self, n_programs: usize, action: impl FnOnce() -> T) -> T;
    fn visiting_program<T>(&self, program: Vec<String>, action: impl FnOnce() -> T) -> T;
    /// Called when building a part of the graph (calculating new outputs).
    fn building_inner_node<T>(&self, n_programs: usize, action: impl FnOnce() -> T) -> T;
    fn building_program(&self);
    /// Called when reaching an inner node, which is associated with some input and holds the
    /// sub-graphs and the outputs that all programs in each sub-graph gave.
    fn visiting_inner_node<T>(&self, n_sub_graphs: usize, action: impl FnOnce() -> T) -> T;
    /// Called when expanding the graph by one instruction forward.
    fn expanding<T>(&self, action: impl FnOnce() -> T) -> T;
    fn found_counter_example(&self, input: String, output: String);
}

// ====== Null Implementation ======

pub struct EmptyDebugPrinter;

#[rustfmt::skip]
impl DebugPrinter for EmptyDebugPrinter {
    fn visiting_leaf<T>(&self, _: usize, f: impl FnOnce() -> T) -> T { f() }
    fn visiting_program<T>(&self, _: Vec<String>, f: impl FnOnce() -> T) -> T { f() }
    fn building_inner_node<T>(&self, _: usize, f: impl FnOnce() -> T) -> T { f() }
    fn building_program(&self) {}
    fn visiting_inner_node<T>(&self, _: usize, f: impl FnOnce() -> T) -> T { f() }
    fn expanding<T>(&self, f: impl FnOnce() -> T) -> T { f() }
    fn found_counter_example(&self, _: String, _: String) {}
}

// ====== Actual Implementation ======

pub struct DebugPrinterImpl {
    io_thread: ThreadHandle<()>,
    channel: Sender<Msg>,
}

#[derive(Debug)]
enum Msg {
    VisitingLeaf { n_programs: usize },
    DoneVisitingLeaf,
    VisitingProgram(Vec<String>),
    DoneVisitingProgram,
    BuildingInnerNode { n_programs: usize },
    BuildingProgram,
    DoneBuildingInnerNode,
    VisitingInnerNode { n_sub_graphs: usize },
    DoneVisitingInnerNode,
    Expanding,
    DoneExpanding,
    FoundCounterExample { input: String, output: String },
}

impl DebugPrinterImpl {
    fn send(&self, msg: Msg) {
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

impl Default for DebugPrinterImpl {
    fn default() -> Self {
        let (sender, receiver) = channel();
        let io_thread = spawn_thread(io_thread_main(receiver));
        Self {
            io_thread,
            channel: sender,
        }
    }
}

impl DebugPrinter for DebugPrinterImpl {
    fn visiting_leaf<T>(&self, n_programs: usize, f: impl FnOnce() -> T) -> T {
        self.send(Msg::VisitingLeaf { n_programs });
        let r = f();
        self.send(Msg::DoneVisitingLeaf);
        r
    }

    fn visiting_program<T>(&self, program: Vec<String>, f: impl FnOnce() -> T) -> T {
        self.send(Msg::VisitingProgram(program));
        let r = f();
        self.send(Msg::DoneVisitingProgram);
        r
    }

    fn building_inner_node<T>(&self, n_programs: usize, f: impl FnOnce() -> T) -> T {
        self.send(Msg::BuildingInnerNode { n_programs });
        let r = f();
        self.send(Msg::DoneBuildingInnerNode);
        r
    }

    fn building_program(&self) {
        self.send(Msg::BuildingProgram);
    }

    fn visiting_inner_node<T>(&self, n_sub_graphs: usize, f: impl FnOnce() -> T) -> T {
        self.send(Msg::VisitingInnerNode { n_sub_graphs });
        let r = f();
        self.send(Msg::DoneVisitingInnerNode);
        r
    }

    fn expanding<T>(&self, f: impl FnOnce() -> T) -> T {
        self.send(Msg::Expanding);
        let r = f();
        self.send(Msg::DoneExpanding);
        r
    }

    fn found_counter_example(&self, input: String, output: String) {
        self.send(Msg::FoundCounterExample { input, output });
    }
}

fn io_thread_main(channel: Receiver<Msg>) -> impl FnOnce() {
    move || {
        let mut state = IoThreadState::default();
        let mut msg_queue = vec![];
        loop {
            msg_queue.clear();
            get_all_messages_or_block(&channel, &mut msg_queue).unwrap_or_else(|err| {
                panic!("{err}");
            });
            let n_msgs_received = msg_queue.len();
            for msg in &msg_queue {
                msg.run(&mut state)
            }
            clear_screen();
            println!("{n_msgs_received} messages received.");
            // println!("Messages: {msg_queue:?}");
            println!("{state}");
            std::thread::sleep(std::time::Duration::from_secs_f64(1. / 24.));
        }
    }
}

#[derive(Debug, Default)]
struct IoThreadState {
    pub total_messages_received: usize,
    pub node_stack: Vec<InnerNodeInfo>,
    pub leaf: Option<LeafInfo>,
    pub expanding: bool,
    pub building: Option<(usize, usize)>,
    pub counter_examples: Vec<(String, String)>,
    pub forward_length: usize,
    pub program: Option<Vec<String>>,
    pub last_msg: Option<String>,
}

#[derive(Clone, Copy, Debug, Default)]
struct InnerNodeInfo {
    n_sub_graphs: usize,
    /// Includes the currently visited sub-graph.
    n_sub_graphs_visited: usize,
}

#[derive(Clone, Copy, Debug, Default)]
struct LeafInfo {
    n_programs: usize,
}

impl Msg {
    pub fn run(&self, state: &mut IoThreadState) {
        state.total_messages_received += 1;
        state.last_msg = Some(format!("{self:?}"));
        match self {
            &Msg::VisitingLeaf { n_programs } => {
                state.leaf = Some(LeafInfo { n_programs });
                if let Some(node) = state.node_stack.last_mut() {
                    node.n_sub_graphs_visited += 1;
                }
            }
            Msg::DoneVisitingLeaf => state.leaf = None,
            Msg::VisitingProgram(p) => state.program = Some(p.clone()),
            Msg::DoneVisitingProgram => state.program = None,
            &Msg::BuildingInnerNode { n_programs } => state.building = Some((1, n_programs)),
            Msg::BuildingProgram => {
                if let Some((a, _)) = &mut state.building {
                    *a += 1;
                }
            }
            Msg::DoneBuildingInnerNode => state.building = None,
            &Msg::VisitingInnerNode { n_sub_graphs } => {
                if let Some(node) = state.node_stack.last_mut() {
                    node.n_sub_graphs_visited += 1;
                }
                state.node_stack.push(InnerNodeInfo {
                    n_sub_graphs,
                    n_sub_graphs_visited: 0,
                });
            }
            Msg::DoneVisitingInnerNode => {
                state.node_stack.pop();
            }
            Msg::Expanding => {
                state.node_stack.clear();
                state.expanding = true;
                state.forward_length += 1;
            }
            Msg::DoneExpanding => state.expanding = false,
            Msg::FoundCounterExample { input, output } => {
                state.counter_examples.push((input.clone(), output.clone()));
            }
        }
    }
}

impl Display for IoThreadState {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f)?;
        writeln!(
            f,
            "Forward Length is {}. Got {} messags so far.",
            self.forward_length, self.total_messages_received
        )?;
        writeln!(
            f,
            "Last message received: {}",
            self.last_msg.as_deref().unwrap_or("nothing")
        )?;
        for &InnerNodeInfo {
            n_sub_graphs,
            n_sub_graphs_visited,
        } in &self.node_stack
        {
            writeln!(f, "◆ [{n_sub_graphs_visited}/{n_sub_graphs}]")?;
        }
        if self.expanding {
            writeln!(f, "Expanding...")?;
        }
        if let Some(LeafInfo { n_programs }) = self.leaf {
            writeln!(f, "Looking at a leaf with {n_programs} programs.")?;
        }
        if let Some(program) = &self.program {
            writeln!(f, "Looking at the program:")?;
            for l in program {
                writeln!(f, "  {l}")?;
            }
        }
        if let Some((a, b)) = self.building {
            writeln!(f, "Building [{a}/{b}]")?;
        }
        writeln!(f)?;
        writeln!(f, "Counter Examples:")?;
        for (i, o) in &self.counter_examples {
            writeln!(f, "  {i:<38} {o:<38}")?;
        }
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
