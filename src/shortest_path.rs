//! This module implements a generic graph search for shortest path between two
//! nodes.

use rustc_hash::FxHashMap;
use std::collections::VecDeque;

#[derive(Clone, Debug)]
struct Entry<N, E> {
    previous: N,
    edge: E,
}

#[allow(dead_code)]
pub fn shortest_path<N, E, I>(
    start: &N,
    neighbors: impl Fn(&N) -> I,
    goal: impl Fn(&N) -> bool,
) -> Option<Vec<E>>
where
    N: Eq + std::hash::Hash + Clone,
    E: Clone,
    I: Iterator<Item = (E, N)>,
{
    // Edge case - start is goal.
    if goal(start) {
        return Some(vec![]);
    }

    println!("Starting shortest path search...");

    let mut first_queue = VecDeque::from([start.clone()]);
    let mut second_queue = VecDeque::new();
    let mut previous: FxHashMap<N, Entry<N, E>> = Default::default();
    loop {
        println!(
            "Queues sizes: {}, {}",
            first_queue.len(),
            second_queue.len()
        );
        let current = match first_queue.pop_front() {
            Some(n) => n,
            None if !second_queue.is_empty() => {
                println!("Swapping queues... {}", second_queue.len());
                std::mem::swap(&mut first_queue, &mut second_queue);
                first_queue.pop_front().expect("swapped non-empty queue")
            }
            None => return None,
        };
        for (edge, neighbor) in neighbors(&current) {
            println!("Visiting neighbor...");
            // Is this the goal?
            if goal(&neighbor) {
                let mut path = vec![edge.clone()];
                let mut backtrack = current.clone();
                loop {
                    if backtrack == *start {
                        break;
                    }
                    let Some(entry) = previous.get(&backtrack) else {
                        break;
                    };
                    path.push(entry.edge.clone());
                    backtrack = entry.previous.clone();
                    println!("Backtracking...");
                }
                path.reverse();
                return Some(path);
            }
            // Update if unvisited.
            previous.entry(neighbor.clone()).or_insert_with(|| {
                second_queue.push_back(neighbor);
                Entry {
                    previous: current.clone(),
                    edge,
                }
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn empty_graph_success() {
        assert_eq!(
            shortest_path(&0, |_| std::iter::empty::<(i32, i32)>(), |i| *i == 0),
            Some(vec![])
        );
    }

    #[test]
    pub fn empty_graph_fail() {
        assert_eq!(
            shortest_path(&0, |_| std::iter::empty::<(i32, i32)>(), |i| *i == 1),
            None,
        );
    }

    fn diamond_graph_neighbors(i: i32) -> impl Iterator<Item = (&'static str, i32)> {
        let slice: &[_] = match i {
            0 => &[("0-1", 1), ("0-2", 2)],
            1 => &[("1-4", 4)],
            2 => &[("2-3", 3)],
            3 => &[("3-4", 4)],
            4 => &[("4-5", 5)],
            _ => &[],
        };
        slice.iter().cloned()
    }

    #[test]
    pub fn diamond_graph_fail() {
        assert_eq!(
            shortest_path(&0, |i| diamond_graph_neighbors(*i), |i| *i == 7,),
            None
        );
    }

    #[test]
    pub fn diamond_graph_success() {
        assert_eq!(
            shortest_path(&0, |i| diamond_graph_neighbors(*i), |i| *i == 5,),
            Some(vec!["0-1", "1-4", "4-5"])
        );
    }
}
