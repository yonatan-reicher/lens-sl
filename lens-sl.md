# Lens + Separation Logic

This document aims to explain and document the current state of the Lens rewrite
We are rewriting Lens in Rust to be faster and more maintainable, and to try to
change the algorithm! We are still not sure how!

For information on the original project, look at `lens.md`.

## Useful Commands

- `cargo test --lib` - Run unit tests for the library.
- `cargo test` - Run all tests (excluding ignored).
- `cargo run --bin arm` - Run the arm interpreter binary.
- `cargo run` - Run the main entry point.

## Changes

Some changes in the algorithm that we made.

- When a new counter example is found, immediately restarts the "Connect And
  Refine" phase. In the original algorithm, when reaching a leaf, the algorithm
  tries all the programs in the leaf, and finds a counter example for each one.
  Then, it adds all the counter examples to the state at once. I found this to
  slow down the search by a lot, as some leaves have thousands of programs in
  them, even at shallow depths like k = 3.

- Allocations have been reduced. Tried to reduce allocations and reuse
  allocations in key places.

- TODO: Check if this actually correct.
  Connection with the SMT solver for program equivalence is kept open. In the
  original algorithm, the SMT solver is at least completely restarted after each
  program. That is, the original program is reintroduced to it.

## Currently Working On

main-loop-testing
smt
state-cloner
unifying symbolic and concrete execution

## TODO

Expand ISA and correct it's behavior.
Backwards search.
Build forward only one layer deeper, not the entire way.
Reduced program with SMT holes for the SMT to find!
Collect information.
Make SMT code more efficient, by giving variables for each step.

## On Collecting Information

What information is interesting for us to collect? We are interested in how many
programs have been found to be reduced-equivalent, but not really equivalent.
This should let us finetune the search. We are interested in the graph of depth
over time, and in the graph of number of leaves over time, and programs over-all
over time.

