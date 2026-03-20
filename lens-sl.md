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

- Connection with the SMT solver for program equivalence is kept open. In the
  original algorithm, the SMT solver is at least completely restarted after each
  program. That is, the original program is reintroduced to it.

- Different Expansion strategy. When a graph is expanded, uses the outputs that
  are stored for each program, and inserts with those, but when a conflict with
  the depths of a branch happens when inserting, it discards the deeper outputs.
  We are not sure what the original algorithm did with these depth-conflicts.

## New Algorithm

The idea of the new algorithm is to take inspiration from Sobeq. Sobeq does a
synthesis search for an Ast of a language with expressions. Here, in ARM, we
don't have expressions, only instructions. In Sobeq's javascript, the atomic
programs are just variable expression. Then you have constructors for method
calls, operators and whatever. Here, in ARM, we'll have all possible
instructions as our atomic programs, and our constructors will just be the
concatenation operator.

**`InitBank`** - `InitBank` is the name of the procedure in Sobeq that takes
inserts new atomic programs into the bank under new equivalence classes. Our
version will do the same ig.

## TODO

Expand ISA and correct it's behavior.
Reduced program with SMT holes for the SMT to find!
Collect information.

## On Collecting Information

What information is interesting for us to collect? We are interested in how many
programs have been found to be reduced-equivalent, but not really equivalent.
This should let us finetune the search. We are interested in the graph of depth
over time, and in the graph of number of leaves over time, and programs over-all
over time.

