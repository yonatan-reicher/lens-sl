# Lens + Separation Logic

This project is us trying to take Lens and improve it by taking inspiration from
ideas of separation logic.

Lens is the algorithm presented in "Scaling Up Super-Optimization".

The project is currently a Rust rewrite of Lens. We chose to rewrite it to make
it faster and easier to maintain and expand.

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

## Currently Working On

Had some bad progress on starting from the Sobeq pseudocode, so I'm starting
over from my Lens implementation.

## TODO

Old algorithm
- [ ] Implement the real backward search graph data structure - a vector of size
  n (amount of tests) with hash-maps from outputs to sets of programs.
- [ ] On insert, skip if program already exists.
- [ ] Why does it take so much time to exit? Is clearing memory slowing us down?
  (We could leak if we want to, then we could exit very fast...)
- [ ] State liveness mask in both concrete and smt.
- [ ] Expand ISA and correct it's behavior.
- [ ] Reduced program with SMT holes for the SMT to find!
- [ ] Collect information.

## On Collecting Information

What information is interesting for us to collect? We are interested in how many
programs have been found to be reduced-equivalent, but not really equivalent.
This should let us finetune the search. We are interested in the graph of depth
over time, and in the graph of number of leaves over time, and programs over-all
over time.

