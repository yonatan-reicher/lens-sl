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
