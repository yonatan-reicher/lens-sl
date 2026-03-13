## Currently Working On

Saved the Backwards Map object to a file when created for the first time. Next
time I want to run some tests on the enumeration code, maybe even in it's own
environment without, as a standalone test. I want to print the minimum and
maximum depths of the tree at lots of different points to understand the real
deal. My suspicions indicate that when building forward, they build the whole
graph, which makes no sense to me.
After that, I should implement the new backwards graph type - it should just be
a vector of hash-maps from outputs to sets of programs. Vector of the length of
the amount of tests.
Will also need to change the insert for programs to check if the program
already exists.

## TODO
! Implement the real backward search graph data structure - a vector of size n
(amount of tests) with hash-maps from outputs to sets of programs.
! On insert, skip if program already exists.
Why does it take so much time to exit? Is clearing memory slowing us down? (We
could leak if we want to, then we could exit very fast...)
State liveness mask in both concrete and smt.
Better representation for State? Maybe something with Rc::make_mut could be
cute.
