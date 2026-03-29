Old algorithm
- [ ] Implement the real backward search graph data structure - a vector of size
  n (amount of tests) with hash-maps from outputs to sets of programs.
- [ ] On insert, skip if program already exists.
- [ ] Why does it take so much time to exit? Is clearing memory slowing us down? (We could leak if we want to, then we could exit very fast...)
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

