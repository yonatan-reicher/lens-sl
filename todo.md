- [ ] ISA shift with registers
- [ ] Algorithm enum
- [ ] Algorithm Config struct
- [ ] Initializing all sub-states in optimize_sl
- [ ] Look what needs to be done for optimize_sl to support many registers
- [ ] If the above aren't enough, why is optimize_sl exploding?

Old algorithm
- [ ] Implement the real backward search graph data structure - a vector of size
  n (amount of tests) with hash-maps from outputs to sets of programs.
- [x] On insert, skip if program already exists.  - Original doesn't do this
- [ ] Why does it take so much time to exit? Is clearing memory slowing us down? (We could leak if we want to, then we could exit very fast...)
- [x] State liveness mask in both concrete and smt. - Liveness in SMT makes not sense.
- [x] Expand ISA and correct it's behavior.
- [ ] Reduced program with SMT holes for the SMT to find!
- [ ] Collect information.

## On Collecting Information

What information is interesting for us to collect? We are interested in how many
programs have been found to be reduced-equivalent, but not really equivalent.
This should let us finetune the search. We are interested in the graph of depth
over time, and in the graph of number of leaves over time, and programs over-all
over time.

