The Lens SL first draft is going well. Restarting from the Lens code was a good
decision. Thinking about the following:
- How to add concatenations in a good way, avoiding duplicates
- Building backwards with liveness.
- Unify code responsible for instruction semantics.

I want to make a plan on how to approach each of those.

## Unifying
This is the most technical out of the three. Basically, the instruction
semantics are in the following functions: `run_instruction`,
`run_instruction_symbolic`, `Inst::read_mask`. Each one of these are related to
exactly one state-like type: `State`, `SymbolicState`, `state::Masked`. Each of
them can have it's register read, or set, and same with flags. We can't call the
trait 'State', but we can call it something like 'StateTrait' or 'StateLike'
