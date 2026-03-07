## Currently Working On

Tried starting to implement backward searching, we'll have to restructure some
things because you have multiple maps for backward searching. Might need to
even restructure the Graph type. But that's for later. I tried focusing on
Inst::run_backward but had a hard time because I made things too sophisticated.
Next time, I should start BackwardMap from scratch - make a function that
generates all states, make Enumerator support generating all instructions, and
we're done with that and can continue.

## TODO
Why does it take so much time to exit? Is clearing memory slowing us down? (We
could leak if we want to, then we could exit very fast...)
State liveness mask in both concrete and smt.
Better representation for State? Maybe something with Rc::make_mut could be
cute.
