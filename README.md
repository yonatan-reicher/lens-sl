## Currently Working On

On branch "main",
Reimplementing State struct and squashing cerrors. That is part of an effort to
simplify the code. That is actually a low priority thing, and I should pivot
towards working on implementating backwards search into the algorithm. That
includes going through the main loop function filling in holes, and then writing
down what needs to be changes. That should all happen in a new branch.

## TODO
Why does it take so much time to exit? Is clearing memory slowing us down? (We
could leak if we want to, then we could exit very fast...)
State liveness mask in both concrete and smt.
Better representation for State? Maybe something with Rc::make_mut could be
cute.
