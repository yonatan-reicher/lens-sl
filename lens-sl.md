
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

Because we aren't doing PBE, we need to consider what happens when we add a
counter example. Because of that we use a tree, just like Lens' graph. It's
graph only stores the input/output (depending on which graph), but we actually
want to store pairs of inputs and outputs at each layer. Where in Lens the layer
depth gave us the input, we just get a sort of general concept of "which counter
example this relates to".

Or, do we want to actually just use the graphs and do something different? who
knows!

## Separration Logic

How this is refered to in the code is a liveness mask. Each possible state of
the machine has a mask associated with it which tells you which variables are
currently considered set. When you apply an instrucion on a state, it may
expand the liveness mask, but it may never remove something from the mask that
was already there.

Another thing to consider, is that sometimes we need to shrink the mask, when
talking about running from a general state, we can look at what part of the
state is read and generate a mask from that.

