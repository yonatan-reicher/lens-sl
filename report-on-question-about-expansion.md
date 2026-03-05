Let's see what seems wrong with the algorithm, that I can't explain.

Let's start with an empty forward graph and ignore the backward graph. We'll
start with a single counter example (i₁, o₂). The forward graph starts as a
single leaf that holds all the program prefixes we're checking.
```

                                      Leaf

```

Now we start a searching phase, which means that we run all the prefixes on i₁.
Now the graph looks like this:
```
                                     i₁
                                    ━━━
                                     a
                                   ┌───Leaf₁
                                   │ b
                                   ├───Leaf₂
                                   │ c
                                 ●─┼───Leaf₃
                                   .
                                   .
                                   .
```
First leaf are prefixes with output a on i₁, the second is prefixes with output
b, and so on.

Now we go through each leaf one-by-one, and look for one with a "good" output
(an output that we can connect to the backward graph). Let's say that b is the
first "good" output. When we reach it, we try every prefix. Let's say that none
were an actual solution, so we got counter examples. Let's say that we have
exactly one new counter example, (i₂, o₂). We don't update the entire graph, we
update only when we reach a leaf. And we don't restart now, we actually continue
from b. So now the graph will look like this.
```
                                 i₁        i₂
                                ━━━       ━━━
                                 a
                               ┌───Leaf₁
                               │ b         d
                               ├───Leaf₂ ┌───Leaf₃₁
                               │ c       │ e
                             ●─┼────●────┼───Leaf₃₂
                               .         │
                               .         .
                               .         .
```
Now we expanded every leaf with a "good" output on i₁ and wrote down it's output
on i₂.

For this example, we continued like this and reached the next expansion phase.
This involves creating a new graph. Call the old one G and the new one G'. For
every possible instruction "inst", we visit every leaf, and insert
`{ prefix ++ inst | prefix ∈ leaf }` into G'. This is where I have a problem -
do we:
1. Make G' a single leaf with all the new prefixes.
2. Insert using the outputs we wrote down, running them one instruction forward
3. Run everything on all the counter examples we stored.
The racket code looks like it's doing 2, and that's what looks problematic to
me. Back to the example - Let's say that the prefixes from Leaf₁ and from Leaf₃₂
have the same outputs on i₁. That means they go in the same new Leaf' in G', but
what depth does it have? When inserting Leaf₁, it creates Leaf' with depth 1 (a
single output). But when inserting Leaf₃₂, it expects Leaf' to have depth 2. I
couldn't find where in the code they account for this.
