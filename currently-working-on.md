 I have 2 things that I'm currently working on:
- How to add concatenations in a good way, avoiding duplicates
- Building backwards with liveness.
I am focusing on the first one.

## Concatenations

In Lens, we are using a structure that's pretty much:
```rust
enum Programs {
    EmptyProgram,
    Concat(Vec<Programs>, Instruction)
}
```

So the natural thing to try for concatenation would be

```rust
enum Programs {
    EmptyProgram,
    Concat(Vec<Programs>, Programs)
}
```

Two further optimizations would be to store these programs objects in an arena
of their own, and to calculate that Vec lazely. If you combine them you can
actually achieve something really cool, by storing just a pointer to the index
of the last program object, you have a slice of all programs created, which you
can iterate over lazely.

Anddd you can of course also memoize! I am not sure but I think this could
really improve results.


Anyway what I really should do is start with a naive implementation.
Now, every time we add another round of concatenations like this we are going to
add many duplicates, and I am convinced this will be a very bad idea. One thing
we can try, is just checking for each program if we already encountered it. We
can just check if it's in the graph. This sounds fine, but the problem is you
are still doing a bunch of duplicate work, because you are creating all those
duplicates, even if you leave them out of the graph. This, I would guess, is
going to majorly slow down the expansion phase, which is already explosively
slow.

You can also use a hash map. That shouldn't be such a bad idea, especially as
you are storing all the programs anyway. More over, you could use iterators to
index into the hashmap, allowing you to check if a program exists without even
creating it. Then, in the hash-map, you could also store data related to the
program, such as where it is in the tree, if that is relevant, or an ID. That ID
can refer to that program, maybe as an index in a flat array where it's
instructions are actually stored. Then we can have the programs be completely
flat, with almost no indirection.


- Programs sets in the graph represented by indices into a flat array called IDs.
- In the flat array they are stored as actions on indices (e.g. Concat(ID 52,
  ID 23)).
- Each individual program is also referenced by 

