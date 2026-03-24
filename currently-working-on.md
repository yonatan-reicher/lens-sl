Working on building backwards feels like the logical step ahead, but Hila
specifically requested I reuse different parts of the program independently of
being backwards and forwards. For that I need to work on concatenation. It
should not be hard to implement naively, the hard part is doing it without
duplicates.

## What do I mean by duplicates?

given the following set of programs:
1. add r0, r0, 1
2. add r0, r0, r0
3. mov r1, 1

Concatenating whatever we can might give us:
1.  add r0, r0, 1
2.  add r0, r0, r0
3.  mov r1, 1
4.  add r0, r0, 1 ; add r0, r0, 1
5.  add r0, r0, 1 ; add r0, r0, r0
6.  add r0, r0, 1 ; mov r1, 1
7.  add r0, r0, r0 ; add r0, r0, 1
8.  add r0, r0, r0 ; add r0, r0, r0
9.  add r0, r0, r0 ; mov r1, 1
10. mov r1, 1 ; add r0, r0, 1
11. mov r1, 1 ; add r0, r0, r0
12. mov r1, 1 ; mov r1, 1

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


## What else?

The Lens SL first draft is going well. Restarting from the Lens code was a good
decision. Thinking about the following:
- How to add concatenations in a good way, avoiding duplicates
- Building backwards with liveness.
- Unify code responsible for instruction semantics.
