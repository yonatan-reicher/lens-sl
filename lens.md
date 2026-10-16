# Lens

This document aims to explain Lens, the super-optimizer, and GreenThumb, the
tool that runs it. This includes only the parts relevant to our goal of
integrating separation logic into Lens.

A lot of information here will be stated more clearly and less precisely in the
`lens.pdf` and `greenthumb.pdf` papers.

## Installation

To setup what we have on a new machine, checkout `install.md`.
For macOS (Apple Silicon), also see `rosette-mac-os.md`.

## Usage

We are currently working with the `arm` architecture, so things will be inside
the `arm` folder. Run a test with:
```sh
racket optimize.rkt --hybrid -p -c 8 -t 3600 programs/p14_floor_avg_o0.s
```
Run with only a single enumerative search instance:
```sh
racket optimize.rkt --enum --linear -c 1 -t 30 programs/p14_floor_avg_o0.s
```

## Overview of Lens

Lens is implemented in Racket inside of a project called Green Thumb. Green
Thumb is a just a tool to Lens and benchmark it.

Lens is not implemented as a single file, it is spread across multiple files and
classes. The relevant files are:
- `forwardbackward.rkt` - Main implementation of the search graph.
- `arm/arm-forwardbackward.rkt` - arm-specific configuration over the search
  graph.
- `arm/arm-machine.rkt` - Definition of the Arm machine, including opcodes and
  certain instruction semantics.
- `path.rkt` - Just defines a variable `srcpath` that points to the project
  directory.

Green Thumb itself runs multiple super-optimizer instances concurrently by
launching processes that it calls "drivers". Each driver communicates with the
rest about the current best program in some way. This means that certain things
in the output of drivers and things like that won't be printed on the screen,
but will go into a log or something like that.

### `forwardbackward.rkt`

Contains the main implementation of the search graph. The search graph is made
up of two variables: `classes` and `backward-classes`.

The variables are nested hash tables, as described in the original paper.
Basically, when you have 5 example input-output pairs, you have a 5-deep hash
table. When you have only 1 example, you have a 1-deep hash table. Each hash
table with depth `i` maps from the output state of example `i` (or the input
state of example `i` for `backward-classes`) to either another hash table (if
`i` is not the last example), or a set of programs (if `i` is the last example).
In the code itself, the set of programs is represented as a list or a vector of
programs, where each program is an object of type `concat`. A `concat` is an
object constructed like this `(concat #() (inst <opcodes> <args>))`. We think
the vector (the first argument) is a program prefix, but we have only see it
empty. The `concat` constructor is defined as a struct.

`inst` is a type also defined as a struct. Sometimes it's arguments are strings
(called "String IR" form) and sometimes they are vectors of integers (called
"Encoded IR" form). The arguments can also be `#f` when the should be
synthesized. Instructions with ints can be printed nicely with `decode-inst` and
`print-syntax-inst` from `arm-printer%`.

The integers in the opcode vector correspond to indices in the `opcodes` vectors
in the `machine%` class. The integers in the argument vector correspond either
to actual numbers, or numbered registers (e.g., r0 = 0, r13 = 13, etc).

In an `inst` with strings, the opcode names map to something slightly different
from actual assembler names. For example, `add` would be an addition for two
registers, while `add#` would be addition with an immediate.

## `arm/arm-forwardbackward.rkt`

Configuration over `forwardbackward.rkt`. Specifically, contains the behaviour
of liveness (that is, the liveness mask that appears in entries).

## `arm/arm-machine.rkt`

`define-instruction-class` is the most interesting function in this file. It
works like this:
```racket
(define-instruction-class 'rrr-commute-shf
  (list '(add and orr eor) cond-opcodes shf-inst-reg)
  #:required '(#t #f #f)
  #:args '((reg reg reg) () (reg)) #:ins '((1 2) (z 0) (3)) #:outs '(0) #:commute '(1 . 2))
```
Here `'rrr-commute-shf` is the name of the instruction class. The second
argument is a list of op-codes. `cond-opcodes` and `shf-inst-reg` are variables
declared elsewhere, and the final list evaluates to
`'((add and ...) (eq ne ...) (asr lsl ...))`. In Arm, each instruction has both
an opcode, a condition code, and possibly a shift operation. So a possible
op-code in this instruction class might be `'(add ne lsl)`. The `#:required`
means that the first group is always required, while the second and third could
be missing. `#:args` indicates the arguments and their types, and splits them
into groups corresponding to the op-codes. So the first group has three register
arguments, the second group has no arguments and the third group has another
register argument. `#:ins` indicates which arguments are inputs for each group.
`(1 2)` means that the first group's inputs are the registers at argument
indices 1 and 2, if the second group is present then the zero flag is an input,
and the 0-index register is an input too (because if the instruction is a not
executed because of the condition, then the instruction leaves the 0-index
register unchanged). For the third group, the register at argument index 3 is an
additional input. `#:outs` indicates what state is modified by the instruction.
In this case, only the 0-index register. We have `#:commute '(1 . 2)`, which
indicates that the order of registers at index 1 and 2 can be swapped without
nothing changing.

## Racket

Some useful racket things to know:
1. Use `(display <value>)` to pretty-print values. Use
  `(pretty-display <value>)` if you print big values. Use `(printf ...)` for
  simple text formatting but do not use it for user types. Note that this
  `printf` uses different formatting specifiers from `c`'s `printf`.

## Other Optimizations

These are some optimizations that we've seen but haven't been documented in the
original paper:
- Maybe: don't check commutative instructions twice
- Use ids for instructions and ids for programs to reduce memory

## Todo

- [ ] Update the graph printing and the python script to be inline with the way
  the reimplementation does it now.
- [ ] Printing the Graph
  - [x] Printing instructions
    - [x] Read more of the documentation
