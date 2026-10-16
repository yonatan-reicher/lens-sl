# Running Rosette / GreenThumb on macOS (Apple Silicon)

The standard installation instructions assume Linux. On an Apple Silicon Mac (M1/M2/M3),
three additional fixes are needed.

## 1. Use the system Z3 instead of the bundled binary

The `rosette/bin/z3` binary shipped with Rosette is a Linux x86_64 ELF binary and
cannot run on macOS. Replace it with a symlink to the Homebrew Z3:

```sh
brew install z3
ln -sf /opt/homebrew/bin/z3 rosette/bin/z3
```

## 2. Switch GreenThumb's solver from KodKod to Z3

GreenThumb explicitly sets KodKod as the solver in two files. KodKod relies on
native `.dylib` libraries compiled for x86_64, which cannot be loaded by an arm64
Java process. Change both occurrences of `kodkod%` to `z3%`:

- `greenthumb/validator.rkt` line ~58:
  ```racket
  (current-solver (new z3%))
  ```
- `greenthumb/symbolic.rkt` line ~120:
  ```racket
  (current-solver (new z3%))
  ```

## 3. Patch Rosette's SMT model parser for newer Z3

Rosette v1.1 was written for an older Z3 whose `get-model` response was wrapped in
a `(model ...)` s-expression. Z3 4.8+ returns the model as a plain list of
`define-fun` entries. Add a second match clause in
`rosette/rosette/solver/smt/smtlib2.rkt` around line 29:

```racket
[(list (== 'model) (list (== 'define-fun) const _ _ val) ...)
 (for/hash ([c const] [v val]) (values c v))]
[(list (list (== 'define-fun) const _ _ val) ...)   ; newer Z3 (4.8+)
 (for/hash ([c const] [v val]) (values c v))]
```

## Note on compiled bytecode caches

After editing any `.rkt` source file, delete Racket's compiled bytecode so it
picks up your changes (Racket uses timestamps to decide whether to recompile, and
those can be unreliable):

```sh
find /path/to/rosette -name "*.zo" -delete
find /path/to/greenthumb -name "*.zo" -delete
```
