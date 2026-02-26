# Copilot Instructions for lens-sl

## What this project is

`lens-sl` is a **superoptimizer** for an ARM-like ISA, implemented in Rust. Given a program and a set of input-output examples, it finds a shorter equivalent program by exhaustive bidirectional search. The algorithm is based on the Lens paper.

## Build, Test, and Run

```sh
cargo build
cargo test                          # all tests (including property tests)
cargo test <test_name>              # single test, e.g. `cargo test basic_extend_test`
cargo test -- --ignored             # run ignored (slow) tests
cargo run                           # default binary: runs the optimizer on a hardcoded example
cargo run --bin arm                 # run the ARM program interpreter binary
cargo fmt                           # format (respects rustfmt.toml)
```

The `proptest` property tests compile with `opt-level = 3` via the test profile to keep them fast.

## Architecture

The synthesis pipeline in `optimize<WT, WS>()` (`main_loop.rs`):
1. **Reduce** the target program to a smaller word size `WS` for faster search (`reduce_bit_width.rs`)
2. **Collect** registers and immediates actually used in the program and test cases (`collect_registers.rs`)
3. **Enumerate** instructions over the reduced space (`enumerate.rs`)
4. **Bidirectional search**: grow a forward graph (program prefixes → output states) and a backward graph (postfixes → states), trying to connect them through a single middle instruction (`graph.rs`, `main_loop.rs`)
5. **Oracle** checks candidate programs for correctness and returns counter-examples to refine the search (`oracle/test_cases.rs`, `oracle/smt.rs`)
6. **Extend** a found reduced program back to full word size and verify with the full oracle

The module dependency diagram is in `src/lib.rs`.

## Key Types and Conventions

### `Word` trait (`word.rs`)
`Word` is a **phantom type** — it carries no data, just type-level word size. Actual integer data lives in `Word::Unsigned` (e.g. `u4`, `u64`) and `Word::Signed`. The concrete word types are `Word4`, `Word8`, `Word64`. Generics use `WT` = target word, `WS` = synthesis (smaller) word.

### ISA definition (`isa.rs`)
Instructions are defined in a single table via the `define_instructions!` macro, which generates `OpCode`, `arg_types()`, and `OpCode::ALL`. To add an instruction, add a row to the table.

The `inst!` macro constructs instructions: `inst!(AddI, 0, 1, 5)` or `inst!(AddI Eq, 0, 1, 5)` (with condition code).

`CondCode::COUNT = 6` — only the first 6 condition codes (Al, Eq, Ne, Cs, Cc, Mi) are enumerated during synthesis, even though more are defined.

### `Enumerator` (`enumerate.rs`)
Uses `unsafe { std::mem::transmute::<u8, OpCode>(n) }` to iterate over enum variants. This relies on variants being repr-sequentially numbered from 0. When adding variants to `OpCode` or modifying `CondCode::COUNT`, maintain this invariant.

### `Programs<I>` (`programs.rs`)
A rope-like lazy tree: `Program(vec)`, `List(vec)`, or `Concat(Rc<Programs>, I)`. The `Concat` variant lets many programs share a common prefix/suffix without cloning. Materializing a program requires traversal. Prefer `for_each_ref` / `try_for_each_ref` over `to_vec` to avoid unnecessary allocation.

### `Graph<State, Programs>` (`graph.rs`)
A trie keyed by program outputs on successive test cases. `Leaf(Programs)` = all programs with the same outputs on all test cases so far. `Nest(FxHashMap<State, Graph>)` = one level of discrimination by one test case's output.

### `Oracle<P, S>` (`oracle.rs`)
Returns `Ok(())` (equivalent) or `Err((input, expected_output))` (counter-example). Two implementations:
- `TestCasesOracle`: checks a fixed set of input/output pairs — used for synthesis
- `oracle/smt.rs`: SMT-based via cvc5 — incomplete (WIP, several `todo!()` calls)

### `Reducer<WBig, WSmall>` (`reduce_bit_width.rs`)
Tracks which large constants reduce to the same small constant, so that when a reduced program is found, it can be **extended** back to enumerate all matching full-size programs.

## Formatting
`rustfmt.toml` sets `reorder_modules = false` — keep modules in their declared order (top-to-bottom readable).

## Testing patterns
- Inline `#[cfg(test)]` modules in each source file
- Property tests use `proptest` with `#[property_test]` attribute from the `proptest` crate
- `tests/bad.rs` contains regression/crash tests; slow ones are marked `#[ignore]`
- `proptest-regressions/` stores failing seeds for deterministic replay
