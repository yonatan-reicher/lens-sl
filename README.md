# Lens + Separation Logic

This project is us trying to take Lens and improve it by taking inspiration from
ideas of separation logic.

Lens is the algorithm presented in "Scaling Up Super-Optimization".

The project is currently a Rust rewrite of Lens. We chose to rewrite it to make
it faster and easier to maintain and expand.

## Useful Commands

- `cargo test --lib` - Run unit tests for the library.
- `cargo test` - Run all tests (excluding ignored).
- `cargo test-slow` - Run the ignored tests, which are slower and output things
  to the screen.
- `cargo run <name>` - Optimize one of the programs in the `programs/` directory.
- `cargo run --bin arm` - Run the arm interpreter binary.
- `cargo benchmark` - Run the benchmarks!
- `cargo run` - Run the main entry point.

## More Markdown Files

- [[changes.md]] - The changes in our Lens reimplementation (The regular one).
- [[lens-sl.md]] - Explanation for the new algorithm, Lens SL.
- [[currently-working-on.md]]
- [[todo.md]] - Tasks to maybe remember.
