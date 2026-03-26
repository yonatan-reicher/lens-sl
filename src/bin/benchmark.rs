use lens_sl::{Inst, Word4, Word64, inst};
use std::hint::black_box;
use std::ops::ControlFlow::{self, Break, Continue};
use std::time::Instant;

fn main() {
    for b in benchmarks() {
        let b = black_box(b); // Don't let the compiler optimize the input
        run(&b);
    }
}

macro_rules! dyn_fn_mut {
    ($e:expr) => {{ (&mut $e) as &mut dyn FnMut(_) -> _ }};
}

fn run(b: &Benchmark) {
    let mut found = vec![];
    let (callback, default) = match &b.expected {
        Some(expected) => (
            dyn_fn_mut!(|optimized: Vec<Inst<W>>| {
                found.push(optimized.clone());
                if optimized == *expected {
                    return Break(true);
                }
                Continue(())
            }),
            false,
        ),
        None => (
            dyn_fn_mut!(|optimized: Vec<Inst<W>>| {
                found.push(optimized.clone());
                Break(false)
            }),
            true,
        ),
    };
    // Show that we are starting to run...
    print!("{} - ...", b.name);
    let _ = std::io::Write::flush(&mut std::io::stdout());
    let (success, elapsed) = {
        // Run!
        let started_at = Instant::now();
        let ret = b.optimize(callback);
        let elapsed = started_at.elapsed();
        (ret.break_value().unwrap_or(default), elapsed)
    };
    let mark = if success { "✅" } else { "❌" };
    println!("{} - {mark} {}", b.name, humantime::Duration::from(elapsed));
    if !success {
        if found.is_empty() {
            println!("  Found nothing.");
        } else {
            println!("  Found:");
            for prog in found {
                if prog.is_empty() {
                    println!("  · <empty program>");
                } else {
                    for (i, inst) in prog.into_iter().enumerate() {
                        let c = if i == 0 { '·' } else { ' ' };
                        println!("  {} {}", c, inst);
                    }
                }
            }
        }
    }
}

fn benchmarks() -> impl IntoIterator<Item = Benchmark> {
    [
        Benchmark {
            name: "empty",
            input: vec![],
            expected: None,
        },
        Benchmark {
            name: "double move",
            input: vec![inst!(MovI, 0, 5), inst!(MovI, 0, 3)],
            expected: Some(vec![inst!(MovI, 0, 3)]),
        },
    ]
}

struct Benchmark {
    name: &'static str,
    input: Vec<Inst<W>>,
    expected: Option<Vec<Inst<W>>>,
}

type W = Word64;

impl Benchmark {
    pub fn optimize<T>(&self, mut f: impl FnMut(Vec<Inst<W>>) -> ControlFlow<T>) -> ControlFlow<T> {
        let x = lens_sl::optimize::<W, Word4>(&self.input, &[], &lens_sl::NoTui);
        if let Some(x) = x {
            f(x)?;
        }
        Continue(())
    }
}
