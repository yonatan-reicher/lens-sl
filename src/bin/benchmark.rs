use lens_sl::{Inst, Word4, Word64, inst};
use std::env;
use std::fs;
use std::hint::black_box;
use std::ops::ControlFlow::{self, Break, Continue};
use std::panic;
use std::path::Path;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;
use std::time::Instant;

struct Options {
    parallel: bool,
    sl: bool,
}

static O: std::sync::LazyLock<Options> = std::sync::LazyLock::new(parse_options);

fn main() {
    let benchmarks = benchmarks();

    if O.parallel {
        run_all_parallel(&benchmarks);
    } else {
        run_all_sequential(&benchmarks);
    }
}

fn run_all_sequential(benchmarks: &[Benchmark]) {
    for b in benchmarks {
        let b = black_box(b); // Don't let the compiler optimize the input
        print!("{} - ...", b.name);
        let _ = std::io::Write::flush(&mut std::io::stdout());
        let result = run(b);
        print_result(b, &result);
    }
}

macro_rules! dyn_fn_mut {
    ($e:expr) => {{ (&mut $e) as &mut dyn FnMut(_) -> _ }};
}

fn run_all_parallel(benchmarks: &[Benchmark]) {
    // Parallel runs can trigger assertion panics for some benchmark inputs.
    // Keep output readable by suppressing default panic backtraces and reporting panics per benchmark.
    let previous_panic_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    thread::scope(|scope| {
        let (tx, rx) = mpsc::channel::<(usize, BenchmarkResult)>();
        for (index, b) in benchmarks.iter().enumerate() {
            let b = black_box(b); // Don't let the compiler optimize the input
            let tx = tx.clone();
            scope.spawn(move || {
                let result = match panic::catch_unwind(|| run(b)) {
                    Ok(result) => result,
                    Err(payload) => BenchmarkResult {
                        success: false,
                        elapsed: Duration::ZERO,
                        found: vec![],
                        panic_message: Some(panic_payload_to_string(payload)),
                    },
                };
                let _ = tx.send((index, result));
            });
        }
        drop(tx);

        for _ in 0..benchmarks.len() {
            let (index, result) = rx.recv().expect("benchmark worker disconnected");
            print_result(&benchmarks[index], &result);
        }
    });
    panic::set_hook(previous_panic_hook);
}

fn run(b: &Benchmark) -> BenchmarkResult {
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

    // Run!
    let started_at = Instant::now();
    let ret = b.optimize(callback);
    let elapsed = started_at.elapsed();
    let success = ret.break_value().unwrap_or(default);
    BenchmarkResult {
        success,
        elapsed,
        found,
        panic_message: None,
    }
}

fn print_result(b: &Benchmark, result: &BenchmarkResult) {
    if let Some(message) = &result.panic_message {
        println!("{} - 💥 thread panicked", b.name);
        println!("  Panic: {message}");
        return;
    }

    let mark = if result.success { "✅" } else { "❌" };
    println!(
        "{} - {mark} {}",
        b.name,
        humantime::Duration::from(result.elapsed)
    );
    if !result.success {
        if result.found.is_empty() {
            println!("  Found nothing.");
        } else {
            println!("  Found:");
            for prog in &result.found {
                if prog.is_empty() {
                    println!("  · <empty program>");
                } else {
                    for (i, inst) in prog.iter().enumerate() {
                        println!("  {:>3}│ {}", i + 1, inst);
                    }
                }
            }
        }
    }
}

fn benchmarks() -> Vec<Benchmark> {
    [
        Benchmark {
            name: "empty".to_string(),
            input: vec![],
            expected: None,
        },
        Benchmark {
            name: "double move".to_string(),
            input: vec![inst!(MovI, 0, 5), inst!(MovI, 0, 3)],
            expected: Some(vec![inst!(MovI, 0, 3)]),
        },
    ]
    .into_iter()
    .chain(benchmarks_in_programs_dir())
    .collect()
}

fn benchmarks_in_programs_dir() -> Vec<Benchmark> {
    let mut benchmarks = Vec::new();
    let Ok(entries) = fs::read_dir("./programs") else {
        eprintln!("warning: could not read ./programs directory");
        return benchmarks;
    };

    for entry_result in entries {
        let entry = match entry_result {
            Ok(entry) => entry,
            Err(err) => {
                eprintln!("warning: skipping unreadable entry in ./programs: {err}");
                continue;
            }
        };

        let path = entry.path();
        if !is_arm_source_file(&path) {
            continue;
        }

        let src = match fs::read_to_string(&path) {
            Ok(src) => src,
            Err(err) => {
                eprintln!(
                    "warning: skipping {}: failed to read ({err})",
                    path.display()
                );
                continue;
            }
        };

        let input = match lens_sl::parse(&src) {
            Ok(program) => program,
            Err(err) => {
                eprintln!("warning: skipping {}: parse failed ({err})", path.display());
                continue;
            }
        };

        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| path.display().to_string());

        benchmarks.push(Benchmark {
            name,
            input,
            expected: None,
        });
    }

    benchmarks
}

fn is_arm_source_file(path: &Path) -> bool {
    path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("s")
}

struct Benchmark {
    name: String,
    input: Vec<Inst<W>>,
    expected: Option<Vec<Inst<W>>>,
}

struct BenchmarkResult {
    success: bool,
    elapsed: Duration,
    found: Vec<Vec<Inst<W>>>,
    panic_message: Option<String>,
}

type W = Word64;

impl Benchmark {
    pub fn optimize<T>(&self, mut f: impl FnMut(Vec<Inst<W>>) -> ControlFlow<T>) -> ControlFlow<T> {
        let optimize = if O.sl {
            lens_sl::optimize_sl::<W, Word4>
        } else {
            lens_sl::optimize::<W, Word4>
        };
        let x = optimize(&self.input, vec![], vec![], &lens_sl::NoTui);
        if let Some(x) = x {
            f(x)?;
        }
        Continue(())
    }
}

fn parse_options() -> Options {
    let mut ret = Options {
        parallel: false,
        sl: false,
    };
    let mut args = env::args();
    let command = args.next().unwrap_or_else(|| "benchmark".to_string());

    for arg in args {
        match arg.as_str() {
            "--parallel" | "-p" => ret.parallel = true,
            "--sl" => ret.sl = true,
            "--help" | "-h" => {
                print_usage(&command);
                std::process::exit(0);
            }
            _ => {
                eprintln!("error: unknown argument '{arg}'");
                print_usage(&command);
                std::process::exit(1);
            }
        }
    }

    ret
}

fn print_usage(command: &str) {
    eprintln!("Usage: {command} [--sl] [--parallel]");
}

fn panic_payload_to_string(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(message) = payload.downcast_ref::<String>() {
        message.clone()
    } else if let Some(message) = payload.downcast_ref::<&'static str>() {
        (*message).to_string()
    } else {
        "non-string panic payload".to_string()
    }
}
