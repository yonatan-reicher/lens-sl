use functionality::Pipe;
use lens_sl::{Algorithm, Config, Inst, OptimizeOutcome, ShouldCancel, Word4, Word32, optimize};
use serde::{Deserialize, Serialize};
use std::env;
use std::ffi::c_int;
use std::fs::{self, File};
use std::hint::black_box;
use std::io::{Read, Write};
use std::ops::ControlFlow::{self, Break, Continue};
use std::os::unix::net::UnixStream;
use std::panic;
use std::path::Path;
use std::process::exit;
use std::sync::Mutex;
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

unsafe extern "C" {
    fn fork() -> c_int;
    fn waitpid(pid: c_int, status: *mut c_int, options: c_int) -> c_int;
    fn _exit(status: c_int) -> !;
}

#[derive(Default)]
struct Options {
    parallel: bool,
    sl: bool,
    timeout: Option<Duration>,
    filter: Filter,
    csv: Option<Mutex<File>>,
    forward_only: bool,
    average_over: Option<u32>,
}

#[derive(Clone, Default)]
enum Filter {
    #[default]
    None,
    Ours,
    Lens,
    Name(String),
}

static O: std::sync::LazyLock<Options> = std::sync::LazyLock::new(parse_options);

fn main() {
    let benchmarks = benchmarks();

    println!("Loaded {} benchmarks.", benchmarks.len());
    for (i, b) in benchmarks.iter().enumerate() {
        println!("{:<3} {}", format!("{}.", i + 1), b.name);
    }

    if O.parallel {
        run_all_parallel(&benchmarks);
    } else {
        run_all_sequential(&benchmarks);
    }
}

fn run_all_sequential(benchmarks: &[Benchmark]) {
    for b in benchmarks {
        let b = black_box(b); // Don't let the compiler optimize the input
        print!("{} - ...\r", b.name);
        let _ = std::io::Write::flush(&mut std::io::stdout());
        let result = run_in_forked_child(b);
        print_result(b, &result);
    }
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
                        timeout: false,
                        elapsed: Duration::ZERO,
                        std: Duration::ZERO,
                        found: vec![],
                        panic_message: Some(panic_payload_to_string(payload)),
                        last_iteration_completion_percent: (0, 0),
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
    // Run!
    let mut already_timed_out = false;
    (0..O.average_over.unwrap_or(1))
        .map(|_| {
            if already_timed_out {
                return BenchmarkResult {
                    success: false,
                    timeout: true,
                    elapsed: Duration::ZERO,
                    std: Duration::ZERO,
                    found: vec![],
                    panic_message: None,
                    last_iteration_completion_percent: (0, 0),
                };
            }
            let mut found = vec![];
            let callback = |optimized: Vec<Inst<W>>| -> ControlFlow<()> {
                found.push(optimized);
                Continue(())
            };

            let ret = b.optimize::<()>(callback);
            let (elapsed, timeout, last_iteration_completion_percent) = match ret {
                Continue((elapsed, timeout, pct)) => (elapsed, timeout, pct),
                Break(()) => unreachable!("benchmark callback never breaks"),
            };
            already_timed_out |= timeout;
            let success = !timeout
                && !found.is_empty()
                && found
                    .iter()
                    .all(|optimized| optimized.len() < b.input.len());
            BenchmarkResult {
                success,
                timeout,
                elapsed,
                std: Duration::ZERO,
                found,
                panic_message: None,
                last_iteration_completion_percent,
            }
        })
        .collect::<Vec<_>>()
        .pipe(|v| {
            let all_success = v.iter().all(|b| b.success);
            let none_success = v.iter().all(|b| !b.success);
            #[rustfmt::skip]
            let success = if all_success { true } else if none_success { false } else { todo!() };
            let all_timeout = v.iter().all(|b| b.timeout);
            let none_timeout = v.iter().all(|b| !b.timeout);
            #[rustfmt::skip]
            let timeout = if all_timeout { true } else if none_timeout { false } else { todo!() };
            #[rustfmt::skip]
            let elapsed = v.iter().map(|b| b.elapsed).sum::<Duration>().mul_f64(1.0 / v.len() as f64);
            let found = v[0].found.clone();
            if !v.iter().all(|b| b.found == found) { todo!() }
            let std = std(v.iter().map(|b| &b.elapsed));
            let last_iteration_completion_percent = v[0].last_iteration_completion_percent;
            if !v.iter().all(|b| b.last_iteration_completion_percent == last_iteration_completion_percent) { todo!() }
            BenchmarkResult {
                success,
                timeout,
                elapsed,
                found,
                std,
                panic_message: None,
                last_iteration_completion_percent,
            }
        })
}

fn print_result(b: &Benchmark, result: &BenchmarkResult) {
    if let Some(message) = &result.panic_message {
        println!("{} - 💥 thread panicked", b.name);
        println!("  Panic: {message}");
        return;
    }

    let mark = if result.timeout {
        "⌛️ timeout"
    } else if result.success {
        "✅"
    } else {
        "❌"
    };
    println!(
        "{} - {mark} {} [{}/{}]",
        b.name,
        humantime::Duration::from(result.elapsed),
        result.last_iteration_completion_percent.0,
        result.last_iteration_completion_percent.1
    );
    if let Some(csv) = &O.csv {
        let csv = &mut csv.lock().unwrap();
        let (name, success, time, std, last_iter_pct) = (
            b.name.as_str(),
            result.success,
            if result.timeout {
                "timeout".to_string()
            } else {
                result.elapsed.as_secs_f64().to_string()
            },
            result.std.as_secs_f64(),
            format!(
                "{}/{}",
                result.last_iteration_completion_percent.0,
                result.last_iteration_completion_percent.1
            ),
        );
        let _ = writeln!(csv, "{name},{success},{time},{std},{last_iter_pct}");
    }
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
    let mut ret = vec![];
    if !matches!(O.filter, Filter::Ours) {
        ret.extend(benchmarks_in_dir("./lens-benchmarks"))
    }
    if !matches!(O.filter, Filter::Lens) {
        ret.extend(benchmarks_in_dir("./our-benchmarks"))
    }
    ret.retain(|b| O.filter.check(b));
    ret.sort_by_key(|b| b.name.clone());
    ret
}

fn benchmarks_in_dir(path: impl AsRef<Path> + std::fmt::Display) -> Vec<Benchmark> {
    let mut benchmarks = Vec::new();
    let Ok(entries) = fs::read_dir(&path) else {
        eprintln!("error: could not read '{path}'");
        exit(1);
    };

    for entry_result in entries {
        let entry = match entry_result {
            Ok(entry) => entry,
            Err(err) => {
                eprintln!("warning: skipping unreadable entry in '{path}': {err}");
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

        benchmarks.push(Benchmark { name, input });
    }

    benchmarks
}

fn is_arm_source_file(path: &Path) -> bool {
    path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("s")
}

struct Benchmark {
    name: String,
    input: Vec<Inst<W>>,
}

#[derive(Serialize, Deserialize)]
struct BenchmarkResult {
    success: bool,
    timeout: bool,
    elapsed: Duration,
    std: Duration,
    found: Vec<Vec<Inst<W>>>,
    panic_message: Option<String>,
    last_iteration_completion_percent: (usize, usize),
}

type W = Word32;

impl Benchmark {
    pub fn optimize<T>(
        &self,
        mut f: impl FnMut(Vec<Inst<W>>) -> ControlFlow<T>,
    ) -> ControlFlow<T, (Duration, bool, (usize, usize))> {
        let algorithm = if O.sl {
            Algorithm::LensSl
        } else {
            Algorithm::Lens
        };
        let should_cancel = match O.timeout {
            None => ShouldCancel::Never,
            Some(d) => ShouldCancel::Timeout(d),
        };
        let result = optimize::<W, Word4>(
            Config {
                algorithm,
                program: &self.input,
                additional_registers: &[],
                additional_immediates: &[],
                should_cancel,
                forward_only: O.forward_only,
            },
            &lens_sl::NoTui,
        );
        let elapsed = result.elapsed;
        let timeout = match result.outcome {
            OptimizeOutcome::Cancelled => true,
            OptimizeOutcome::NoProgram => false,
            OptimizeOutcome::Program(p) => {
                f(p)?;
                false
            }
        };
        Continue((elapsed, timeout, result.last_iteration_completion_percent))
    }
}

fn run_in_forked_child(b: &Benchmark) -> BenchmarkResult {
    let (reader, writer) = match UnixStream::pair() {
        Ok(pair) => pair,
        Err(err) => return BenchmarkResult::failed(format!("failed to create IPC channel: {err}")),
    };
    // SAFETY: We intentionally fork to isolate benchmark panics/crashes from the parent process.
    let pid = unsafe { fork() };
    if pid < 0 {
        return BenchmarkResult::failed(format!(
            "failed to fork benchmark process: {}",
            std::io::Error::last_os_error()
        ));
    }
    if pid == 0 {
        drop(reader);
        let result = run(b);
        let status = match write_benchmark_result(&writer, &result) {
            Ok(()) => 0,
            Err(err) => {
                eprintln!("error: child failed to send benchmark result: {err}");
                1
            }
        };
        // SAFETY: after `fork`, use `_exit` (not `std::process::exit`) so the child terminates
        // without running parent-side teardown/flush/destructor logic.
        unsafe { _exit(status) };
    }
    drop(writer);
    let read_result = read_benchmark_result(&reader);
    let exit_status = wait_for_child(pid);
    match (exit_status, read_result) {
        (Ok(ChildExitStatus::Exited(0)), Ok(result)) => result,
        (Ok(ChildExitStatus::Exited(0)), Err(err)) => BenchmarkResult::failed(format!(
            "child exited successfully but sent no result: {err}"
        )),
        (Ok(ChildExitStatus::Exited(code)), _) => {
            BenchmarkResult::failed(format!("child exited with code {code}"))
        }
        (Ok(ChildExitStatus::Signaled(signal)), _) => {
            BenchmarkResult::failed(format!("child terminated by signal {signal}"))
        }
        (Ok(ChildExitStatus::Other(status)), _) => {
            BenchmarkResult::failed(format!("child exited with unexpected status {status}"))
        }
        (Err(err), _) => {
            BenchmarkResult::failed(format!("failed waiting for child process: {err}"))
        }
    }
}

enum ChildExitStatus {
    Exited(i32),
    Signaled(i32),
    Other(i32),
}

fn wait_for_child(pid: c_int) -> std::io::Result<ChildExitStatus> {
    let mut status = 0;
    // SAFETY: waitpid is called with a child PID returned by fork.
    let ret = unsafe { waitpid(pid, &mut status, 0) };
    if ret < 0 {
        return Err(std::io::Error::last_os_error());
    }
    if wifexited(status) {
        Ok(ChildExitStatus::Exited(wexitstatus(status)))
    } else if wifsignaled(status) {
        Ok(ChildExitStatus::Signaled(wtermsig(status)))
    } else {
        Ok(ChildExitStatus::Other(status))
    }
}

fn wifexited(status: c_int) -> bool {
    (status & 0x7f) == 0
}

fn wexitstatus(status: c_int) -> c_int {
    (status >> 8) & 0xff
}

fn wifsignaled(status: c_int) -> bool {
    let signal = status & 0x7f;
    signal != 0 && signal != 0x7f
}

fn wtermsig(status: c_int) -> c_int {
    status & 0x7f
}

fn write_benchmark_result(
    mut writer: &UnixStream,
    result: &BenchmarkResult,
) -> std::io::Result<()> {
    let payload = postcard::to_stdvec(result).map_err(std::io::Error::other)?;
    let payload_len = u64::try_from(payload.len()).map_err(std::io::Error::other)?;
    writer.write_all(&payload_len.to_le_bytes())?;
    writer.write_all(&payload)?;
    writer.flush()?;
    Ok(())
}

fn read_benchmark_result(mut reader: &UnixStream) -> std::io::Result<BenchmarkResult> {
    let mut len = [0_u8; std::mem::size_of::<u64>()];
    reader.read_exact(&mut len)?;
    let payload_len = usize::try_from(u64::from_le_bytes(len)).map_err(std::io::Error::other)?;
    let mut payload = vec![0_u8; payload_len];
    reader.read_exact(&mut payload)?;
    postcard::from_bytes(&payload).map_err(std::io::Error::other)
}

fn parse_options() -> Options {
    let mut ret = Options::default();
    let mut args = env::args();
    let command = args.next().unwrap_or_else(|| "benchmark".to_string());

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                print_usage(&command);
                std::process::exit(0);
            }
            "--parallel" | "-p" => ret.parallel = true,
            "--sl" => ret.sl = true,
            "--filter" => {
                ret.filter = match args.next().as_deref() {
                    Some("ours") => Filter::Ours,
                    Some("lens") => Filter::Lens,
                    Some("name") => {
                        let name = args.next().unwrap();
                        Filter::Name(name)
                    }
                    None => {
                        eprintln!("error: filter option requires an argument");
                        exit(1);
                    }
                    Some(s) => {
                        eprintln!(
                            "error: filter argument must be either 'ours' or 'lens', but was '{s}'"
                        );
                        exit(1);
                    }
                };
            }
            "--csv" => {
                ret.csv = Some(Mutex::new(match args.next().as_deref() {
                    None => {
                        eprintln!("error: csv option requires an argument");
                        exit(1);
                    }
                    Some(path) => match File::create(path) {
                        Ok(mut f) => {
                            let _ =
                                writeln!(f, "name,success,time(seconds/timeout),std,last-iter-%");
                            f
                        }
                        Err(e) => {
                            eprintln!("error: {e}");
                            exit(1);
                        }
                    },
                }))
            }
            "--forward-only" | "-f" => {
                ret.forward_only = true;
            }
            "--average-over" | "-a" => {
                ret.average_over = Some(match args.next() {
                    None => {
                        eprintln!(
                            "error: average-over option needs a number argument, but had no argument"
                        );
                        exit(1);
                    }
                    Some(n) => match n.parse() {
                        Err(_) => {
                            eprintln!(
                                "error: average-over option needs a number argument, but got '{n}'"
                            );
                            exit(1);
                        }
                        Ok(n) => n,
                    },
                })
            }
            "--timeout" => {
                let Some(t) = args.next() else {
                    eprintln!("error: timeout option needs a time argument, but had no argument");
                    print_usage(&command);
                    exit(1);
                };
                let Ok(t) = t.parse() else {
                    eprintln!("error: timeout option argument must be a number, but was '{t}'");
                    print_usage(&command);
                    exit(1);
                };
                ret.timeout = Some(Duration::from_secs(t));
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
    eprintln!(
        "Usage: {command} [--sl] [--parallel] [--timeout <seconds>] [--filter [ours | lens | name <name>]] [--csv <filename>] [--forward-only | -f] [(--average-over | -a) <n>]"
    );
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

fn std<'a>(i: impl Iterator<Item = &'a Duration> + Clone) -> Duration {
    let n = i.clone().count();
    let avg = i.clone().sum::<Duration>().mul_f64(1.0 / n as f64);
    let sqr_dists = i
        .clone()
        .map(|d| d.abs_diff(avg).as_secs_f64() * d.abs_diff(avg).as_secs_f64());
    let sqr_dists_avg = sqr_dists.sum::<f64>() / n as f64;
    Duration::from_secs_f64(sqr_dists_avg.sqrt())
}

impl Filter {
    pub fn check(&self, b: &Benchmark) -> bool {
        match self {
            Filter::None => true,
            Filter::Ours => true, // TODO
            Filter::Lens => true, // TODO
            Filter::Name(s) => b.name.contains(s.as_str()),
        }
    }
}

impl BenchmarkResult {
    fn failed(message: String) -> Self {
        Self {
            success: false,
            timeout: false,
            elapsed: Duration::ZERO,
            std: Duration::ZERO,
            found: vec![],
            panic_message: Some(message),
            last_iteration_completion_percent: (0, 0),
        }
    }
}
