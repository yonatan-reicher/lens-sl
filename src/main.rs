use lens_sl::OptimizeResult;
#[allow(unused_imports)]
use lens_sl::{
    Algorithm, Config, LiveValue, NoTui, OptimizeOutcome, Register, ShouldCancel, Tui, Word4,
    Word8, Word32, inst, optimize,
};

fn main() {
    let args: &mut Vec<String> = &mut std::env::args().collect();
    let sl = parse_flag(args, "--sl");
    let forward_only = parse_flag(args, "--forward-only");
    let no_tui = parse_flag(args, "--no-tui");
    let h = parse_flag(args, "--help") || parse_flag(args, "-h");

    if args.len() > 2 || h {
        eprintln!(
            "Usage: {} [--sl] [--forward-only] [--no-tui] [PROGRAM_PATH]",
            args[0]
        );
        std::process::exit(1);
    }

    let (program, live_out) = if let Some(path) = args.get(1) {
        let src = std::fs::read_to_string(path).unwrap_or_else(|err| {
            eprintln!("Failed to read program file '{}': {err}", path);
            std::process::exit(1);
        });
        let program = lens_sl::parse(&src).unwrap_or_else(|err| {
            eprintln!("Failed to parse program file '{}': {err}", path);
            std::process::exit(1);
        });
        // `.s.info` is expected to live next to the input program (same path + `.info`).
        let info_path = format!("{path}.info");
        let live_out = match lens_sl::info_from_file(&info_path) {
            Ok(values) => values
                .into_iter()
                // Accept numeric (`0`) and register-style (`r0`) live-out entries.
                .filter_map(|v| match v {
                    LiveValue::Num(n) => u8::try_from(n).ok().map(Register),
                    LiveValue::Name(name) => name
                        .strip_prefix('r')
                        .and_then(|s| s.parse::<u8>().ok().map(Register)),
                })
                .collect(),
            // For now, missing/invalid info files do not fail the run.
            Err(_err) => vec![],
        };
        (program, live_out)
    } else {
        (
            vec![
                inst!(AddI, 0, 0, 5),
                inst!(AddI Eq, 1, 0, 1),
                inst!(Mul Eq, 1, 0, 1),
                inst!(Orr, 0, 0, 1),
                inst!(AddI Eq, 1, 0, 1),
                inst!(AddI, 1, 0, 1),
                inst!(Mul, 1, 0, 1),
                inst!(AddI Eq, 1, 0, 1),
                inst!(AddI Eq, 1, 0, 1),
            ],
            vec![],
        )
    };

    // Keep parsed live-out info available until `optimize` accepts it directly.
    let _live_out = live_out;

    let config = Config {
        algorithm: if sl {
            Algorithm::MonocleV3
        } else {
            Algorithm::Lens
        },
        program: &program,
        additional_registers: &[],
        additional_immediates: &[],
        should_cancel: ShouldCancel::Never,
        forward_only,
    };

    let OptimizeResult {
        outcome,
        elapsed,
        last_iteration_completion_percent,
    } = if !no_tui {
        let tui = Tui::default();
        let p = optimize::<Word32, Word4>(config, &tui);
        tui.close();
        p
    } else {
        optimize::<Word32, Word4>(config, &NoTui)
    };
    match outcome {
        OptimizeOutcome::Cancelled => {
            println!("Cancelled!");
        }
        OptimizeOutcome::NoProgram => {
            println!("No equivalent program found");
        }
        OptimizeOutcome::Program(p) => {
            println!("Optimized program:");
            for inst in p {
                println!("{inst}");
            }
        }
    }
    println!("Time: {}", humantime::Duration::from(elapsed));
    println!(
        "Last iteration completion: {}/{}",
        last_iteration_completion_percent.0, last_iteration_completion_percent.1
    );
}

fn parse_flag(args: &mut Vec<String>, flag: &str) -> bool {
    args.iter()
        .position(|a| *a == flag)
        .map(|i| args.remove(i))
        .is_some()
}
