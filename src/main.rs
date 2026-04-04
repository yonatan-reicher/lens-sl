#[allow(unused_imports)]
use lens_sl::{LiveValue, Register, Word4, Word8, Word64, inst, optimize};
#[allow(unused_imports)]
use lens_sl::{NoTui, Tui};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 2 {
        eprintln!("Usage: {} [PROGRAM_PATH]", args[0]);
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

    let tui = Tui::default();
    let p = optimize::<Word64, Word4>(
        &program,
        vec![], // additional_registers
        vec![], // additional_immediates
        &tui,   // */ &NoTui,
    );
    tui.close();
    let Some(p) = p else {
        println!("No equivalent program found");
        return;
    };
    println!("Optimized program:");
    for inst in p {
        println!("{inst}");
    }
}
