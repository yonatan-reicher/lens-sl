#[allow(unused_imports)]
use lens_sl::{Tui, NoTui};
#[allow(unused_imports)]
use lens_sl::{Register, Word64, Word8, Word4, inst, optimize};

fn main() {
    let p = optimize::<Word64, Word4>(
        &[
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
        &[
            &[(Register(0), 0), (Register(1), 0)],
            &[(Register(0), 1), (Register(1), 0)],
            &[(Register(0), 0), (Register(1), 1)],
            &[(Register(0), 1), (Register(1), 1)],
            &[(Register(0), 20), (Register(1), 1)],
            &[(Register(0), 8), (Register(1), 1)],
            &[(Register(0), 93), (Register(1), 1)],
            &[(Register(0), 92), (Register(1), 11)],
        ],
        &Tui::default(), // */ &NoTui,
    );
    let Some(p) = p else {
        println!("No equivalent program found");
        return;
    };
    println!("Optimized program:");
    for inst in p {
        println!("{inst}");
    }
}
