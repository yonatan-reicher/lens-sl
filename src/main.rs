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
            &[(Register(0), 0.into()), (Register(1), 0.into())],
            &[(Register(0), 1.into()), (Register(1), 0.into())],
            &[(Register(0), 0.into()), (Register(1), 1.into())],
            &[(Register(0), 1.into()), (Register(1), 1.into())],
            &[(Register(0), 20.into()), (Register(1), 1.into())],
            &[(Register(0), 8.into()), (Register(1), 1.into())],
            &[(Register(0), 93.into()), (Register(1), 1.into())],
            &[(Register(0), 92.into()), (Register(1), 11.into())],
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
