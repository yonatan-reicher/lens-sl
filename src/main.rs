#[allow(unused_imports)]
use lens_sl::{NoTui, Tui};
#[allow(unused_imports)]
use lens_sl::{Register, Word4, Word8, Word64, inst, optimize};

fn main() {
    let tui = Tui::default();
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
        [Register(0), Register(1)],
        [0.into(), 1.into(), 20.into(), 8.into(), 11.into(), 92.into(), 93.into()],
        &tui, // */ &NoTui,
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
