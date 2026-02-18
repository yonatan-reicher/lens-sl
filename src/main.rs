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
        ],
        &[
            &[(Register(0), 0), (Register(1), 0)],
        ],
    );
    println!("Optimized program:");
    for inst in p {
        println!("{inst}");
    }
}
