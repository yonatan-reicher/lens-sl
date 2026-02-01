#[allow(unused_imports)]
use lens_sl::{Register, Word64, Word8, Word4, inst, optimize};

fn main() {
    let p = optimize::<Word64, Word4>(
        &[
            // inst!(AddI, 0, 1, 5),
            // inst!(AddI, 0, 0, 5),
            // inst!(AddI Eq, 1, 0, 1),
            // inst!(Mul Eq, 1, 0, 1),
            // inst!(Orr, 0, 0, 1),
            // inst!(AddI Eq, 1, 0, 1),
            // inst!(Eor, 1, 0, 1),
            inst!(SubI, 3, 0, 1),
            inst!(Orr, 3, 3, 0),
            inst!(AddI, 3, 3, 1),
            inst!(And, 0, 3, 0),
        ],
        &[
            &[(Register(0), 0), (Register(3), 0)],
            &[(Register(0), 64), (Register(3), 0)],
            &[(Register(0), 64), (Register(3), 124)],
            &[(Register(0), 4), (Register(3), 24)],
            &[(Register(0), 54), (Register(3), 24)],
            &[(Register(0), 54), (Register(3), 34)],
            &[(Register(0), 54), (Register(3), 34)],
            &[(Register(0), 0), (Register(3), 0)],
            &[(Register(0), 1), (Register(3), 0)],
        ],
    );
    println!("Optimized program:");
    for inst in p {
        println!("{inst}");
    }
}
