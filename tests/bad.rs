use lens_sl::{Register, Word4, Word64, inst, optimize};

/// This test currently crashes.
#[test]
#[ignore] // Ignored because it takes a long time.
fn bad_case() {
    let p = optimize::<Word64, Word4>(
        &[
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

/// This test currently crashes.
#[test]
fn bad_case_2() {
    let p = optimize::<Word64, Word4>(
        &[
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
            inst!(Mul Eq, 1, 0, 1),
            inst!(Orr, 0, 0, 1),
            inst!(AddI Eq, 1, 0, 1),
        ],
        &[&[(Register(0), 0), (Register(1), 0)]],
    );
    println!("Optimized program:");
    for inst in p {
        println!("{inst}");
    }
}
