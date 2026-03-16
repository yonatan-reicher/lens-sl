use lens_sl::{NoTui, Register, Word4, Word64, inst, optimize};

// These are ignored because they take a long time.

#[test]
#[ignore]
fn bad_case() {
    let p = optimize::<Word64, Word4>(
        &[
            inst!(SubI, 3, 0, 1),
            inst!(Orr, 3, 3, 0),
            inst!(AddI, 3, 3, 1),
            inst!(And, 0, 3, 0),
        ],
        &[
            &[(Register(0), 0.into()), (Register(3), 0.into())],
            &[(Register(0), 64.into()), (Register(3), 0.into())],
            &[(Register(0), 64.into()), (Register(3), 124.into())],
            &[(Register(0), 4.into()), (Register(3), 24.into())],
            &[(Register(0), 54.into()), (Register(3), 24.into())],
            &[(Register(0), 54.into()), (Register(3), 34.into())],
            &[(Register(0), 54.into()), (Register(3), 34.into())],
            &[(Register(0), 0.into()), (Register(3), 0.into())],
            &[(Register(0), 1.into()), (Register(3), 0.into())],
        ],
        &NoTui,
    );
    assert_eq!(p, None);
}

#[test]
#[ignore]
fn bad_case_2() {
    let p = optimize::<Word64, Word4>(
        &[
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
            inst!(Mul Eq, 1, 0, 1),
            inst!(Orr, 0, 0, 1),
            inst!(AddI Eq, 1, 0, 1),
        ],
        &[&[(Register(0), 0.into()), (Register(1), 0.into())]],
        &NoTui,
    );
    assert_eq!(
        p,
        Some(vec![
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
            inst!(Orr, 0, 0, 1)
        ])
    );
}
