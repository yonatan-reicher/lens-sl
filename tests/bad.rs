use lens_sl::{Algorithm, Config, NoTui, Word4, Word64, inst, optimize};

// These are ignored because they take a long time.

#[test]
#[ignore]
fn bad_case() {
    let p = optimize::<Word64, Word4>(
        Config {
            algorithm: Algorithm::Lens,
            program: &[
                inst!(SubI, 3, 0, 1),
                inst!(Orr, 3, 3, 0),
                inst!(AddI, 3, 3, 1),
                inst!(And, 0, 3, 0),
            ],
            ..Config::default()
        },
        &NoTui,
    )
    .unwrap();
    assert_eq!(p, None);
}

#[test]
#[ignore]
fn bad_case_2() {
    let p = optimize::<Word64, Word4>(
        Config {
            algorithm: Algorithm::Lens,
            program: &[
                inst!(AddI, 0, 0, 5),
                inst!(AddI Eq, 1, 0, 1),
                inst!(Mul Eq, 1, 0, 1),
                inst!(Orr, 0, 0, 1),
                inst!(AddI Eq, 1, 0, 1),
            ],
            ..Config::default()
        },
        &NoTui,
    )
    .unwrap();
    assert_eq!(
        p,
        Some(vec![
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
            inst!(Orr, 0, 0, 1)
        ])
    );
}
