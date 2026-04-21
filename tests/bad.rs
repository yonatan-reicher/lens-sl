use lens_sl::{inst, optimize, Algorithm, Config, NoTui, OptimizeOutcome, Word4, Word64};

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
    );
    assert_eq!(p.outcome, OptimizeOutcome::NoProgram);
}
