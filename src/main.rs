use lens_sl::{Register, inst, optimize};


fn main() {
    let p = optimize(
        &[
            inst!(AddI, 0, 1, 5),
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
        ],
        &[
            &[(Register(0), 0), (Register(1), 10)],
            &[(Register(0), 0), (Register(1), 20)],
        ],
    );
    println!("Optimized program:");
    for inst in p {
        println!("{inst:?}");
    }
}
