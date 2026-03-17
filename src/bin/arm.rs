use lens_sl::*;

// This binary runs arm programs for us

// TODO: rename this binary...

type W = Word4;
// type U = <W as Word>::Unsigned;

fn main() {
    let program: &[Inst<W>] = &[
        inst!(AddI, 0, 0, 5),
        inst!(Orr, 0, 1, 0),
    ];
    let mut initial_state = State::default();
    initial_state.set_register(Register(0), 6.into());
    initial_state.set_register(Register(1), 13.into());
    println!("Initial state: {initial_state}");
    let mut state = initial_state;
    let mut i = 0;
    for inst in program {
        i += 1;
        inst.run(&mut state);
        println!("{i:2} │ {inst:20} {state}");
    }
}
