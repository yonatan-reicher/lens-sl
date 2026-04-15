use lens_sl::*;

// This binary runs arm programs for us

// TODO: rename this binary...

type W = Word4;
// type U = <W as Word>::Unsigned;

fn main() {
    let program: &[Inst<W>] = &[
        inst!(And, 0, 0, 1; shift Ror(1.into())),
        inst!(Add, 0, 0, 0),
        inst!(Add, 1, 1, 1),
    ];
    let mut initial_state = State::default();
    initial_state[Register(0)] = 9.into();
    initial_state[Register(1)] = 0.into();
    println!("Initial state: {initial_state}");
    let mut state = initial_state;
    let mut i = 0;
    for inst in program {
        i += 1;
        inst.run(&mut state);
        println!("{i:2} │ {inst:20} {state}");
    }
}
