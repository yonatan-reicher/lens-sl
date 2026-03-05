use arbitrary_int::traits::Integer;
use lens_sl::*;

// This binary runs arm programs for us

// TODO: rename this binary...

type W = Word4;
// type U = <W as Word>::Unsigned;

fn main() {
    let program: &[Inst<W>] = &[
        inst!(AddI, 0.as_(), 0.as_(), 5.as_()),
        inst!(Orr, 0.as_(), 1.as_(), 0.as_()),
    ];
    let mut initial_state = State::default();
    initial_state.set_register(Register(0), 6.as_());
    initial_state.set_register(Register(1), 13.as_());
    println!("Initial state: {initial_state}");
    let mut state = initial_state;
    let mut i = 0;
    for inst in program {
        i += 1;
        inst.run(&mut state);
        println!("{i:2} │ {inst:20} {state}");
    }
}
