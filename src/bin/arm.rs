use lens_sl::*;

// This binary runs arm programs for us

// TODO: rename this binary...

type W = Word64;

#[derive(Clone, Copy, Debug, Default)]
struct S {
    registers: [u64; 16],
    flags: Flags,
}

impl State<W> for S {
    fn get_register(&self, r: Register) -> u64 {
        self.registers[r.0 as usize]
    }
    fn set_register(&mut self, r: Register, x: u64) {
        self.registers[r.0 as usize] = x;
    }
    fn get_flags(&self) -> Flags {
        self.flags
    }
    fn set_flags(&mut self, f: Flags) {
        self.flags = f;
    }
}

impl std::fmt::Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "registers: {:?} flags: {:?}", &self.registers[..5], self.flags)?;
        Ok(())
    }
}

fn main() {
    let program = &[
        inst!(AddI, 0, 0, 5),
        inst!(AddI Eq, 1, 0, 1),
        inst!(Mul Eq, 1, 0, 1),
        inst!(Orr, 0, 0, 1),
        inst!(AddI Eq, 1, 0, 1),
    ];
    let initial_state = S {
        registers: [
            /* r0  = */ 1, /* r1  = */ 0, /* r2  = */ 0, /* r3  = */ 0,
            /* r4  = */ 0, /* r5  = */ 0, /* r6  = */ 0, /* r7  = */ 0,
            /* r8  = */ 0, /* r9  = */ 0, /* r10 = */ 0, /* r11 = */ 0,
            /* r12 = */ 0, /* r13 = */ 0, /* r14 = */ 0, /* r15 = */ 0,
        ],
        flags: Flags::empty(),
    };
    println!("Initial state: {initial_state}");
    let mut state = initial_state;
    let mut i = 0;
    for inst in program {
        i += 1;
        inst.run(&mut state);
        println!("{i:2} │ {inst:20} {state}");
    }
}
