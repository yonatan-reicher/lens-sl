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
    type Num = u64;
    type Bool = bool;

    fn get_register(&self, r: Register) -> u64 {
        self.registers[r.0 as usize]
    }
    fn set_register(&mut self, r: Register, x: u64) {
        self.registers[r.0 as usize] = x;
    }

    fn get_flags_raw(&self) -> (bool, bool, bool, bool) {
        (
            self.flags.contains(Flags::Z),
            self.flags.contains(Flags::N),
            self.flags.contains(Flags::C),
            self.flags.contains(Flags::V),
        )
    }
    fn set_flags_raw(&mut self, (z, n, c, v): (bool, bool, bool, bool)) {
        self.flags = Flags::new(z, n, c, v);
    }
    fn bool_lit(&self, b: bool) -> bool {
        b
    }
    fn from_word(&self, val: u64) -> u64 {
        val
    }

    fn num_add(a: u64, b: u64) -> u64 {
        a.overflowing_add(b).0
    }
    fn num_sub(a: u64, b: u64) -> u64 {
        a.overflowing_sub(b).0
    }
    fn num_mul(a: u64, b: u64) -> u64 {
        (a as i64).overflowing_mul(b as i64).0 as u64
    }

    fn is_zero(a: u64) -> bool {
        a == 0
    }
    fn is_negative(a: u64) -> bool {
        (a as i64) < 0
    }
    fn add_carry(a: u64, b: u64) -> bool {
        a.overflowing_add(b).1
    }
    fn sub_carry(a: u64, b: u64) -> bool {
        !a.overflowing_sub(b).1
    }
    fn add_signed_overflow(a: u64, b: u64) -> bool {
        (a as i64).overflowing_add(b as i64).1
    }
    fn sub_signed_overflow(a: u64, b: u64) -> bool {
        (a as i64).overflowing_sub(b as i64).1
    }

    fn bool_eq(a: bool, b: bool) -> bool {
        a == b
    }
    fn select_num(cond: bool, t: u64, e: u64) -> u64 {
        if cond { t } else { e }
    }
    fn select_bool(cond: bool, t: bool, e: bool) -> bool {
        if cond { t } else { e }
    }
}

impl std::fmt::Display for S {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "registers: {:?} flags: {:?}",
            &self.registers[..5],
            self.flags
        )?;
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
