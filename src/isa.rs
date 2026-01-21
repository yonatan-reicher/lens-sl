use std::fmt::{Debug, Display};
// use arbitrary_int::{i4, u4};
use num_traits::{
    AsPrimitive, PrimInt, Signed, Unsigned, Zero,
    ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArgType {
    /// Register
    Reg,
    /// Immediate value (number)
    Imm,
    /// This argument is unused.
    Unused,
}

/// In Arm, every instruction can be conditionally executed based on the state
/// of the flags.
#[derive(Copy, Clone, Debug, derive_more::Display, Default, PartialEq, Eq, Hash)]
#[display("{}", self.to_string())]
pub enum CondCode {
    /// Always (unconditional)
    /// In real Arm, this is actually the 15th condition code, but for we put it
    /// first because we want to enumerate it first.
    #[default]
    Al,
    /// Equal - Z set
    Eq,
    /// Not equal - Z clear
    Ne,
    /// Carry set
    Cs,
    /// Carry clear
    Cc,
    /// Negative - N set
    Mi,
    /// Positive or zero - N clear
    Pl,
    /// V set (overflow)
    Vs,
    /// V clear (no overflow)
    Vc,
    /// Unsigned higher - C set and Z clear
    Hi,
    /// Unsigned lower or same - C clear or Z set
    Ls,
    /// Signed greater than or equal - N equals V
    Ge,
    /// Signed less than - N not equal to V
    Lt,
    /// Signed greater than - Z clear AND N equals V
    Gt,
    /// Signed less than or equal - Z set OR N not equal to V
    Le,
}

impl CondCode {
    pub const COUNT: u8 = 6;

    pub const fn to_string(&self) -> &'static str {
        match self {
            CondCode::Al => "al",
            CondCode::Eq => "eq",
            CondCode::Ne => "ne",
            CondCode::Cs => "cs",
            CondCode::Cc => "cc",
            CondCode::Mi => "mi",
            CondCode::Pl => "pl",
            CondCode::Vs => "vs",
            CondCode::Vc => "vc",
            CondCode::Hi => "hi",
            CondCode::Ls => "ls",
            CondCode::Ge => "ge",
            CondCode::Lt => "lt",
            CondCode::Gt => "gt",
            CondCode::Le => "le",
        }
    }
}

/// This macro will let us define our ISA as a table.
macro_rules! define_instructions {
    (
        | OpCode | Arg 1 | Arg 2 | Arg 3 | String |
        $(-)+
        $( | $op_code:ident | $arg1:ident | $arg2:ident | $arg3:ident | $str:literal |)+
    ) => {
        /// The operation codes supported by our ISA.
        #[derive(Copy, Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
        #[display("{}", self.to_string())]
        pub enum OpCode {
            $( $op_code, )+
        }

        impl OpCode {
            /// Returns the argument types for this opcode.
            pub fn arg_types(&self) -> [ArgType; 3] {
                match self {
                    $( OpCode::$op_code =>
                        [ArgType::$arg1, ArgType::$arg2, ArgType::$arg3], )+
                }
            }

            pub fn to_string(&self) -> String {
                match self {
                    $( OpCode::$op_code => $str.to_string(), )+
                }
            }

            /// An array of all op-codes.
            pub const ALL: &'static [OpCode] = &[
                $( OpCode::$op_code, )+
            ];

            /// The number of op-codes.
            pub const COUNT: u8 = Self::ALL.len() as u8;
        }
    };
}

define_instructions! {
    | OpCode  | Arg 1  | Arg 2  | Arg 3  | String |
    -----------------------------------------------
    | Nop     | Unused | Unused | Unused | "nop"  |
    | Add     | Reg    | Reg    | Reg    | "add"  |
    | AddI    | Reg    | Reg    | Imm    | "add"  |
    | And     | Reg    | Reg    | Reg    | "and"  |
    | Eor     | Reg    | Reg    | Reg    | "eor"  |
    | Mov     | Reg    | Reg    | Unused | "mov"  |
    | MovI    | Reg    | Imm    | Unused | "mov"  |
    | Mul     | Reg    | Reg    | Reg    | "mul"  |
    | Orr     | Reg    | Reg    | Reg    | "orr"  |
}

/// A number representing a register.
#[derive(
    Clone,
    Copy,
    derive_more::Debug,
    derive_more::Display,
    Default,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
)]
#[debug("r{_0}")]
#[display("r{_0}")]
pub struct Register(pub u8);

impl Register {
    /// TODO: Does Lens only use the regular 16 registers?
    pub const COUNT: u8 = 16;
}

pub trait Word {
    type Signed: AsPrimitive<i8>
        + AsPrimitive<Self::Unsigned>
        + Debug
        + Display
        + Signed
        + OverflowingAdd
        + OverflowingSub
        + OverflowingMul
        + PrimInt;
    type Unsigned: AsPrimitive<u8>
        + AsPrimitive<Self::Signed>
        + Debug
        + Display
        + Unsigned
        + OverflowingAdd
        + OverflowingSub
        + OverflowingMul
        + PrimInt;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Word64;
impl Word for Word64 {
    type Unsigned = u64;
    type Signed = i64;
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Word8;
impl Word for Word8 {
    type Unsigned = u8;
    type Signed = i8;
}
/*
pub struct Word4;
impl Word for Word4 {
    type Unsigned = u4;
    type Signed = i4;
}
*/

/// A single instruction.
#[derive(derive_more::Debug, derive_more::Display, PartialEq, Eq, Hash)]
#[debug("{op_code:?}{}{args:?}",
    match cond_code {
        CondCode::Al => "".to_string(),
        _ => format!("{cond_code:?}"),
    }
)]
#[display("{}", self.to_string_impl())]
pub struct Inst<W: Word> {
    pub op_code: OpCode,
    pub cond_code: CondCode,
    pub args: [W::Unsigned; 3],
}

// Implementing `Clone` and `Copy` manually instead of by `derive` because `derive` adds
// unnecessary trait bounds on the generic parameter.
impl<W: Word> Clone for Inst<W> {
    fn clone(&self) -> Self {
        Self { op_code: self.op_code.clone(), cond_code: self.cond_code.clone(), args: self.args.clone() }
    }
}

impl<W: Word> Copy for Inst<W> {}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct Flags: u8 {
        /// Zero - is the result zero? Disregard overflow and carry.
        const Z = 0b0001;
        /// Negative - is the Msb set? Disregard overflow and carry.
        const N = 0b0100;
        /// Carry - on when unsigned addition overflows or unsigned subtraction doesn't underflow.
        const C = 0b0010;
        /// Overflow - on signed addition/subtraction, on when result is out of signed range.
        const V = 0b1000;
    }
}

pub trait State {
    type W: Word;

    fn get_register(&self, reg: Register) -> <Self::W as Word>::Unsigned;
    fn set_register(&mut self, reg: Register, value: <Self::W as Word>::Unsigned);
    fn get_flags(&self) -> Flags;
    fn set_flags(&mut self, flags: Flags);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum AddOrSub {
    Add,
    Sub,
}

fn run_addition_or_subtraction<W: Word, S: State<W = W>>(
    state: &mut S,
    left: W::Unsigned,
    right: W::Unsigned,
    result_register: Register,
    kind: AddOrSub,
) {
    let (res, signed_overflow, unsigned_overflow) = match kind {
        AddOrSub::Add => {
            let signed_left: W::Signed = left.as_();
            let signed_right: W::Signed = right.as_();
            let (res, unsigend_overflow) = left.overflowing_add(&right);
            let (res2, signed_overflow) = signed_left.overflowing_add(&signed_right);
            debug_assert_eq!(AsPrimitive::<W::Signed>::as_(res), res2);
            (res, signed_overflow, unsigend_overflow)
        }
        AddOrSub::Sub => {
            let signed_left: W::Signed = left.as_();
            let signed_right: W::Signed = right.as_();
            let (res, unsigend_overflow) = left.overflowing_sub(&right);
            let (res2, signed_overflow) = signed_left.overflowing_sub(&signed_right);
            debug_assert_eq!(AsPrimitive::<W::Signed>::as_(res), res2);
            (res, signed_overflow, unsigend_overflow)
        }
    };
    let res_signed: W::Signed = res.as_();
    state.set_register(result_register, res);
    // Set flags.
    let mut flags = Flags::empty();
    if res.is_zero() {
        flags |= Flags::Z;
    }
    if res_signed.is_negative() {
        flags |= Flags::N;
    }
    if unsigned_overflow && kind == AddOrSub::Add {
        flags |= Flags::C;
    }
    if !unsigned_overflow && kind == AddOrSub::Sub {
        flags |= Flags::C;
    }
    if signed_overflow {
        flags |= Flags::V;
    }
    state.set_flags(flags);
}

fn run_instruction<W: Word, S: State<W = W>>(inst: &Inst<S::W>, state: &mut S) {
    /// Get a register value.
    macro_rules! r {
        ($i:literal u) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
            let r = Register(inst.args[$i].as_());
            state.get_register(r)
        }};
        ($i:literal i) => {{
            let r: W::Signed = r![$i u].as_();
            r
        }};
    }
    /// Set a register value.
    macro_rules! set {
        (r![$i:literal u] <- $value:expr) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
            let r = Register(inst.args[$i].as_());
            state.set_register(r, $value)
        }};
        (r![$i:literal i] <- $value:expr) => {{
            set!(r![$i u] <- $value.as_())
        }};
    }
    /// Get an immediate value.
    macro_rules! imm {
        ($i:literal u) => { inst.args[$i] };
        ($i:literal i) => {{
            let i: W::Signed = imm![$i u].as_();
            i
        }};
    }

    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => run_addition_or_subtraction(
            state,
            r![1 u],
            r![2 u],
            Register(inst.args[0].as_()),
            AddOrSub::Add,
        ),
        AddI => run_addition_or_subtraction(
            state,
            r![1 u],
            imm![2 u],
            Register(inst.args[0].as_()),
            AddOrSub::Add,
        ),
        And => set!(r![0 u] <- r![1 u] & r![2 u]),
        Eor => set!(r![0 u] <- r![1 u] ^ r![2 u]),
        Mov => set!(r![0 u] <- r![1 u]),
        MovI => set!(r![0 u] <- imm![1 u]),
        Mul => set!(r![0 i] <- r![1 i].overflowing_mul(&r![2 i]).0),
        Orr => set!(r![0 u] <- r![1 u] | r![2 u]),
    }
}

impl<W: Word> Inst<W> {
    pub fn run<S: State<W = W>>(&self, state: &mut S) {
        run_instruction(self, state)
    }

    fn to_string_impl(&self) -> String {
        let Inst {
            op_code,
            cond_code,
            args,
        } = self;
        let args = args
            .iter()
            .zip(op_code.arg_types())
            .map(|(arg, arg_type)| match arg_type {
                ArgType::Reg => format!("r{arg}"),
                ArgType::Imm => format!("#{arg}"),
                ArgType::Unused => "-".to_string(),
            })
            .collect::<Vec<_>>();
        if cond_code == &CondCode::Al {
            format!("{op_code} {}, {}, {}", args[0], args[1], args[2])
        } else {
            format!("{op_code}{cond_code} {}, {}, {}", args[0], args[1], args[2])
        }
    }
}

/// A macro to create an instruction more easily.
#[macro_export]
macro_rules! inst {
    ( $op_code:ident $cond_code:ident, $( $arg:expr ),* $(,)? ) => {
        {
            let mut args_iter = [$( $arg as _ ),*].iter();
            $crate::Inst {
                op_code: $crate::OpCode::$op_code,
                cond_code: $crate::CondCode::$cond_code,
                args: [
                    *args_iter.next().unwrap_or(&0),
                    *args_iter.next().unwrap_or(&0),
                    *args_iter.next().unwrap_or(&0),
                ],
            }
        }
    };
    ( $op_code:ident, $( $arg:expr ),* $(,)? ) => {
        inst!($op_code Al, $( $arg ),* )
    };
}
