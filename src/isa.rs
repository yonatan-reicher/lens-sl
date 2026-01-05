use std::fmt::{Debug, Display};
// use arbitrary_int::{i4, u4};
use num_traits::{AsPrimitive, PrimInt, Signed, Unsigned, WrappingAdd, WrappingMul};

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
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
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
}

/// This macro will let us define our ISA as a table.
macro_rules! define_instructions {
    (
        | OpCode | Arg 1 | Arg 2 | Arg 3 |
        $(-)+
        $( | $op_code:ident | $arg1:ident | $arg2:ident | $arg3:ident | )+
    ) => {
        /// The operation codes supported by our ISA.
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
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
    | OpCode  | Arg 1  | Arg 2  | Arg 3  |
    --------------------------------------
    | Nop     | Unused | Unused | Unused |
    | Add     | Reg    | Reg    | Reg    |
    | AddI    | Reg    | Reg    | Imm    |
    | And     | Reg    | Reg    | Reg    |
    | Eor     | Reg    | Reg    | Reg    |
    | Mov     | Reg    | Reg    | Unused |
    | MovI    | Reg    | Imm    | Unused |
    | Mul     | Reg    | Reg    | Reg    |
    | Orr     | Reg    | Reg    | Reg    |
}

/// A number representing a register.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
        + WrappingAdd
        + WrappingMul
        + PrimInt;
    type Unsigned: AsPrimitive<u8>
        + AsPrimitive<Self::Signed>
        + Debug
        + Display
        + Unsigned
        + WrappingAdd
        + WrappingMul
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
#[derive(Copy, Clone, derive_more::Debug, PartialEq, Eq, Hash)]
#[debug("{op_code:?}{}{args:?}",
    match cond_code {
        CondCode::Al => "".to_string(),
        _ => format!("{cond_code:?}"),
    }
)]
pub struct Inst<W: Word> {
    pub op_code: OpCode,
    pub cond_code: CondCode,
    pub args: [W::Unsigned; 3],
}

pub trait State<W: Word> {
    fn get_register(&self, reg: Register) -> W::Unsigned;
    fn set_register(&mut self, reg: Register, value: W::Unsigned);
}

fn run_instruction<W: Word, S: State<W>>(inst: &Inst<W>, state: &mut S) {
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
        Add => set!(r![0 i] <- r![1 i].wrapping_add(&r![2 i])),
        AddI => set!(r![0 i] <- r![1 i].wrapping_add(&imm![2 i])),
        And => set!(r![0 u] <- r![1 u] & r![2 u]),
        Eor => set!(r![0 u] <- r![1 u] ^ r![2 u]),
        Mov => set!(r![0 u] <- r![1 u]),
        MovI => set!(r![0 u] <- imm![1 u]),
        Mul => set!(r![0 i] <- r![1 i].wrapping_mul(&r![2 i])),
        Orr => set!(r![0 u] <- r![1 u] | r![2 u]),
    }
}

impl<W: Word> Inst<W> {
    pub fn run<S: State<W>>(&self, state: &mut S) {
        run_instruction(self, state)
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
