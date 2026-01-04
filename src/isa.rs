use std::fmt::{Debug, Display};
use arbitrary_int::{i4, u4};
use num_traits::{PrimInt, Signed, Unsigned, AsPrimitive, Bounded};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArgType {
    /// Register
    Reg,
    /// Immediate value (number)
    Imm,
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
    | OpCode  | Arg 1 | Arg 2 | Arg 3 |
    -----------------------------------
    | Add     | Reg   | Reg   | Reg   |
    | AddI    | Reg   | Reg   | Imm   |
}

/// A number representing a register.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Register(pub u8);

impl Register {
    /// TODO: Does Lens only use the regular 16 registers?
    pub const COUNT: u8 = 16;
}

pub trait Word {
    type Signed:
        AsPrimitive<i8>
        + AsPrimitive<Self::Unsigned>
        + Bounded
        + Debug
        + Display
        + Signed
        + PrimInt;
    type Unsigned:
        AsPrimitive<u8>
        + AsPrimitive<Self::Signed>
        + Bounded
        + Debug
        + Display
        + Unsigned
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
#[debug("{op_code:?}{args:?}")]
pub struct Inst<W: Word> {
    pub op_code: OpCode,
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
        Add => set!(r![0 i] <- r![1 i] + r![2 i]),
        AddI => set!(r![0 i] <- r![1 i] + imm![2 i]),
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
    ( $op_code:ident, $( $arg:expr ),* $(,)? ) => {
        {
            let mut args_iter = [$( $arg as _ ),*].iter();
            $crate::Inst {
                op_code: $crate::OpCode::$op_code,
                args: [
                    *args_iter.next().unwrap_or(&0),
                    *args_iter.next().unwrap_or(&0),
                    *args_iter.next().unwrap_or(&0),
                ],
            }
        }
    };
}
