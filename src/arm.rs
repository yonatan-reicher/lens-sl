// Imports

use crate::all_permutations::Iter as PermutationIter;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::bool::prelude::*;
use crate::iter_slice_or_single::Iter as SliceOrSingle;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::word::prelude::*;

use std::fmt::{self, Debug, Display, Formatter};
use std::ops::ControlFlow;

// Derive macros
use derive_more::From;
use serde::{Deserialize, Serialize};

#[cfg(test)]
use proptest::prelude::*;

use smtlib::prelude::*;

// The actual code

#[derive(Copy, Clone, Debug, From, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ArgType {
    /// A register
    #[from]
    Reg(RegArgType),
    /// Immediate value (number)
    Imm,
    /// This argument is unused.
    Unused,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum RegArgType {
    // I like using 'Inp' instead of 'In' because it aligns with 'Out'
    Inp,
    Out,
}

impl ArgType {
    /// Shorthand for `matches!(x, ArgType::Reg(..))`
    pub fn is_reg(&self) -> bool {
        matches!(self, ArgType::Reg(..))
    }
}

/// In Arm, every instruction can be conditionally executed based on the state of the flags.
/// In the actual arm syntax, there are two more condition codes that we are missing, "HS" and
/// "LO", but these are just synonyms for "CS" and "CC".
#[derive(
    Copy, Clone, Debug, derive_more::Display, Default, PartialEq, Eq, Hash, Serialize, Deserialize,
)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[display("{}", self.to_string())]
/// TODO: Rename to cond
pub enum CondCode {
    /// Always (unconditional)
    /// In real Arm, this is actually the 15th condition code, but for we put it first because we
    /// want to enumerate it first.
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
    pub const COUNT: u8 = 15;

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

    pub fn check<B: Bool>(self, flags: Flags<B>) -> B {
        let Flags { z, n, c, v } = flags;
        match self {
            CondCode::Al => Bool::r#true(),
            CondCode::Eq => z,
            CondCode::Ne => !z,
            CondCode::Cs => c,
            CondCode::Cc => !c,
            CondCode::Mi => n,
            CondCode::Pl => !n,
            CondCode::Vs => v,
            CondCode::Vc => !v,
            CondCode::Hi => c & !z,
            CondCode::Ls => !c | z,
            CondCode::Ge => n.eq(&v),
            CondCode::Lt => n.neq(&v),
            CondCode::Gt => !z & n.eq(&v),
            CondCode::Le => z | n.neq(&v),
        }
    }
}

/// This macro will let us define our ISA as a table. Take a look at the use to below to see the
/// syntax. Explanations of the columns:
/// * OpCode - the name of the operation code, in Pascal Case.
/// * Arg 1/2/3 - the `ArgType` of the argument.
/// * String - the 3-letter name of the op-code in ARM syntax.
/// * Commutative - if true, the order of the second and third arguments do not matter, only makes
///   sense if the are of the same type.
/// * Affects Flags - this isn't how ARM actually works, but good enough for now
macro_rules! define_instructions {
    (
        | OpCode | Arg 1 | Arg 2 | Arg 3 | String | Commutative | Affects Flags |
        $(-)+
        $( | $op_code:ident | $arg1:ident $( ($subarg1:ident) )? | $arg2:ident $( ($subarg2:ident) )? | $arg3:ident $( ($subarg3:ident) )? | $str:literal | $com:literal | $affects_flags:literal | )+
    ) => {
        /// The operation codes supported by our ISA.
        #[derive(Copy, Clone, Debug, derive_more::Display, PartialEq, Eq, Hash, Serialize, Deserialize)]
        #[cfg_attr(test, derive(proptest_derive::Arbitrary))]
        #[display("{}", self.to_string())]
        pub enum OpCode {
            $( $op_code, )+
        }

        impl OpCode {
            /// Returns the argument types for this opcode.
            pub fn arg_types(&self) -> [ArgType; 3] {
                use ArgType::*;
                use RegArgType::*;
                match self {
                    $( OpCode::$op_code =>
                        [$arg1 $( ($subarg1) )?, $arg2 $( ($subarg2) )?, $arg3 $( ($subarg3) )?], )+
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

            pub fn commutative(&self) -> bool {
                match self {
                    $( OpCode::$op_code => $com, )+
                }
            }

            pub fn affects_flags(&self) -> bool {
                match self {
                    $( OpCode::$op_code => $affects_flags, )+
                }
            }
        }

        impl From<OpCode> for u8 {
            fn from(o: OpCode) -> u8 {
                o as u8
            }
        }
    };
}

define_instructions! {
    | OpCode  | Arg 1    | Arg 2    | Arg 3    | String | Commutative | Affects Flags |
    -----------------------------------------------------------------------------------
    | Nop     | Unused   | Unused   | Unused   | "nop"  |    true     |     false     |
    | Add     | Reg(Out) | Reg(Inp) | Reg(Inp) | "add"  |    true     |     false     |
    | AddI    | Reg(Out) | Reg(Inp) | Imm      | "add"  |    false    |     false     |
    | Sub     | Reg(Out) | Reg(Inp) | Reg(Inp) | "sub"  |    false    |     false     |
    | SubI    | Reg(Out) | Reg(Inp) | Imm      | "sub"  |    false    |     false     |
    | And     | Reg(Out) | Reg(Inp) | Reg(Inp) | "and"  |    true     |     false     |
    | Eor     | Reg(Out) | Reg(Inp) | Reg(Inp) | "eor"  |    true     |     false     |
    | Mov     | Reg(Out) | Reg(Inp) | Unused   | "mov"  |    false    |     false     |
    | MovI    | Reg(Inp) | Imm      | Unused   | "mov"  |    false    |     false     |
    | Mul     | Reg(Out) | Reg(Inp) | Reg(Inp) | "mul"  |    true     |     false     |
    | Orr     | Reg(Out) | Reg(Inp) | Reg(Inp) | "orr"  |    true     |     false     |
    | Cmp     | Reg(Inp) | Reg(Inp) | Unused   | "cmp"  |    false    |     true      |
    | CmpI    | Reg(Inp) | Imm      | Unused   | "cmp"  |    false    |     true      |
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
    Serialize,
    Deserialize,
)]
#[debug("r{_0}")]
#[display("r{_0}")]
pub struct Register(pub u8); // TODO: Change to Word4

#[cfg(test)]
impl Arbitrary for Register {
    type Parameters = ();
    type Strategy = prop::strategy::Map<std::ops::Range<u8>, fn(u8) -> Register>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (0..Register::COUNT).prop_map(Register)
    }
}

impl Register {
    /// TODO: Does Lens only use the regular 16 registers?
    pub const COUNT: u8 = 16;
    pub fn all() -> impl IntoIterator<Item = Register> + Iterator<Item = Register> {
        (0..Self::COUNT).map(Register)
    }
    pub const ALL: [Register; Self::COUNT as usize] = [
        Register(0),
        Register(1),
        Register(2),
        Register(3),
        Register(4),
        Register(5),
        Register(6),
        Register(7),
        Register(8),
        Register(9),
        Register(10),
        Register(11),
        Register(12),
        Register(13),
        Register(14),
        Register(15),
    ];
}

/// TODO: This (and a bunch of other impls in the crate) is actually kind of bad. The docs say From
/// conversions should be lossless. Oh well.
impl<W: Word> From<W> for Register {
    fn from(x: W) -> Self {
        Register(x.into_word::<Word8>().into())
    }
}

// impl<W: Word> From<Register> for W { // Some weird orphan rule conflict :/
impl From<Register> for Word8 {
    fn from(r: Register) -> Word8 {
        r.0.into()
    }
}

#[derive(Clone, Copy, Debug, derive_more::Display, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub enum ShiftCode<W> {
    None,
    /// Arithmetic shift right - shift right but keep MSB the same.
    /// Must have 1 <= n <= BITS.
    #[display("asr #{_0}")]
    Asr(W),
    /// Logical shift left.
    /// NOTE: There exists a synonym called `asl`, very confusing. lol.
    /// Must have 1 <= n <= BITS - 1.
    #[display("lsl #{_0}")]
    Lsl(W),
    /// Logical shift right.
    /// Must have 1 <= n <= BITS.
    #[display("lsr #{_0}")]
    Lsr(W),
    /// Rotate right.
    /// Must have 1 <= n <= BITS - 1.
    #[display("ror #{_0}")]
    Ror(W),
    /// Rotate right one bit, sign extended.
    #[display("rrx")]
    Rrx,
}

/// A single instruction.
/// `W` - number type for arguments.
/// `WShift` - number type for shift arguments.
/// NOTE: This is missing the 'S' bit - an optional bit toggling whether condition codes (flags)
/// should be updated. In Lens, they pretend it doesn't exist and that only `cmp` and `tst` update
/// the flags. When in Rome, act like a Roman.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Inst<W, WShift = BitWord<W>> {
    pub op_code: OpCode,
    pub cond_code: CondCode,
    pub shift: ShiftCode<WShift>,
    pub args: [W; 3],
}

// =========================================== State ==============================================

pub mod state;

use state::{BitMask, Mask};
pub use state::{Flags, FlagsBitField, State, StateVars, SymbolicState};

// ========================================= Semantics =============================================

mod semantics;

// ======================================== Backward Map ===========================================

mod backward_map;
pub use backward_map::BackwardMap;

impl<W: Copy + Into<Register>, WShift> Inst<W, WShift> {
    fn args_with_types(&self) -> impl Iterator<Item = (W, ArgType)> {
        self.args.into_iter().zip(self.op_code.arg_types())
    }

    pub fn regs(&self) -> impl Iterator<Item = Register> {
        let mut ret = vec![];
        for (a, t) in self.args_with_types() {
            if t.is_reg() {
                ret.push(a.into());
            }
        }
        ret.into_iter()
    }
}

impl<W: Word + HasBitWord> Inst<W> {
    pub fn run(&self, state: &mut State<W>) {
        let input = *state;
        let (mut read_mask, mut write_mask) = Default::default();
        *state = semantics::run(self, &input, &mut read_mask, &mut write_mask, &())
    }

    pub fn run_symbolic<'st>(&self, state: &mut SymbolicState<'st, W::SmtWord<'st>>) {
        let input = *state;
        let (mut read_mask, mut write_mask) = (Mask::empty(), Mask::empty());
        let st = state.registers[0].st();
        *state = semantics::run(self, &input, &mut read_mask, &mut write_mask, &st);
    }

    pub fn run_backward<'a>(
        &self,
        state: State<W>,
        bm: &'a BackwardMap<W>,
    ) -> impl IntoIterator<Item = &'a State<W>> + use<'a, W> {
        &bm[(*self, state)]
    }

    /// What the instruction reads from, given a state.
    pub fn read_mask(&self, state: &State<W>) -> Mask {
        let input = *state;
        let (mut read_mask, mut write_mask) = Default::default();
        semantics::run(self, &input, &mut read_mask, &mut write_mask, &());
        read_mask
    }

    pub fn run_masked(&self, masked: state::Masked<W>) -> Option<state::Masked<W>> {
        let (mut read_mask, mut write_mask) = Default::default();
        let out = semantics::run(self, masked.state(), &mut read_mask, &mut write_mask, &());
        // Check if we didn't read something that's not actually allowed
        let input_mask: BitMask = masked.mask();
        let missing_inputs_mask = read_mask.into_bit_mask() & !input_mask;
        if !missing_inputs_mask.is_empty() {
            return None;
        }
        // Nope, everything is fine and fun
        Some(out.masked(input_mask.into_mask() | write_mask))
    }

    pub fn run_backward_masked<'a>(
        &self,
        _output: state::Masked<W>,
        _bm: &'a BackwardMap<W>,
    ) -> impl IntoIterator<Item = &'a state::Masked<W>> + use<'a, W> {
        // let unmasked_outputs = output.mask().;
        // self.run_backward(*output.state(), bm)
        //     .into_iter()
        //     .map(|input| {
        //         todo!()
        //     })
        todo!();
        #[allow(unreachable_code)]
        []
    }

    pub fn reduce<WSmall: Word, WShiftSmall: Word>(
        &self,
        reducer: &mut Reducer<W, WSmall>,
    ) -> Inst<WSmall, WShiftSmall> {
        fn reduce_arg<W: Word, WSmall: Word>(
            reducer: &mut Reducer<W, WSmall>,
            arg: W,
            arg_type: ArgType,
            info: &ImmediateInfo,
        ) -> WSmall {
            match arg_type {
                ArgType::Imm => reducer.reduce(arg, info),
                ArgType::Reg(..) | ArgType::Unused => arg.into_word(),
            }
        }

        let arg_types = self.op_code.arg_types();
        let not_shift = ImmediateInfo {
            // TODO: is_shift: op_code.is_shift_instruction(),
            is_shift: false,
        };
        let shift = self.shift.reduce(reducer);
        Inst {
            op_code: self.op_code,
            cond_code: self.cond_code,
            shift,
            args: [
                reduce_arg(reducer, self.args[0], arg_types[0], &not_shift),
                reduce_arg(reducer, self.args[1], arg_types[1], &not_shift),
                reduce_arg(reducer, self.args[2], arg_types[2], &not_shift),
            ],
        }
    }

    pub fn extend<WBig: Word, WShiftBig: Word>(
        &self,
        reducer: &Reducer<WBig, W>,
    ) -> impl Iterator<Item = Inst<WBig, WShiftBig>> + Clone {
        fn extend_arg<WSmall: Word, WBig: Word>(
            reducer: &Reducer<WBig, WSmall>,
            arg: WSmall,
            arg_type: ArgType,
        ) -> SliceOrSingle<'_, WBig> {
            match arg_type {
                ArgType::Imm => reducer.extend(arg),
                ArgType::Reg(..) | ArgType::Unused => SliceOrSingle::Single(arg.into_word()),
            }
        }
        // If only we had do notation 🥹
        let args = self.args;
        let arg_types = self.op_code.arg_types();
        extend_arg(reducer, args[0], arg_types[0]).flat_map(move |arg0| {
            extend_arg(reducer, args[1], arg_types[1]).flat_map(move |arg1| {
                extend_arg(reducer, args[2], arg_types[2]).flat_map(move |arg2| {
                    self.shift.extend(reducer).map(move |shift| Inst {
                        op_code: self.op_code,
                        cond_code: self.cond_code,
                        shift,
                        args: [arg0, arg1, arg2],
                    })
                })
            })
        })
    }
}

impl<WShift: Word> ShiftCode<WShift> {
    fn reduce<W: Word, WSmall: Word, WShiftSmall: Word>(
        &self,
        reducer: &mut Reducer<W, WSmall>,
    ) -> ShiftCode<WShiftSmall> {
        use ShiftCode::*;
        let yes_shift = &ImmediateInfo { is_shift: true };
        match self {
            None => None,
            Asr(x) => Asr(reducer.reduce(x.into_word(), yes_shift).into_word()),
            Lsl(x) => Lsl(reducer.reduce(x.into_word(), yes_shift).into_word()),
            Lsr(x) => Lsr(reducer.reduce(x.into_word(), yes_shift).into_word()),
            Ror(x) => Ror(reducer.reduce(x.into_word(), yes_shift).into_word()),
            Rrx => Rrx,
        }
    }

    fn extend<W: Word, WBig: Word, WShiftBig: Word>(
        &self,
        reducer: &Reducer<WBig, W>,
    ) -> impl Iterator<Item = ShiftCode<WShiftBig>> + Clone {
        use ShiftCode::*;
        use itertools::Either;
        match self {
            None => Either::Left([None].into_iter()),
            // TODO: Filter to only results that make sense (fit)
            Asr(x) => Either::Right(
                reducer
                    .extend(x.into_word())
                    .map((|x: WBig| Asr(x.into_word())) as fn(_) -> _),
            ),
            Lsl(x) => Either::Right(
                reducer
                    .extend(x.into_word())
                    .map((|x: WBig| Lsl(x.into_word())) as fn(_) -> _),
            ),
            Lsr(x) => Either::Right(
                reducer
                    .extend(x.into_word())
                    .map((|x: WBig| Lsr(x.into_word())) as fn(_) -> _),
            ),
            Ror(x) => Either::Right(
                reducer
                    .extend(x.into_word())
                    .map((|x: WBig| Ror(x.into_word())) as fn(_) -> _),
            ),
            Rrx => Either::Left([Rrx].into_iter()),
        }
    }
}

pub mod enumerate;
impl<W: Word + HasBitWord> Inst<W> {
    pub fn enumerate<'a>(ei: EnumerationInfo<'a, W>) -> impl Iterator<Item = Self> + use<'a, W> {
        enumerate::Enumerator::new(ei)
    }
}

fn fmt_inst(
    Inst {
        op_code,
        cond_code,
        shift,
        args,
    }: &Inst<&str, &str>, // Look at this cute hack! Taking the arguments as strings.
    f: &mut Formatter,
) -> fmt::Result {
    let args = args
        .iter()
        .zip(op_code.arg_types())
        .map(|(arg, arg_type)| match arg_type {
            ArgType::Reg(..) => format!("r{arg}"),
            ArgType::Imm => format!("#{arg}"),
            ArgType::Unused => "-".to_string(),
        })
        .collect::<Vec<_>>();
    #[rustfmt::skip] {
                                        write!(f, "{op_code}")?;
        if *cond_code != CondCode::Al { write!(f, "{cond_code}")? };
                                        write!(f, " {}, {}, {}", args[0], args[1], args[2])?;
        if *shift != ShiftCode::None  { write!(f, ", {}", shift)? };
    };
    Ok(())
}

impl<W: Debug, WShift: Debug> Debug for Inst<W, WShift> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let arg_strings = self.args.each_ref().map(|a| format!("{a:?}"));
        let shift_string = self.shift.as_ref().map(|x| format!("{x:?}"));
        fmt_inst(
            &Inst {
                op_code: self.op_code,
                cond_code: self.cond_code,
                shift: shift_string.as_ref().map(|s| s.as_str()),
                args: arg_strings.each_ref().map(|s| s.as_str()),
            },
            f,
        )
    }
}

impl<W: Display, WShift: Display> Display for Inst<W, WShift> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let arg_strings = self.args.each_ref().map(|a| format!("{a}"));
        let shift_string = self.shift.as_ref().map(|x| format!("{x}"));
        fmt_inst(
            &Inst {
                op_code: self.op_code,
                cond_code: self.cond_code,
                shift: shift_string.as_ref().map(|s| s.as_str()),
                args: arg_strings.each_ref().map(|s| s.as_str()),
            },
            f,
        )
    }
}

// Shift code map
impl<T> ShiftCode<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ShiftCode<U> {
        match self {
            ShiftCode::None => ShiftCode::None,
            ShiftCode::Asr(x) => ShiftCode::Asr(f(x)),
            ShiftCode::Lsl(x) => ShiftCode::Lsl(f(x)),
            ShiftCode::Lsr(x) => ShiftCode::Lsr(f(x)),
            ShiftCode::Ror(x) => ShiftCode::Ror(f(x)),
            ShiftCode::Rrx => ShiftCode::Rrx,
        }
    }

    pub fn as_ref(&self) -> ShiftCode<&T> {
        match self {
            ShiftCode::None => ShiftCode::None,
            ShiftCode::Asr(x) => ShiftCode::Asr(x),
            ShiftCode::Lsl(x) => ShiftCode::Lsl(x),
            ShiftCode::Lsr(x) => ShiftCode::Lsr(x),
            ShiftCode::Ror(x) => ShiftCode::Ror(x),
            ShiftCode::Rrx => ShiftCode::Rrx,
        }
    }
}

#[cfg(test)]
impl<W: Word + Arbitrary, WShift: Word + Arbitrary> Arbitrary for Inst<W, WShift> {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): ()) -> Self::Strategy {
        any::<(OpCode, CondCode, ShiftCode<WShift>)>()
            .prop_flat_map(|(op_code, cond_code, shift)| {
                op_code
                    .arg_types()
                    .map(|arg_type| match arg_type {
                        ArgType::Reg(..) => any::<Register>()
                            .prop_map(|r| Word8::from(r).into_word())
                            .boxed(),
                        ArgType::Imm => any::<W>().boxed(),
                        ArgType::Unused => Just(0.into()).boxed(),
                    })
                    .prop_map(move |args| Inst {
                        op_code,
                        cond_code,
                        shift,
                        args,
                    })
            })
            .boxed()
    }
}

/// A macro to create an instruction more easily.
#[macro_export]
macro_rules! inst {
    ( $op_code:ident $cond_code:ident, $( $arg:expr ),* $(; shift $shift:ident $( ($shift_arg:expr) )? )? ) => {
        {
            let args_iter = [$( $arg ),*];
            #[allow(unused_assignments, unused_mut)]
            let mut shift = $crate::ShiftCode::None;
            $( shift = $crate::ShiftCode::$shift $( ($shift_arg) )?; )?
            $crate::Inst {
                op_code: $crate::OpCode::$op_code,
                cond_code: $crate::CondCode::$cond_code,
                shift,
                args: [
                    args_iter.get(0).cloned().unwrap_or(0usize).into(),
                    args_iter.get(1).cloned().unwrap_or(0usize).into(),
                    args_iter.get(2).cloned().unwrap_or(0usize).into(),
                ],
            }
        }
    };
    ($op_code:ident, $( $arg:expr ),* $(;  shift $shift:ident $( ($shift_arg:expr) )? )? ) => {
        inst!($op_code Al, $( $arg ),* $(;  shift $shift $( ($shift_arg) )? )? )
    };
}

pub fn extend_program_for_each<F, T, WBig, WSmall>(
    program: &[Inst<WSmall>],
    reducer: &Reducer<WBig, WSmall>,
    mut f: F,
) -> ControlFlow<T>
where
    F: FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
    WBig: Word + HasBitWord,
    WSmall: Word + HasBitWord,
{
    let mut ret = vec![];
    let iters: Vec<_> = program.iter().map(|inst| inst.extend(reducer)).collect();
    let mut iter = PermutationIter::new(iters.as_slice());
    while let Some(perm) = iter.next_slice() {
        ret.clear();
        ret.extend_from_slice(perm);
        f(&ret)?;
    }
    ControlFlow::Continue(())
}

/// Returns a mask having only those parts of the input state that affect the output of the
/// program.
pub fn what_program_reads<W: Word + HasBitWord>(
    prog: impl IntoIterator<Item = Inst<W>>,
    input: &State<W>,
) -> BitMask {
    // This will be the parts of the input that we have read.
    let mut input_mask = BitMask::EMPTY;
    // And this will be the parts of the output that we have written to.
    let mut output_mask = BitMask::EMPTY;
    let mut state = *input;
    for inst in prog {
        let (mut read_mask, mut write_mask) = Default::default();
        state = semantics::run(&inst, &state, &mut read_mask, &mut write_mask, &());
        input_mask = input_mask | (read_mask.into_bit_mask() & !output_mask);
        output_mask = output_mask | write_mask.into();
    }
    input_mask
}

pub fn run_program_masked<W: Word + HasBitWord>(
    prog: impl IntoIterator<Item = Inst<W>>,
    input: state::Masked<W>,
) -> Option<state::Masked<W>> {
    prog.into_iter()
        .try_fold(input, |current, inst| inst.run_masked(current))
}

pub mod parse;
pub use parse::parse;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arm::state::Mask;
    use functionality::prelude::*;
    use proptest::property_test;
    use std::collections::HashSet;

    #[test]
    fn basic_extend_test() {
        let program: [Inst<Word64>; 2] = [inst!(AddI, 0, 1, 1242), inst!(Sub, 2, 0, 1)];
        let mut reducer = Reducer::<Word64, Word4>::default();
        // Add another constant that clashes with 1242 when reduced.
        reducer.reduce((1242 + 16usize).into(), &ImmediateInfo { is_shift: false });
        let mut programs = HashSet::new();
        let program_reduced = program.map(|inst| inst.reduce(&mut reducer));
        let _ = extend_program_for_each(&program_reduced, &reducer, |extended_program| {
            for inst in extended_program {
                println!("{}", inst);
            }
            println!("---");
            programs.insert(extended_program.to_vec());
            ControlFlow::<(), ()>::Continue(())
        });
        assert_eq!(
            programs,
            [
                program.to_vec(),
                vec![inst!(AddI, 0, 1, 1242 + 16), inst!(Sub, 2, 0, 1),],
            ]
            .into()
        );
    }

    #[test]
    fn test_flags_from_sub_zero() {
        let flags = Flags::from_sub::<Word64>(5usize.into(), 5usize.into());
        assert!(flags.z);
        assert!(!flags.n);
        assert!(flags.c); // no borrow
        assert!(!flags.v);
    }

    #[test]
    fn test_flags_from_sub_positive() {
        let flags = Flags::from_sub::<Word64>(10usize.into(), 3usize.into());
        assert!(!flags.z);
        assert!(!flags.n); // 7 is positive
        assert!(flags.c); // no borrow (10 >= 3)
        assert!(!flags.v);
    }

    #[test]
    fn test_flags_from_sub_negative() {
        let flags = Flags::from_sub::<Word64>(3usize.into(), 10usize.into());
        assert!(!flags.z);
        assert!(flags.n); // result is negative (wraps)
        assert!(!flags.c); // borrow occurred (3 < 10)
        assert!(!flags.v);
    }

    #[test]
    fn test_flags_from_sub_overflow_positive() {
        // Positive - Negative = Negative (overflow)
        // For u8: 127 - (-128) = 127 - 128 (as unsigned) = 127 - 128 (wraps)
        let flags = Flags::from_sub::<Word8>(127usize.into(), 128usize.into()); // 127 - (-128 as u8)
        assert!(flags.n); // wrapped to negative
        assert!(flags.v); // overflow occurred
    }

    #[test]
    fn test_flags_from_sub_overflow_negative() {
        // Negative - Positive = Positive (overflow)
        // For u8: 128 (as -128 signed) - 1 = wraps to 127
        let flags = Flags::from_sub::<Word8>(128usize.into(), 1usize.into());
        assert!(!flags.n); // wrapped to positive
        assert!(flags.v); // overflow occurred
    }

    // TODO: Change when we add OpCode::Cmp
    #[test]
    fn what_program_reads_example_1() {
        type W = Word8;
        let p: Vec<Inst<W>> = vec![inst!(AddI, 0, 0, 5)];
        let input = State::default();
        let input_mask = what_program_reads(p.clone(), &input);
        let output = run_program_masked(p, input.masked(input_mask.into()));
        assert_eq!(input_mask, Mask::just_register(Register(0)).into());
        assert_eq!(
            output.unwrap(),
            input
                .mutate(|i| i[Register(0)] = 5.into())
                .masked(Mask::just_register(Register(0)))
        );
    }

    #[test]
    fn what_program_reads_example_2() {
        type W = Word8;
        let p: Vec<Inst<W>> = vec![
            inst!(AddI, 0, 0, 5),
            inst!(AddI Eq, 1, 0, 1),
            inst!(Mul Eq, 1, 0, 1),
        ];
        // The zero flag is false, so the two last instructions should not run.
        let input = State::default();
        let input_mask = what_program_reads(p.clone(), &input);
        let output = run_program_masked(p, input.masked(input_mask.into()));
        let m = Mask::just_register(Register(0)) | Mask::JUST_FLAGS;
        assert_eq!(input_mask, m.into());
        assert_eq!(
            output.unwrap(),
            input.mutate(|i| i[Register(0)] = 5.into()).masked(m),
        );
    }

    #[property_test]
    fn what_program_reads_is_enough_for_run_program_masked(
        prog: [Inst<Word64>; 1],
        state: State<Word64>,
    ) {
        println!("==== Starting! ====");
        dbg!(&prog, &state);
        let mask = what_program_reads(prog, &state);
        dbg!(&mask);
        let state = state.masked(mask.into());
        let out = dbg!(run_program_masked(prog, state));
        prop_assert!(out.is_some());
        println!("Success!");
    }
}
