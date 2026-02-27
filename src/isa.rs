use crate::all_permutations::Iter as PermutationIter;
use crate::bool::prelude::*;
use crate::iter_slice_or_single::Iter as SliceOrSingle;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::word::prelude::*;

use std::fmt::Debug;
use std::ops::ControlFlow;

use arbitrary_int::traits::Integer;

use smtlib::Storage;
use smtlib::prelude::*;
use smtlib::terms::Const;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Flags<B = bool> {
    pub z: B,
    pub n: B,
    pub c: B,
    pub v: B,
}

impl<'st, B: Copy + Into<SmtBool<'st>>> BoolEq<SmtBool<'st>> for Flags<B> {
    fn eq(&self, other: &Self) -> SmtBool<'st> {
        self.z.into().eq(&other.z.into())
        & self.n.into().eq(&other.n.into())
        & self.c.into().eq(&other.c.into())
        & self.v.into().eq(&other.v.into())
    }
}

impl<'st> From<Flags<bool>> for Flags<SmtBool<'st>> {
    fn from(Flags { z, n, c, v }: Flags<bool>) -> Self {
        Self {
            z: Bool::from_bool(z),
            n: Bool::from_bool(n),
            c: Bool::from_bool(c),
            v: Bool::from_bool(v),
        }
    }
}

impl<'st> From<Flags<Const<'st, SmtBool<'st>>>> for Flags<SmtBool<'st>> {
    fn from(Flags { z, n, c, v }: Flags<Const<'st, SmtBool<'st>>>) -> Self {
        Self {
            z: z.into(),
            n: n.into(),
            c: c.into(),
            v: v.into(),
        }
    }
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

/// This macro will let us define our ISA as a table.
macro_rules! define_instructions {
    (
        | OpCode | Arg 1 | Arg 2 | Arg 3 | String |
        $(-)+
        $( | $op_code:ident | $arg1:ident | $arg2:ident | $arg3:ident | $str:literal |)+
    ) => {
        /// The operation codes supported by our ISA.
        #[derive(Copy, Clone, Debug, derive_more::Display, PartialEq, Eq, Hash)]
        #[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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
    | Sub     | Reg    | Reg    | Reg    | "sub"  |
    | SubI    | Reg    | Reg    | Imm    | "sub"  |
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
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[debug("r{_0}")]
#[display("r{_0}")]
pub struct Register(pub u8);

impl Register {
    /// TODO: Does Lens only use the regular 16 registers?
    pub const COUNT: u8 = 16;
}

/// A single instruction.
#[derive(derive_more::Debug, derive_more::Display, PartialEq, Eq, Hash)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
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
impl<W: Word> Copy for Inst<W> {}
impl<W: Word> Clone for Inst<W> {
    fn clone(&self) -> Self {
        *self
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
    pub struct FlagsBitField: u8 {
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

impl From<Flags> for FlagsBitField {
    #[rustfmt::skip]
    fn from(Flags { z, n, c, v } : Flags) -> Self {
        let mut flags = FlagsBitField::empty();
        if z { flags |= FlagsBitField::Z; }
        if n { flags |= FlagsBitField::N; }
        if c { flags |= FlagsBitField::C; }
        if v { flags |= FlagsBitField::V; }
        flags
    }
}

impl<B: Bool> From<FlagsBitField> for Flags<B> {
    fn from(value: FlagsBitField) -> Self {
        Self {
            z: B::from_bool(value.contains(FlagsBitField::Z)),
            n: B::from_bool(value.contains(FlagsBitField::N)),
            c: B::from_bool(value.contains(FlagsBitField::C)),
            v: B::from_bool(value.contains(FlagsBitField::V)),
        }
    }
}

#[cfg(test)]
impl proptest::arbitrary::Arbitrary for FlagsBitField {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    #[rustfmt::skip]
    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use proptest::prelude::*;
        (any::<bool>(), any::<bool>(), any::<bool>(), any::<bool>())
            .prop_map(|(z, n, c, v)| {
                let mut flags = FlagsBitField::empty();
                if z { flags |= FlagsBitField::Z; }
                if n { flags |= FlagsBitField::N; }
                if c { flags |= FlagsBitField::C; }
                if v { flags |= FlagsBitField::V; }
                flags
            })
            .boxed()
    }
}

pub trait State<W: Word> {
    fn get_register(&self, reg: Register) -> W::Unsigned;
    fn set_register(&mut self, reg: Register, value: W::Unsigned);
    fn get_flags(&self) -> FlagsBitField;
    fn set_flags(&mut self, flags: FlagsBitField);
}

#[derive(Clone, Copy, Debug)]
pub struct StateVars<'st, W: Word> {
    pub registers: [Const<'st, W::SymbolicBitVec<'st>>; Register::COUNT as usize],
    pub flags: Flags<Const<'st, SmtBool<'st>>>,
}

impl<'st, W: Word> StateVars<'st, W> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            registers: std::array::from_fn(|i| {
                W::SymbolicBitVec::new_const(st, &format!("{name}_r{i}"))
            }),
            flags: Flags::new(st, name),
        }
    }
}

impl<'st> Flags<Const<'st, SmtBool<'st>>> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            z: SmtBool::new_const(st, &format!("{name}_z")),
            n: SmtBool::new_const(st, &format!("{name}_n")),
            c: SmtBool::new_const(st, &format!("{name}_c")),
            v: SmtBool::new_const(st, &format!("{name}_v")),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SymbolicState<'st, W: Word> {
    pub registers: [W::SymbolicBitVec<'st>; Register::COUNT as usize],
    pub flags: Flags<SmtBool<'st>>,
}

impl<'st, W: Word> From<StateVars<'st, W>> for SymbolicState<'st, W> {
    fn from(value: StateVars<'st, W>) -> Self {
        Self {
            registers: value.registers.map(Into::into),
            flags: value.flags.into(),
        }
    }
}

impl<'st, W: Word> SymbolicState<'st, W> {
    pub fn eq(&self, other: Self) -> SmtBool<'st> {
        let regs = self.registers.iter().zip(other.registers);
        let regs_eq = regs
            .map(|(ra, rb)| ra._eq(rb))
            .reduce(|b1, b2| b1 & b2)
            .unwrap();
        regs_eq & self.flags.eq(&other.flags)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum AddOrSub {
    Add,
    Sub,
}

fn run_addition_or_subtraction<W: Word, S: State<W>>(
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
            let (res, unsigend_overflow) = left.overflowing_add(right);
            let (res2, signed_overflow) = signed_left.overflowing_add(signed_right);
            debug_assert_eq!(Integer::as_::<W::Signed>(res), res2);
            (res, signed_overflow, unsigend_overflow)
        }
        AddOrSub::Sub => {
            let signed_left: W::Signed = left.as_();
            let signed_right: W::Signed = right.as_();
            let (res, unsigend_overflow) = left.overflowing_sub(right);
            let (res2, signed_overflow) = signed_left.overflowing_sub(signed_right);
            debug_assert_eq!(Integer::as_::<W::Signed>(res), res2);
            (res, signed_overflow, unsigend_overflow)
        }
    };
    let res_signed: W::Signed = res.as_();
    state.set_register(result_register, res);
    // Set flags.
    let mut flags = FlagsBitField::empty();
    if res.is_zero() {
        flags |= FlagsBitField::Z;
    }
    if res_signed > 0.as_() {
        flags |= FlagsBitField::N;
    }
    if unsigned_overflow && kind == AddOrSub::Add {
        flags |= FlagsBitField::C;
    }
    if !unsigned_overflow && kind == AddOrSub::Sub {
        flags |= FlagsBitField::C;
    }
    if signed_overflow {
        flags |= FlagsBitField::V;
    }
    state.set_flags(flags);
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
        Sub => run_addition_or_subtraction(
            state,
            r![1 u],
            r![2 u],
            Register(inst.args[0].as_()),
            AddOrSub::Sub,
        ),
        SubI => run_addition_or_subtraction(
            state,
            r![1 u],
            imm![2 u],
            Register(inst.args[0].as_()),
            AddOrSub::Sub,
        ),
        And => set!(r![0 u] <- r![1 u] & r![2 u]),
        Eor => set!(r![0 u] <- r![1 u] ^ r![2 u]),
        Mov => set!(r![0 u] <- r![1 u]),
        MovI => set!(r![0 u] <- imm![1 u]),
        Mul => set!(r![0 i] <- r![1 i].overflowing_mul(r![2 i]).0),
        Orr => set!(r![0 u] <- r![1 u] | r![2 u]),
    }
}

fn run_instruction_symbolic<W: Word>(inst: &Inst<W>, state: &mut SymbolicState<'_, W>) {
    /// Get a register value.
    macro_rules! r {
        ($i:literal) => {
            state.registers[{
                debug_assert!($i < 3);
                debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
                let r = Register(inst.args[$i].as_());
                r.0 as usize
            }]
        };
    }
    /// Get an immediate value.
    macro_rules! imm {
        ($i:literal) => {
            W::new_bit_vec(state.registers[0].st(), inst.args[$i])
        };
    }

    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => r![0] = r![1] + r![2],
        AddI => r![0] = r![1] + imm![2],
        Sub => r![0] = r![1] + -r![2],
        SubI => r![0] = r![1] + -imm![2],
        And => r![0] = r![1] & r![2],
        Eor => r![0] = r![1] ^ r![2],
        Mov => r![0] = r![1],
        MovI => r![0] = imm![1],
        Mul => r![0] = r![1] * r![2],
        Orr => r![0] = r![1] | r![2],
    }
}

impl<W: Word> Inst<W> {
    pub fn run<S: State<W>>(&self, state: &mut S) {
        run_instruction(self, state)
    }

    pub fn run_symbolic(&self, state: &mut SymbolicState<'_, W>) {
        run_instruction_symbolic(self, state)
    }

    fn to_string_impl(self) -> String {
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
        if cond_code == CondCode::Al {
            format!("{op_code} {}, {}, {}", args[0], args[1], args[2])
        } else {
            format!("{op_code}{cond_code} {}, {}, {}", args[0], args[1], args[2])
        }
    }

    pub fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> Inst<WSmall> {
        fn reduce_arg<W: Word, WSmall: Word>(
            reducer: &mut Reducer<W, WSmall>,
            arg: W::Unsigned,
            arg_type: ArgType,
            info: &ImmediateInfo,
        ) -> WSmall::Unsigned {
            match arg_type {
                ArgType::Imm => reducer.reduce(arg, info),
                ArgType::Reg | ArgType::Unused => arg.as_(),
            }
        }

        let arg_types = self.op_code.arg_types();
        let info = ImmediateInfo {
            // TODO: is_shift: op_code.is_shift_instruction(),
            is_shift: false,
        };
        Inst {
            op_code: self.op_code,
            cond_code: self.cond_code,
            args: [
                reduce_arg(reducer, self.args[0], arg_types[0], &info),
                reduce_arg(reducer, self.args[1], arg_types[1], &info),
                reduce_arg(reducer, self.args[2], arg_types[2], &info),
            ],
        }
    }

    pub fn extend<WBig: Word>(
        &self,
        reducer: &Reducer<WBig, W>,
    ) -> impl Iterator<Item = Inst<WBig>> + Clone {
        fn extend_arg<WSmall: Word, WBig: Word>(
            reducer: &Reducer<WBig, WSmall>,
            arg: WSmall::Unsigned,
            arg_type: ArgType,
        ) -> SliceOrSingle<'_, WBig::Unsigned> {
            match arg_type {
                ArgType::Imm => reducer.extend(arg),
                ArgType::Reg | ArgType::Unused => SliceOrSingle::Single(arg.as_()),
            }
        }
        // If only we had do notation 🥹
        let args = self.args;
        let arg_types = self.op_code.arg_types();
        extend_arg(reducer, args[0], arg_types[0]).flat_map(move |arg0| {
            extend_arg(reducer, args[1], arg_types[1]).flat_map(move |arg1| {
                extend_arg(reducer, args[2], arg_types[2]).map(move |arg2| Inst {
                    op_code: self.op_code,
                    cond_code: self.cond_code,
                    args: [arg0, arg1, arg2],
                })
            })
        })
    }
}

/// A macro to create an instruction more easily.
#[macro_export]
macro_rules! inst {
    ( $op_code:ident $cond_code:ident, $( $arg:expr ),* $(,)? ) => {
        {
            let args_iter = [$( $arg ),*];
            $crate::Inst {
                op_code: $crate::OpCode::$op_code,
                cond_code: $crate::CondCode::$cond_code,
                args: [
                    *args_iter.get(0).unwrap_or(&Default::default()),
                    *args_iter.get(1).unwrap_or(&Default::default()),
                    *args_iter.get(2).unwrap_or(&Default::default()),
                ],
            }
        }
    };
    ( $op_code:ident, $( $arg:expr ),* $(,)? ) => {
        inst!($op_code Al, $( $arg ),* )
    };
}

pub fn extend_program_for_each<F, T, WBig, WSmall>(
    program: &[Inst<WSmall>],
    reducer: &Reducer<WBig, WSmall>,
    mut f: F,
) -> ControlFlow<T>
where
    F: FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
    WBig: Word,
    WSmall: Word,
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn basic_extend_test() {
        let program: [Inst<Word64>; 2] = [inst!(AddI, 0, 1, 1242), inst!(Sub, 2, 0, 1)];
        let mut reducer = Reducer::<Word64, Word4>::default();
        // Add another constant that clashes with 1242 when reduced.
        reducer.reduce(1242 + 16, &ImmediateInfo { is_shift: false });
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
}
