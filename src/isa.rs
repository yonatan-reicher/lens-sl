// Imports

use crate::all::All;
use crate::all_permutations::Iter as PermutationIter;
use crate::bool::prelude::*;
use crate::collect_registers;
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::iter_slice_or_single::Iter as SliceOrSingle;
use crate::oracle;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::word::prelude::*;

use std::fmt::Debug;
use std::ops::ControlFlow;

// Derive macros
use derive_more::Display;
use serde::{Deserialize, Serialize};

#[cfg(test)]
use proptest::prelude::*;

use rustc_hash::FxHashMap;

use smtlib::Storage;
use smtlib::prelude::*;
use smtlib::terms::Const;

// The actual code

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

/// In Arm, every instruction can be conditionally executed based on the state of the flags.
/// In the actual arm syntax, there are two more condition codes that we are missing, "HS" and
/// "LO", but these are just synonyms for "CS" and "CC".
#[derive(
    Copy, Clone, Debug, derive_more::Display, Default, PartialEq, Eq, Hash, Serialize, Deserialize,
)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[display("{}", self.to_string())]
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

impl<'st> Flags<SmtBool<'st>> {
    fn update_from_add<W: SmtWord<'st>>(&mut self, op1: W, op2: W, enabled: SmtBool<'st>) {
        let sum = op1 + op2;
        self.z = enabled.if_then_else(sum.is_zero(), self.z);
        self.n = enabled.if_then_else(sum.is_negative(), self.n);
        self.c = enabled.if_then_else(sum.unsigned_lt(op1), self.c);
        let both_positive = op1.is_positive() & op2.is_positive();
        let both_negative = op1.is_negative() & op2.is_negative();
        self.v = enabled.if_then_else(
            (both_positive & sum.signed_lt(op1)) | (both_negative & sum.is_positive()),
            self.v,
        );
    }

    fn update_from_sub<W: SmtWord<'st>>(&mut self, op1: W, op2: W, enabled: SmtBool<'st>) {
        let diff = op1.sub(op2);
        self.z = enabled.if_then_else(diff.is_zero(), self.z);
        self.n = enabled.if_then_else(diff.is_negative(), self.n);
        self.c = enabled.if_then_else(op2.unsigned_le(op1), self.c);
        let op1_positive = op1.is_positive();
        let op2_negative = op2.is_negative();
        let op1_negative = op1.is_negative();
        let op2_positive = op2.is_positive();
        self.v = enabled.if_then_else(
            (op1_positive & op2_negative & diff.is_negative())
                | (op1_negative & op2_positive & diff.is_positive()),
            self.v,
        );
    }
}

impl Flags<bool> {
    fn update_from_add<W: Word>(&mut self, op1: W, op2: W, enabled: bool) {
        if !enabled {
            return;
        }
        let sum = op1 + op2;
        self.z = sum.is_zero();
        self.n = sum.signed_negative();
        self.c = sum < op1;
        let both_positive = op1.signed_positive() && op2.signed_positive();
        let both_negative = op1.signed_negative() && op2.signed_negative();
        self.v = (both_positive && sum.signed_lt(op1)) || (both_negative && sum.signed_positive());
    }

    fn update_from_sub<W: Word>(&mut self, op1: W, op2: W, enabled: bool) {
        if !enabled {
            return;
        }
        let diff = op1 - op2;
        self.z = diff.is_zero();
        self.n = diff.signed_negative();
        self.c = op1 >= op2;
        let op1_positive = op1.signed_positive();
        let op2_negative = op2.signed_negative();
        let op1_negative = op1.signed_negative();
        let op2_positive = op2.signed_positive();
        self.v = (op1_positive && op2_negative && diff.signed_negative())
            || (op1_negative && op2_positive && diff.signed_positive());
    }

    /// Contains all possible combinations of flags.
    #[rustfmt::skip]
    pub const ALL: [Flags; 16 /* 2^4 */] = [
        Flags { z: false, n: false, c: false, v: false },
        Flags { z: true,  n: false, c: false, v: false },
        Flags { z: false, n: true,  c: false, v: false },
        Flags { z: true,  n: true,  c: false, v: false },
        Flags { z: false, n: false, c: true,  v: false },
        Flags { z: true,  n: false, c: true,  v: false },
        Flags { z: false, n: true,  c: true,  v: false },
        Flags { z: true,  n: true,  c: true,  v: false },
        Flags { z: false, n: false, c: false, v: true  },
        Flags { z: true,  n: false, c: false, v: true  },
        Flags { z: false, n: true,  c: false, v: true  },
        Flags { z: true,  n: true,  c: false, v: true  },
        Flags { z: false, n: false, c: true,  v: true  },
        Flags { z: true,  n: false, c: true,  v: true  },
        Flags { z: false, n: true,  c: true,  v: true  },
        Flags { z: true,  n: true,  c: true,  v: true  },
    ];
}

impl Display for Flags<bool> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let &Self { z, n, c, v } = self;
        let s = |b, c: char| if b { c } else { '─' };
        let w = |f: &mut Formatter, b, c| write!(f, "{}", s(b, c));
        w(f, z, 'Z')?;
        w(f, n, 'N')?;
        w(f, c, 'C')?;
        w(f, v, 'V')?;
        Ok(())
    }
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

/// This macro will let us define our ISA as a table.
macro_rules! define_instructions {
    (
        | OpCode | Arg 1 | Arg 2 | Arg 3 | String | Commutative |
        $(-)+
        $( | $op_code:ident | $arg1:ident | $arg2:ident | $arg3:ident | $str:literal | $com:literal | )+
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

            pub fn commutative(&self) -> bool {
                match self {
                    $( OpCode::$op_code => $com, )+
                }
            }
        }
    };
}

define_instructions! {
    | OpCode  | Arg 1  | Arg 2  | Arg 3  | String | Commutative |
    -------------------------------------------------------------
    | Nop     | Unused | Unused | Unused | "nop"  |    true     |
    | Add     | Reg    | Reg    | Reg    | "add"  |    true     |
    | AddI    | Reg    | Reg    | Imm    | "add"  |    false    |
    | Sub     | Reg    | Reg    | Reg    | "sub"  |    false    |
    | SubI    | Reg    | Reg    | Imm    | "sub"  |    false    |
    | And     | Reg    | Reg    | Reg    | "and"  |    true     |
    | Eor     | Reg    | Reg    | Reg    | "eor"  |    true     |
    | Mov     | Reg    | Reg    | Unused | "mov"  |    false    |
    | MovI    | Reg    | Imm    | Unused | "mov"  |    false    |
    | Mul     | Reg    | Reg    | Reg    | "mul"  |    true     |
    | Orr     | Reg    | Reg    | Reg    | "orr"  |    true     |
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

impl<W: Word> From<W> for Register {
    fn from(x: W) -> Self {
        Register(x.into_word::<Word8>().into())
    }
}

/// A single instruction.
#[derive(derive_more::Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[debug("{op_code:?}{}{args:?}",
    match cond_code {
        CondCode::Al => "".to_string(),
        _ => format!("{cond_code:?}"),
    }
)]
pub struct Inst<W> {
    pub op_code: OpCode,
    pub cond_code: CondCode,
    pub args: [W; 3],
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
    #[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
    #[display("{}", Flags::from(*self))]
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

// =========================================== State ==============================================

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct State<W> {
    /// This vector is always sorted by register. Registers that are zero are omitted.
    /// TODO: Change this!
    pub flags: FlagsBitField,
    pub registers: [W; Register::COUNT as usize],
}

impl<W: Copy> State<W> {
    pub fn get_register(&self, reg: Register) -> W {
        self.registers[reg.0 as usize]
    }
    pub fn set_register(&mut self, reg: Register, x: W) {
        self.registers[reg.0 as usize] = x;
    }
    pub fn get_flags(&self) -> FlagsBitField {
        // self.flags.expect("Flags not set in state.")
        self.flags
    }
    pub fn set_flags(&mut self, flags: FlagsBitField) {
        self.flags = flags
    }
    /// Copies this state to another state object. Used to avoid clones, that in a loop, can
    /// allocate more.
    #[inline]
    pub fn clone_to(&self, other: &mut Self) {
        other.registers = self.registers;
        other.flags = self.flags;
    }
    pub fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> State<WSmall>
        where W: Word
    {
        State {
            registers: self
                .registers
                .map(|v| reducer.reduce(v, &Default::default())),
            flags: self.flags,
        }
    }
    #[inline]
    pub fn clear(&mut self) where W: Default {
        self.registers = [W::default(); _];
        self.flags = FlagsBitField::empty();
    }

    pub fn all_each(ei: &EnumerationInfoOptions<Register>, mut f: impl FnMut(&Self))
        where W: All + Default, W::Iter: Clone
    {
        // fn or_none<T: Clone>(
        //     i: impl Clone + Iterator<Item = T>,
        // ) -> impl Clone + Iterator<Item = Option<T>> {
        //     [None].into_iter().chain(i.map(Some))
        // }

        let registers = ei.into_iter().collect::<Box<[_]>>();
        let reg_value_iter = registers.iter().map(|_| W::all()).collect::<Box<[_]>>();
        let mut iter = PermutationIter::new(&reg_value_iter);
        let mut state = State::default();
        while let Some(reg_values) = iter.next_slice() {
            state.clear();
            for (r, &v) in registers.iter().cloned().zip(reg_values) {
                state.set_register(r, v);
            }
            for flags in Flags::ALL {
                state.set_flags(flags.into());
                f(&state)
            }
        }
    }
}

impl<W: Word, F, I> From<(F, I)> for State<W>
where
    F: Into<Flags>,
    I: IntoIterator<Item = (Register, W)>,
{
    fn from((f, i): (F, I)) -> Self {
        let mut ret = Self::default();
        // Flags
        ret.set_flags(f.into().into());
        // Registers
        for (r, v) in i {
            ret.set_register(r, v);
        }
        ret
    }
}

use std::fmt::{self, Display, Formatter};
impl<W: Word> Display for State<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.get_flags())?;
        for r in Register::all() {
            let v = self.get_register(r);
            if !v.is_zero() {
                write!(f, " {r}={v:>2}")?;
            }
        }
        Ok(())
    }
}

impl<W: Word> collect_registers::State<W> for State<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W)> {
        Register::all().filter_map(|r| {
            if !self.get_register(r).is_zero() {
                Some((r, self.get_register(r)))
            } else {
                None
            }
        })
    }
}

impl<W: Word> oracle::test_cases::State for State<W> {
    fn clone_to(&self, output: &mut Self) {
        self.clone_to(output);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StateVars<'st, W> {
    pub registers: [Const<'st, W>; Register::COUNT as usize],
    pub flags: Flags<Const<'st, SmtBool<'st>>>,
}

impl<'st, W: SmtWord<'st>> StateVars<'st, W> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            registers: std::array::from_fn(|i| {
                W::new_const(st, &format!("{name}_r{i}"))
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

// TODO: Unify with [State]
#[derive(Clone, Copy, Debug)]
pub struct SymbolicState<'st, W> {
    pub registers: [W; Register::COUNT as usize],
    pub flags: Flags<SmtBool<'st>>,
    // TODO: Live mask
}

impl<'st, W: SmtWord<'st>> From<StateVars<'st, W>> for SymbolicState<'st, W> {
    fn from(value: StateVars<'st, W>) -> Self {
        Self {
            registers: value.registers.map(Into::into),
            flags: value.flags.into(),
        }
    }
}

impl<'st, W: SmtWord<'st>> SymbolicState<'st, W> {
    pub fn eq(&self, other: Self) -> SmtBool<'st> {
        let regs = self.registers.iter().zip(other.registers);
        let regs_eq = regs
            .map(|(ra, rb)| ra._eq(rb))
            .reduce(|b1, b2| b1 & b2)
            .unwrap();
        regs_eq & self.flags.eq(&other.flags)
    }
}

fn run_instruction<W: Word>(inst: &Inst<W>, state: &mut State<W>) {
    /// Get a register value.
    macro_rules! r {
        ($i:literal) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
            let r = Register(inst.args[$i].into_word::<Word8>().into());
            state.get_register(r)
        }};
    }
    /// Set a register value.
    macro_rules! set {
        (r![$i:literal] <- $value:expr) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
            let r = Register(inst.args[$i].into_word::<Word8>().into());
            state.set_register(r, $value)
        }};
        (flags <- $f:ident( $($e:expr),* )) => {{
            let mut flags: Flags = state.get_flags().into();
            flags.$f::<W>( $($e),*, true );
            state.set_flags(flags.into());
        }};
    }
    /// Get an immediate value.
    macro_rules! imm {
        ($i:literal) => { inst.args[$i] };
    }

    // Skip the instruction if it is skipped by the flags.
    if !inst.cond_code.check(state.get_flags().into()) {
        return;
    }

    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => {
            set! { flags <- update_from_add(r![1], r![2]) };
            set! { r![0] <- r![1] + r![2] };
        }
        AddI => {
            set! { flags <- update_from_add(r![1], imm![2]) };
            set! { r![0] <- r![1] + imm![2] };
        }
        Sub => {
            set! { flags <- update_from_sub(r![1], r![2]) };
            set! { r![0] <- r![1] - r![2] };
        }
        SubI => {
            set! { flags <- update_from_sub(r![1], imm![2]) };
            set! { r![0] <- r![1] - imm![2] };
        }
        And => set!(r![0] <- r![1] & r![2]),
        Eor => set!(r![0] <- r![1] ^ r![2]),
        Mov => set!(r![0] <- r![1]),
        MovI => set!(r![0] <- imm![1]),
        // Mul => set!(r![0 i] <- r![1 i].overflowing_mul(r![2 i]).0),
        Mul => set!(r![0] <- r![1] * r![2]),
        Orr => set!(r![0] <- r![1] | r![2]),
    }
}

fn run_instruction_symbolic<'st, W: Word>(inst: &Inst<W>, state: &mut SymbolicState<'st, W::SmtWord<'st>>) {
    let enabled = inst.cond_code.check(state.flags);

    /// Get a register value.
    macro_rules! r {
        ($i:literal) => {
            state.registers[{
                debug_assert!($i < 3);
                debug_assert!(inst.op_code.arg_types()[$i] == ArgType::Reg);
                let r = Register(inst.args[$i].into_word::<Word8>().into());
                r.0 as usize
            }]
        };
    }
    /// Set a register value. Also checks the condition code.
    macro_rules! set {
        (r![$i:literal] <- $e:expr) => {{ r![$i] = enabled.if_then_else($e, r![$i]); }};
        (flags <- $f:ident($($e:expr),*)) => {{ state.flags.$f::<W::SmtWord<'st>>($($e),* , enabled); }};
    }
    /// Get an immediate value.
    macro_rules! imm {
        ($i:literal) => {
            inst.args[$i].into_smt_word(state.registers[0].st())
        };
    }

    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => {
            set! { flags <- update_from_add(r![1], r![2]) };
            set! { r![0] <- r![1] + r![2] };
        }
        AddI => {
            set! { flags <- update_from_add(r![1], imm![2]) };
            set! { r![0] <- r![1] + imm![2] };
        }
        Sub => {
            set! { flags <- update_from_sub(r![1], r![2]) };
            set! { r![0] <- r![1] + -r![2] };
        }
        SubI => {
            set! { flags <- update_from_sub(r![1], imm![2]) };
            set! { r![0] <- r![1] + -imm![2] };
        }
        And => set! { r![0] <- r![1] & r![2] },
        Eor => set! { r![0] <- r![1] ^ r![2] },
        Mov => set! { r![0] <- r![1] },
        MovI => set! { r![0] <- imm![1] },
        Mul => set! { r![0] <- r![1] * r![2] },
        Orr => set! { r![0] <- r![1] | r![2] },
    }
}

/// A hash-map between instructions and output states to input states that send to the output. The
/// states use a liveness mask to mark which registers are ignored, and they all ignore the
/// condition flag. Currently, states also don't represent memory, so we don't need to worry about
/// that.
///
/// About the condition flag again: all instructions in the map have a condition flag of always.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BackwardMap<W: Word> {
    pub map: FxHashMap<(Inst<W>, State<W>), Inputs<W>>,
    empty_vec: Vec<State<W>>,
    // The registers to consider when indexing into the map. These are the registers that are
    // "live" in the states, and other registers should be ignored.
    registers: Vec<Register>,
}
pub type Inputs<W> = Vec<State<W>>;

impl<W: Word> BackwardMap<W> {
    pub fn new(registers: &[Register]) -> std::io::Result<Self>
    where
        Self: Serialize + for<'a> Deserialize<'a>,
        <W as All>::Iter: Clone
    {
        let file_path = std::path::Path::new(".").join(Self::file_name(registers));
        if file_path.exists() {
            println!("loading backwards map from '{}'", file_path.display());
            let f = std::fs::File::open(file_path)?;
            let reader = std::io::BufReader::new(&f);
            Self::load(reader)
        } else {
            println!("creating backwards map");
            let f = std::fs::File::create(&file_path)?;
            let this = Self::new_recalculate(registers);
            println!("saving backwards map to '{}'", file_path.display());
            let mut writer = std::io::BufWriter::new(&f);
            this.save(&mut writer)?;
            std::io::Write::flush(&mut writer)?;
            Ok(this)
        }
    }

    pub fn file_name(registers: &[Register]) -> String {
        let mut ret = String::new();
        ret.push_str("backward-map-");
        ret.push_str(&W::BITS.to_string());
        ret.push_str("bit");
        for r in registers {
            ret.push_str(&format!("-{r}"));
        }
        ret.push_str(".postcard"); // This is the name of the format we use
        ret
    }

    pub fn save(&self, w: impl std::io::Write) -> std::io::Result<()>
    where
        Self: Serialize,
    {
        postcard::to_io(self, w).map_err(std::io::Error::other)?;
        Ok(())
    }

    pub fn load(r: impl std::io::Read) -> std::io::Result<Self>
    where
        Self: for<'a> Deserialize<'a>,
    {
        let mut buf = [0; 1024 * 1024];
        let (this, _) = postcard::from_io((r, &mut buf)).map_err(std::io::Error::other)?;
        Ok(this)
    }

    /// Build from scratch a new backwards behavior map.
    pub fn new_recalculate(registers: &[Register]) -> Self where <W as All>::Iter: Clone {
        let mut ret = FxHashMap::default();
        let mut i = 0;
        let ei = EnumerationInfoOptions::Limited(registers);
        State::all_each(&ei, |input| {
            if i % 100 == 0 {
                dbg!(i);
            }
            i += 1;
            let mut output = State::default();
            let ei = EnumerationInfo {
                registers: EnumerationInfoOptions::Limited(registers),
                immediates: EnumerationInfoOptions::Unlimited,
            };
            for inst in Enumerator::new().into_iter(&ei) {
                input.clone_to(&mut output);
                inst.run(&mut output);
                // Store!
                let inputs = ret.entry((inst, output.clone())).or_insert_with(Vec::new);
                if !inputs.contains(input) {
                    inputs.push(input.clone());
                }
            }
        });
        Self {
            map: ret,
            empty_vec: vec![],
            registers: registers.to_vec(),
        }
    }
}

impl<W: Word> std::ops::Index<(Inst<W>, State<W>)> for BackwardMap<W> {
    type Output = [State<W>];

    fn index(&self, (inst, mut state): (Inst<W>, State<W>)) -> &Self::Output {
        // Clear the registers that don't matter.
        for r in Register::all() {
            if !self.registers.contains(&r) {
                state.set_register(r, 0.into());
            }
        }
        self.map
            .get(&(inst, state))
            .map(|v| v.as_slice())
            .unwrap_or(&self.empty_vec)
    }
}

impl<W: Copy + Into<Register>> Inst<W> {
    pub fn regs(&self) -> impl Iterator<Item = Register> {
        let mut ret = vec![];
        for (a, t) in self.args.iter().zip(self.op_code.arg_types()) {
            if t == ArgType::Reg {
                ret.push((*a).into());
            }
        }
        ret.into_iter()
    }
}

impl<W: Word> Inst<W> {
    pub fn run(&self, state: &mut State<W>) {
        run_instruction(self, state)
    }

    pub fn run_symbolic<'st>(&self, state: &mut SymbolicState<'st, W::SmtWord<'st>>) {
        run_instruction_symbolic(self, state)
    }

    pub fn run_backward<'a>(
        &self,
        state: State<W>,
        bm: &'a BackwardMap<W>,
    ) -> impl IntoIterator<Item = &'a State<W>> + use<'a, W> {
        &bm[(*self, state)]
    }

    pub fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> Inst<WSmall> {
        fn reduce_arg<W: Word, WSmall: Word>(
            reducer: &mut Reducer<W, WSmall>,
            arg: W,
            arg_type: ArgType,
            info: &ImmediateInfo,
        ) -> WSmall {
            match arg_type {
                ArgType::Imm => reducer.reduce(arg, info),
                ArgType::Reg | ArgType::Unused => arg.into_word(),
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
            arg: WSmall,
            arg_type: ArgType,
        ) -> SliceOrSingle<'_, WBig> {
            match arg_type {
                ArgType::Imm => reducer.extend(arg),
                ArgType::Reg | ArgType::Unused => SliceOrSingle::Single(arg.into_word()),
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

impl<W: Display> Display for Inst<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
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
        if *cond_code == CondCode::Al {
            write!(f, "{op_code} {}, {}, {}", args[0], args[1], args[2])
        } else {
            write!(f, "{op_code}{cond_code} {}, {}, {}", args[0], args[1], args[2])
        }
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
                    args_iter.get(0).cloned().unwrap_or(0usize).into(),
                    args_iter.get(1).cloned().unwrap_or(0usize).into(),
                    args_iter.get(2).cloned().unwrap_or(0usize).into(),
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
    fn test_update_from_sub_zero() {
        let mut flags = Flags::<bool>::default();
        flags.update_from_sub::<Word64>(5usize.into(), 5usize.into(), true);
        assert!(flags.z);
        assert!(!flags.n);
        assert!(flags.c); // no borrow
        assert!(!flags.v);
    }

    #[test]
    fn test_update_from_sub_positive() {
        let mut flags = Flags::<bool>::default();
        flags.update_from_sub::<Word64>(10usize.into(), 3usize.into(), true);
        assert!(!flags.z);
        assert!(!flags.n); // 7 is positive
        assert!(flags.c); // no borrow (10 >= 3)
        assert!(!flags.v);
    }

    #[test]
    fn test_update_from_sub_negative() {
        let mut flags = Flags::<bool>::default();
        flags.update_from_sub::<Word64>(3usize.into(), 10usize.into(), true);
        assert!(!flags.z);
        assert!(flags.n); // result is negative (wraps)
        assert!(!flags.c); // borrow occurred (3 < 10)
        assert!(!flags.v);
    }

    #[test]
    fn test_update_from_sub_overflow_positive() {
        // Positive - Negative = Negative (overflow)
        // For u8: 127 - (-128) = 127 - 128 (as unsigned) = 127 - 128 (wraps)
        let mut flags = Flags::<bool>::default();
        flags.update_from_sub::<Word8>(127usize.into(), 128usize.into(), true); // 127 - (-128 as u8)
        assert!(flags.n); // wrapped to negative
        assert!(flags.v); // overflow occurred
    }

    #[test]
    fn test_update_from_sub_overflow_negative() {
        // Negative - Positive = Positive (overflow)
        // For u8: 128 (as -128 signed) - 1 = wraps to 127
        let mut flags = Flags::<bool>::default();
        flags.update_from_sub::<Word8>(128usize.into(), 1usize.into(), true);
        assert!(!flags.n); // wrapped to positive
        assert!(flags.v); // overflow occurred
    }

    #[test]
    fn test_update_from_sub_disabled() {
        let mut flags = Flags {
            z: true,
            n: true,
            c: true,
            v: true,
        };
        flags.update_from_sub::<Word64>(10usize.into(), 3usize.into(), false);
        // All flags should remain unchanged
        assert!(flags.z);
        assert!(flags.n);
        assert!(flags.c);
        assert!(flags.v);
    }

    #[test]
    fn test_backward_map_some_not_empty() {
        type W = Word4;
        let bm = BackwardMap::<W>::new_recalculate(&[Register(1)]);
        let mut n_empty = 0;
        bm.map.iter().for_each(|((inst, state), inputs)| {
            println!("Instruction: {inst}, Output State: {state}");
            println!("Input States:");
            inputs.iter().for_each(|input| print!("  {input}"));
            println!();
            if inputs.is_empty() {
                n_empty += 1;
            }
        });
        println!("Number of entries with empty input states: {n_empty}");
        assert!(n_empty < bm.map.len());
    }

    #[test]
    fn test_backward_map() {
        type W = Word4;
        let bm = BackwardMap::<W>::new_recalculate(&[Register(1)]);
        let inst: Inst<W> = inst!(AddI, 1, 1, 5);
        let mut output = State::<W>::default();
        output.set_register(Register(1), 12.into());
        output.set_register(Register(2), 6.into());
        output.set_flags(
            Flags {
                z: false,
                n: true,
                c: false,
                v: true,
            }
            .into(),
        );
        let inputs = inst
            .run_backward(output, &bm)
            .into_iter()
            .collect::<Vec<_>>();
        dbg!(&inputs);
        assert_eq!(inputs.len(), 16);
    }

    #[test]
    fn run_nop_backwards_one_option() {
        type W = Word4;
        let bm = BackwardMap::<W>::new_recalculate(&[Register(1)]);
        let inst = inst!(Nop,);
        let mut output = State::<W>::default();
        output.set_register(Register(1), 12.into());
        output.set_register(Register(2), 6.into());
        output.set_flags(
            Flags {
                z: false,
                n: true,
                c: false,
                v: true,
            }
            .into(),
        );
        let inputs = inst
            .run_backward(output, &bm)
            .into_iter()
            .collect::<Vec<_>>();
        dbg!(&inputs);
        assert_eq!(inputs.len(), 1);
    }

    #[test]
    #[ignore]
    fn backwards_map_specific_state() {
        type W = Word4;
        let bm = BackwardMap::<W>::new(&[Register(0), Register(1)]).unwrap();
        let mut state = State::<W>::default();
        state.set_register(Register(0), 15.into());
        state.set_register(Register(1), 15.into());
        state.set_flags(
            Flags {
                z: false,
                n: true,
                c: false,
                v: true,
            }
            .into(),
        );
        let ei = EnumerationInfo {
            registers: EnumerationInfoOptions::Limited(&[Register(0), Register(1)]),
            immediates: EnumerationInfoOptions::Limited(&[0.into(), 1.into(), 5.into()]),
        };
        for inst in Enumerator::new().into_iter(&ei) {
            let x = &bm[(inst, state.clone())];
            println!("Instruction: {inst}, Output State: {state}");
            println!("Input States: {x:?}");
            if !x.is_empty() {
                println!("Found non-empty input states for this instruction and output state!");
                return;
            }
        }
        panic!("No instruction produced non-empty input states for the given output state!");
    }
}
