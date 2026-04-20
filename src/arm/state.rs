//! State types and state-related logic for the ARM-like ISA.

use super::Register;
use crate::all::All;
use crate::all_permutations::Iter as PermutationIter;
use crate::arm::enumerate::EnumerationInfoOptions;
use crate::bool::all_eq;
use crate::bool::prelude::*;
use crate::collect_registers;
use crate::reduce_bit_width::Reducer;
use crate::word::prelude::*;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::ControlFlow;
// derive macros
use derive_more::Display;
use serde::{Deserialize, Serialize};
// smt
use smtlib::Storage;
use smtlib::prelude::*;
use smtlib::terms::Const;
// proptest
#[cfg(test)]
use proptest::prelude::*;
#[cfg(test)]
use proptest_derive::*;
// other
use itertools::Itertools;

// ========================================== Traits ==============================================

pub trait Get<W: AbstractWord> {
    fn reg(&self, r: Register) -> W;
    fn flags(&self) -> Flags<W::Bool>;
}

pub trait Set<W: AbstractWord> {
    fn maybe_set_reg(&mut self, r: Register, cond: W::Bool, w: W);
    fn maybe_set_flags(&mut self, cond: W::Bool, f: Flags<W::Bool>);
    #[allow(unused)]
    fn set_reg(&mut self, r: Register, x: W) {
        self.maybe_set_reg(r, W::Bool::r#true(), x);
    }
    #[allow(unused)]
    fn set_flags(&mut self, f: Flags<W::Bool>) {
        self.maybe_set_flags(W::Bool::r#true(), f)
    }
}

pub trait StateTrait<W: AbstractWord>: Get<W> + Set<W> {}

// ============================= State =============================

#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct State<W> {
    /// This vector is always sorted by register. Registers that are zero are omitted.
    /// TODO: Change this!
    pub flags: FlagsBitField,
    pub registers: [W; Register::COUNT as usize],
}

// TODO: Unify with [State]
#[derive(Clone, Copy, Debug)]
pub struct SymbolicState<'st, W> {
    pub registers: [W; Register::COUNT as usize],
    pub flags: Flags<SmtBool<'st>>,
}

#[derive(Clone, Copy, Debug)]
pub struct StateVars<'st, W> {
    pub registers: [Const<'st, W>; Register::COUNT as usize],
    pub flags: Flags<Const<'st, SmtBool<'st>>>,
}

// ============================= Flags =============================

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Flags<B = bool> {
    pub z: B,
    pub n: B,
    pub c: B,
    pub v: B,
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

// ============================ Mask ============================

/// A liveness mask. For each register, is it used or not. Has just one flag for the whole flags
/// register (because flags are used and written to together).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct Mask<B = bool> {
    pub registers: [B; Register::COUNT as usize],
    pub flags: B,
}

/// [Mask], but compacted to a bit-field.
#[derive(
    Clone, Copy, derive_more::Debug, Display, Default, PartialEq, Eq, Hash, Serialize, Deserialize,
)]
#[debug("{:?}", Mask::from(*self))]
#[display("{}", Mask::from(*self))]
pub struct BitMask(u32);

// =========================== Masked State ===========================

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Masked<W> {
    state: State<W>,
    mask: BitMask,
}

// ================================== Implementing The Trait =======================================

// --- State ---

#[rustfmt::skip]
impl<W: Word> Get<W> for State<W> {
    fn reg(&self, r: Register) -> W { self[r] }
    fn flags(&self) -> Flags        { self.flags.into() }
}

#[rustfmt::skip]
impl<W: Word> Set<W> for State<W> {
    fn maybe_set_reg(&mut self, r: Register, cond: bool, w: W) { if cond { self[r] = w; } }
    fn maybe_set_flags(&mut self, cond: bool, f: Flags)        { if cond { self.flags = f.into(); } }
}

impl<W: Word> StateTrait<W> for State<W> {}

// --- Symbolic State ---

#[rustfmt::skip]
impl<'st, W: SmtWord<'st>> Get<W> for SymbolicState<'st, W> {
    fn reg(&self, r: Register) -> W        { self[r] }
    fn flags(&self) -> Flags<SmtBool<'st>> { self.flags }
}

impl<'st, W: SmtWord<'st>> Set<W> for SymbolicState<'st, W> {
    fn maybe_set_reg(&mut self, r: Register, cond: SmtBool<'st>, w: W) {
        self[r] = cond.if_then_else(w, self[r]);
    }
    fn maybe_set_flags(&mut self, cond: SmtBool<'st>, f: Flags<SmtBool<'st>>) {
        self.flags = Flags {
            z: cond.if_then_else(f.z, self.flags.z),
            n: cond.if_then_else(f.n, self.flags.n),
            c: cond.if_then_else(f.c, self.flags.c),
            v: cond.if_then_else(f.v, self.flags.v),
        };
    }
}

impl<'st, W: SmtWord<'st>> StateTrait<W> for SymbolicState<'st, W> {}

// ============================ State impl ============================

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
    where
        W: Word,
    {
        State {
            registers: self
                .registers
                .map(|v| reducer.reduce(v, &Default::default())),
            flags: self.flags,
        }
    }
    #[inline]
    pub fn clear(&mut self)
    where
        W: Default,
    {
        self.registers = [W::default(); _];
        self.flags = FlagsBitField::empty();
    }

    pub fn masked(self, mask: Mask) -> Masked<W>
    where
        W: Default,
    {
        Masked::from(self) & mask
    }

    pub fn try_all_each<T>(
        ei: &EnumerationInfoOptions<Register>,
        mut f: impl FnMut(&Self) -> ControlFlow<T>,
    ) -> ControlFlow<T>
    where
        W: All + Default,
        W::Iter: Clone,
    {
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
                f(&state)?
            }
        }
        ControlFlow::Continue(())
    }

    pub fn all_each(ei: &EnumerationInfoOptions<Register>, mut f: impl FnMut(&Self))
    where
        W: All + Default,
        W::Iter: Clone,
    {
        let _ = Self::try_all_each::<()>(ei, |s| {
            f(s);
            ControlFlow::Continue(())
        });
    }
}

impl<W> Index<Register> for State<W> {
    type Output = W;
    fn index(&self, r: Register) -> &W {
        &self.registers[r.0 as usize]
    }
}

impl<W> IndexMut<Register> for State<W> {
    fn index_mut(&mut self, r: Register) -> &mut W {
        &mut self.registers[r.0 as usize]
    }
}

impl<W: PartialEq> State<W> {
    pub fn diff(&self, other: &Self) -> Mask {
        Mask {
            flags: self.flags != other.flags,
            registers: Register::ALL.map(|r| self[r] != other[r]),
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

impl<W: Debug> Debug for State<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.flags)?;
        for (r, v) in Register::all().zip(&self.registers) {
            write!(f, " {r}={v:>2?}")?;
        }
        Ok(())
    }
}

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

// ======================================= Symbolic State ==========================================

impl<'st, W> Index<Register> for SymbolicState<'st, W> {
    type Output = W;
    fn index(&self, r: Register) -> &W {
        &self.registers[r.0 as usize]
    }
}

impl<'st, W> IndexMut<Register> for SymbolicState<'st, W> {
    fn index_mut(&mut self, r: Register) -> &mut W {
        &mut self.registers[r.0 as usize]
    }
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
        let SymbolicState { registers, flags } = self;
        let regs = registers.iter().zip(other.registers);
        let regs_eq = regs
            .map(|(ra, rb)| ra._eq(rb))
            .reduce(|b1, b2| b1 & b2)
            .unwrap();
        regs_eq & flags.eq(&other.flags)
    }
}

impl<'st, W: SmtWord<'st>> StateVars<'st, W> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            registers: std::array::from_fn(|i| W::new_const(st, &format!("{name}_r{i}"))),
            flags: Flags::new(st, name),
        }
    }
}

//     State Masking

impl<W: Copy + Default> State<W> {
    pub fn mask_or_default(self, mask: Mask) -> Self {
        State {
            flags: if mask.flags {
                self.flags
            } else {
                FlagsBitField::default()
            },
            registers: Register::ALL.map(|r| {
                if mask[r] {
                    self.get_register(r)
                } else {
                    W::default()
                }
            }),
        }
    }
}

// ============================ Flags impl ============================

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

impl<B: Bool> Flags<B> {
    pub fn from_add<W: AbstractWord<Bool = B>>(op1: W, op2: W) -> Self {
        let sum = op1 + op2;
        let z = sum.is_zero();
        let n = sum.signed_negative();
        let c = sum.unsigned_lt(&op1);
        let both_positive = op1.signed_positive() & op2.signed_positive();
        let both_negative = op1.signed_negative() & op2.signed_negative();
        let v = (both_positive & sum.signed_lt(&op1)) | (both_negative & sum.signed_positive());
        Flags { z, n, c, v }
    }

    pub fn from_sub<W: AbstractWord<Bool = B>>(op1: W, op2: W) -> Self {
        let diff = op1 + (-op2);
        let z = diff.is_zero();
        let n = diff.signed_negative();
        let c = !op1.unsigned_lt(&op2);
        let op1_positive = op1.signed_positive();
        let op2_negative = op2.signed_negative();
        let op1_negative = op1.signed_negative();
        let op2_positive = op2.signed_positive();
        let v = (op1_positive & op2_negative & diff.signed_negative())
            | (op1_negative & op2_positive & diff.signed_positive());
        Flags { z, n, c, v }
    }

    pub fn from_and<W: AbstractWord<Bool = B>>(a: W, b: W) -> Self {
        let z = (a & b).is_zero();
        let n = (a & b).signed_negative();
        Flags {
            z,
            n,
            c: B::r#false(),
            v: B::r#false(),
        }
    }
}

impl Flags {
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

// ============================ Mask impl ============================

impl Mask {
    pub const EMPTY: Self = Mask {
        flags: false,
        registers: [false; _],
    };
    pub const FULL: Self = Mask {
        flags: true,
        registers: [true; _],
    };
    pub const JUST_FLAGS: Self = Mask {
        flags: true,
        registers: [false; _],
    };
    pub const fn just_register(r: Register) -> Self {
        let mut ret = Self::EMPTY;
        ret.registers[r.0 as usize] = true;
        ret
    }

    pub fn into_bit_mask(self) -> BitMask {
        self.into()
    }

    pub fn registers(&self) -> impl Iterator<Item = Register> {
        Register::ALL.into_iter().filter(|r| self[*r])
    }

    /// The sub-mask are the masks that contain only a single thing
    pub fn singleton_sub_masks(self) -> impl Iterator<Item = Mask> + Clone {
        return I(self);

        #[derive(Clone)]
        struct I(Mask);
        impl Iterator for I {
            type Item = Mask;
            fn next(&mut self) -> Option<Self::Item> {
                if let Some(r) = Register::ALL.into_iter().find(|r| self.0[*r]) {
                    self.0[r] = false;
                    return Some(Mask::just_register(r));
                }
                if self.0.flags {
                    self.0.flags = false;
                    return Some(Mask::JUST_FLAGS);
                }
                None
            }
        }
    }

    /// This is the power-set!
    pub fn sub_masks(self) -> impl Iterator<Item = Mask> + Clone {
        self.singleton_sub_masks().powerset().map(|singletons| {
            singletons
                .into_iter()
                .reduce(|x, y| x | y)
                .unwrap_or_default()
        })
    }

    pub fn len(&self) -> usize {
        let bits = self.into_bit_mask();
        bits.0.count_ones() as usize
    }
}

impl<B> Mask<B> {
    pub fn map<B2>(self, mut f: impl FnMut(B) -> B2) -> Mask<B2> {
        Mask {
            registers: self.registers.map(&mut f),
            flags: f(self.flags),
        }
    }

    pub fn empty() -> Self
    where
        B: Bool,
    {
        Mask {
            flags: B::r#false(),
            registers: [B::r#false(); _],
        }
    }

    pub fn is_sub_mask(&self, other: &Self) -> B
    where
        B: Bool,
        Self: BoolEq<B>,
    {
        (*self & *other).eq(self)
    }
}

impl<'st> BoolEq<SmtBool<'st>> for Mask<SmtBool<'st>> {
    fn eq(&self, other: &Self) -> SmtBool<'st> {
        let Mask { registers, flags } = self;
        all_eq(registers.iter().zip(&other.registers)) & flags.eq(&other.flags)
    }
}

impl BitMask {
    pub const EMPTY: BitMask = BitMask(0);

    pub fn into_mask(self) -> Mask {
        self.into()
    }

    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub const fn is_sub_mask(&self, other: &Self) -> bool {
        (self.0 & other.0) == self.0
    }
}

impl<B> Index<Register> for Mask<B> {
    type Output = B;
    fn index(&self, r: Register) -> &B {
        &self.registers[r.0 as usize]
    }
}

impl<B> IndexMut<Register> for Mask<B> {
    fn index_mut(&mut self, r: Register) -> &mut B {
        &mut self.registers[r.0 as usize]
    }
}

impl Display for Mask {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut words = vec![];
        // Flags
        if self.flags {
            words.push("F".to_string());
        }
        // Registers
        for r in Register::all() {
            if self[r] {
                words.push(r.to_string());
            }
        }
        // Finalize
        let mut joined = words.join(",");
        if joined.is_empty() {
            joined.push('-');
        }
        write!(f, "{}", joined)?;
        Ok(())
    }
}

impl From<Mask> for BitMask {
    fn from(m: Mask) -> BitMask {
        let mut ret = 0u32;
        // Flags
        if m.flags {
            ret = 1
        };
        // Registers
        for r in Register::all() {
            if m[r] {
                // Reg i is stored in the (i + 1)th bit
                ret |= 1 << (1 + r.0);
            }
        }
        // Return
        BitMask(ret)
    }
}

impl From<BitMask> for Mask {
    fn from(b: BitMask) -> Mask {
        Mask {
            flags: (b.0 & 1) > 0,
            // Reg i is stored in the (i + 1)th bit
            registers: Register::ALL.map(|r| ((b.0 >> (1 + r.0)) & 1) > 0),
        }
    }
}

// It's much easier to implement `All` for the bit-masks
impl All for BitMask {
    type Iter = std::iter::Map<std::ops::Range<u32>, fn(u32) -> BitMask>;
    fn all() -> Self::Iter {
        (0..BitMask::from(Mask::FULL).0 + 1).map(Self)
    }
}

#[cfg(test)]
impl Arbitrary for BitMask {
    type Parameters = ();
    type Strategy = prop::strategy::Map<std::ops::Range<u32>, fn(u32) -> BitMask>;
    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (0..Mask::FULL.into_bit_mask().0 + 1).prop_map(BitMask)
    }
}

#[cfg(test)]
impl Arbitrary for Mask {
    type Parameters = ();
    type Strategy = prop::arbitrary::Mapped<[bool; 17], Mask>;
    #[rustfmt::skip]
    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        any::<[bool; _]>().prop_map(
            |[flags, r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]| {
                Mask {
                    flags,
                    registers: [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15],
                }
            },
        )
    }
}

// --- Mask operations ---
use std::ops::{BitAnd, BitOr, BitXor, Index, IndexMut, Not};

// Bit-Mask

impl BitAnd<BitMask> for BitMask {
    type Output = BitMask;
    fn bitand(self, other: BitMask) -> Self::Output {
        Self(self.0 & other.0)
    }
}

impl BitOr<BitMask> for BitMask {
    type Output = BitMask;
    fn bitor(self, other: BitMask) -> Self::Output {
        Self(self.0 | other.0)
    }
}

impl BitXor<BitMask> for BitMask {
    type Output = BitMask;
    fn bitxor(self, other: Self) -> BitMask {
        Self((self.0 ^ other.0) & BitMask::from(Mask::FULL).0)
    }
}

impl Not for BitMask {
    type Output = BitMask;
    fn not(self) -> BitMask {
        Self(!self.0 & BitMask::from(Mask::FULL).0)
    }
}

// Mask

impl<B: Bool> BitAnd<Mask<B>> for Mask<B> {
    type Output = Mask<B>;
    fn bitand(self, other: Mask<B>) -> Self::Output {
        Self {
            flags: self.flags & other.flags,
            registers: Register::ALL.map(|r| self[r] & other[r]),
        }
    }
}

impl<B: Bool> BitOr<Mask<B>> for Mask<B> {
    type Output = Mask<B>;
    fn bitor(self, other: Mask<B>) -> Self::Output {
        Self {
            flags: self.flags | other.flags,
            registers: Register::ALL.map(|r| self[r] | other[r]),
        }
    }
}

impl<B: Bool> BitXor<Mask<B>> for Mask<B> {
    type Output = Mask<B>;
    fn bitxor(self, other: Self) -> Self {
        Self {
            flags: self.flags ^ other.flags,
            registers: Register::ALL.map(|r| self[r] ^ other[r]),
        }
    }
}

impl<B: Bool> Not for Mask<B> {
    type Output = Mask<B>;
    fn not(self) -> Self {
        Self {
            flags: !self.flags,
            registers: Register::ALL.map(|r| !self[r]),
        }
    }
}

// ============================ Masked State impl ============================

#[rustfmt::skip]
impl<W> Masked<W> {
    pub fn mask(&self) -> BitMask { self.mask }
    pub fn state(&self) -> &State<W> { &self.state }
}

impl<W> Masked<W> {
    /// Returns the singleton sub-masked-states of this masked state. That is, the states that
    /// contain only a single part of this state's mask.
    pub fn singleton_sub_states(self) -> impl Iterator<Item = Self>
    where
        W: Copy + Default,
    {
        self.mask
            .into_mask()
            .singleton_sub_masks()
            .map(move |m| self & m)
    }

    pub fn sub_states(self) -> impl Iterator<Item = Self> + Clone
    where
        W: Copy + Default,
    {
        self.mask.into_mask().sub_masks().map(move |m| self & m)
    }

    pub fn is_sub_state(&self, other: &Self) -> bool
    where
        W: Word,
    {
        self.mask().is_sub_mask(&other.mask()) && *self == *other & self.mask().into_mask()
    }

    /// TODO: Move to a new 'Effect' type?
    pub fn compose((i1, o1): (Self, Self), (i2, o2): (Self, Self)) -> Option<(Self, Self)>
    where
        W: Word,
    {
        let conflict: Mask = (o1.mask() & i2.mask()).into_mask();
        // The inputs don't match
        if o1 & conflict != i2 & conflict {
            return None;
        }
        // Run!
        // The input is whatever both take as input, and the first guy doesn't give to the second
        let i = i1 | (i2 & !conflict);
        // The output is just whatever both of them give
        let o = o2 | o1;
        Some((i, o))
    }
}

impl<W> From<State<W>> for Masked<W> {
    fn from(state: State<W>) -> Masked<W> {
        Masked {
            state,
            mask: Mask::FULL.into(),
        }
    }
}

// And'ing a masked state with a mask activates the mask on it
impl<W: Copy + Default> BitAnd<Mask> for Masked<W> {
    type Output = Self;
    fn bitand(self, mask: Mask) -> Self {
        Self {
            mask: self.mask & mask.into(),
            state: self.state.mask_or_default(mask),
            // mask mask mask mask...
        }
    }
}

// Or'ing two masked states returns the first state, with the values of the second state where this
// one is masked away.
impl<W: Copy + Default> BitOr for Masked<W> {
    type Output = Self;
    fn bitor(self, other: Self) -> Self {
        // self takes priority
        let mask = self.mask | other.mask;
        // Convert the bit-masks to real masks
        let (self_mask, other_mask) = (Mask::from(self.mask), Mask::from(other.mask));
        let state = State {
            flags: if self_mask.flags {
                self.state.flags
            } else if other_mask.flags {
                other.state.flags
            } else {
                FlagsBitField::default()
            },
            registers: Register::ALL.map(|r| {
                if self_mask[r] {
                    self.state.get_register(r)
                } else if other_mask[r] {
                    other.state.get_register(r)
                } else {
                    W::default()
                }
            }),
        };
        Masked { state, mask }
    }
}

impl<W: Clone + Display> Display for Masked<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mask = self.mask().into_mask();
        let state = self.state().clone();
        let mut parts = vec![];
        if mask.flags {
            parts.push(state.flags.to_string());
        }
        for r in Register::all().filter(|r| mask[*r]) {
            parts.push(format!("{r}={}", state[r]));
        }
        write!(f, "{}", parts.join(" "))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::property_test;

    #[property_test]
    fn test_singleton_sub_masks_length(mask: BitMask) {
        let mask = mask.into_mask();
        let count = (if mask.flags { 1 } else { 0 }) + mask.registers().count();
        prop_assert_eq!(mask.singleton_sub_masks().count(), count);
    }

    #[property_test]
    fn test_sub_masks_length(mask: BitMask) {
        let mask = mask.into_mask();
        let count = (if mask.flags { 1 } else { 0 }) + mask.registers().count();
        prop_assert_eq!(mask.sub_masks().count(), 2usize.pow(count as _));
    }

    #[property_test]
    fn sub_masks_acsending_order(mask: BitMask) {
        let sub_masks = mask.into_mask().sub_masks();
        let lengths = sub_masks.map(|m| m.len());
        prop_assert!(lengths.is_sorted() /* Ascending order */);
    }

    #[property_test]
    fn mask_or_spec(a: Mask, b: Mask) {
        println!("a {a}  b {b}");
        let c = a | b;
        println!("c {c}");
        prop_assert_eq!(c.flags, a.flags || b.flags);
        for r in Register::ALL {
            prop_assert_eq!(c[r], a[r] || b[r]);
        }
        let d = (a.into_bit_mask() | b.into_bit_mask()).into_mask();
        prop_assert_eq!(c, d);
    }

    #[property_test]
    fn mask_to_bit_mask(a: Mask) {
        prop_assert_eq!(a, a.into_bit_mask().into_mask());
    }

    #[property_test]
    fn bit_mask_to_mask(a: BitMask) {
        prop_assert_eq!(a, a.into_mask().into_bit_mask());
    }
}
