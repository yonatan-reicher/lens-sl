//! State types and state-related logic for the ARM-like ISA.

use super::Register;
use crate::all::All;
use crate::all_permutations::Iter as PermutationIter;
use crate::bool::prelude::*;
use crate::collect_registers;
use crate::enumerate::EnumerationInfoOptions;
use crate::oracle;
use crate::reduce_bit_width::Reducer;
use crate::word::prelude::*;
use std::fmt::{self, Display, Formatter};
// derive macros
use derive_more::{Debug, Display};
use serde::{Deserialize, Serialize};
// smt
use smtlib::Storage;
use smtlib::prelude::*;
use smtlib::terms::Const;

// ============================= State =============================

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    // TODO: Live mask
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
#[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, Hash)]
#[debug("{:?}", Mask::from(*self))]
#[display("{}", Mask::from(*self))]
pub struct BitMask(u32);

// =========================== Masked State ===========================

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct Masked<W> {
    state: State<W>,
    mask: BitMask,
}

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

    pub fn all_each(ei: &EnumerationInfoOptions<Register>, mut f: impl FnMut(&Self))
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
                f(&state)
            }
        }
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
    pub fn mask(self, mask: Mask) -> Self {
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

impl<'st> Flags<SmtBool<'st>> {
    pub(crate) fn update_from_add<W: SmtWord<'st>>(
        &mut self,
        op1: W,
        op2: W,
        enabled: SmtBool<'st>,
    ) {
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

    pub(crate) fn update_from_sub<W: SmtWord<'st>>(
        &mut self,
        op1: W,
        op2: W,
        enabled: SmtBool<'st>,
    ) {
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
    pub(crate) fn update_from_add<W: Word>(&mut self, op1: W, op2: W, enabled: bool) {
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

    pub(crate) fn update_from_sub<W: Word>(&mut self, op1: W, op2: W, enabled: bool) {
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
            flags: self.flags | other.flags,
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
            state: self.state.mask(mask),
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
