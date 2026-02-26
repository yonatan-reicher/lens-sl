//! Machine-state representation for instruction execution.
//!
//! This module defines:
//!
//! * [State] — the unified execution trait, generic over a [`Word`] size and
//!   parameterised by associated types [`State::Num`] and [`State::Bool`] that
//!   differ between concrete and symbolic execution.
//! * [`ConcreteState`] / [`SymbolicState`] — marker sub-traits.
//! * [`SmtState`], [`SymbolicFlags`], [`StateVars`], [`FlagVars`] — the symbolic
//!   (SMT) state types used by the SMT oracle.
//! * [`SearchState`] — the concrete state used during bidirectional synthesis
//!   search.

use crate::collect_registers;
use crate::isa::{self, Flags, Register};
use crate::oracle;
use crate::reduce_bit_width::Reducer;
use crate::word::{SymbolicOps, prelude::*};

use std::ops::{BitAnd, BitOr, BitXor, Not};

use arbitrary_int::traits::Integer;
use smtlib::prelude::*;
use smtlib::terms::Const;
use smtlib::{Bool, Storage};

// ============================================================================
//                                 State trait
// ============================================================================

/// Unified execution trait for both concrete and symbolic machine states.
///
/// Generic over a [`Word`] size `W`. Two associated types capture what differs
/// between execution modes:
///
/// * **Concrete mode** (`W::Unsigned` / `bool`): used by the synthesis search
///   and the test-cases oracle.  Operations are plain Rust arithmetic and logic.
/// * **Symbolic mode** (`W::SymbolicBitVec<'st>` / `smtlib::Bool<'st>`): used
///   by the SMT oracle.  Operations lower to SMT bit-vector / boolean terms.
///
/// See [`ConcreteState`] and [`SymbolicState`] for the corresponding marker
/// sub-traits.
pub trait State<W: Word> {
    /// Numeric value type.  `W::Unsigned` in concrete mode; `W::SymbolicBitVec<'st>` in
    /// symbolic mode.  Bitwise `&`, `|`, `^` are available via the operator bounds.
    type Num: Clone
        + Copy
        + BitAnd<Output = Self::Num>
        + BitOr<Output = Self::Num>
        + BitXor<Output = Self::Num>;
    /// Boolean type.  `bool` in concrete mode; `smtlib::Bool<'st>` in symbolic mode.
    /// Logical `&`, `|`, `^`, `!` are available via the operator bounds.
    type Bool: Clone
        + Copy
        + BitAnd<Output = Self::Bool>
        + BitOr<Output = Self::Bool>
        + BitXor<Output = Self::Bool>
        + Not<Output = Self::Bool>;

    // ── Register & flag access ────────────────────────────────────────────

    fn get_register(&self, reg: Register) -> Self::Num;
    fn set_register(&mut self, reg: Register, val: Self::Num);

    /// Returns the current `(Z, N, C, V)` flags.
    ///
    /// Concrete states return `(bool, bool, bool, bool)`.  Symbolic states return four
    /// independent SMT boolean terms.  Uninitialized flags are treated as all-`false`.
    fn get_flags_raw(&self) -> (Self::Bool, Self::Bool, Self::Bool, Self::Bool);
    /// Overwrites all four `(Z, N, C, V)` flags at once.
    fn set_flags_raw(&mut self, flags: (Self::Bool, Self::Bool, Self::Bool, Self::Bool));

    // ── Lifting concrete values ───────────────────────────────────────────

    /// Creates a boolean constant.  Needs `&self` in symbolic mode to access the
    /// underlying SMT storage arena.
    fn bool_lit(&self, b: bool) -> Self::Bool;
    /// Lifts a concrete immediate (`W::Unsigned`) to `Self::Num`.  Identity in concrete
    /// mode; creates a bit-vector constant in symbolic mode.
    fn from_word(&self, val: W::Unsigned) -> Self::Num;

    // ── Arithmetic operations ─────────────────────────────────────────────
    //
    // These are required methods rather than `std::ops` operator bounds because:
    //   • `Add`/`Sub` panic on overflow for concrete unsigned integers in debug mode,
    //     but we need wrapping semantics throughout.
    //   • `Sub` is not implemented for SMT bit-vectors; subtraction is `a + (-b)`.

    /// Wrapping addition.
    fn num_add(a: Self::Num, b: Self::Num) -> Self::Num;
    /// Wrapping subtraction.
    fn num_sub(a: Self::Num, b: Self::Num) -> Self::Num;
    /// Wrapping multiplication (result truncated to the word width).
    fn num_mul(a: Self::Num, b: Self::Num) -> Self::Num;

    // ── Predicates → Bool ────────────────────────────────────────────────

    /// True when the result is zero.
    fn is_zero(a: Self::Num) -> Self::Bool;
    /// True when the MSB is set (value is negative in two's-complement).
    fn is_negative(a: Self::Num) -> Self::Bool;
    /// True when unsigned addition `a + b` overflows (carry out).
    fn add_carry(a: Self::Num, b: Self::Num) -> Self::Bool;
    /// ARM carry flag for subtraction: `true` when `a >= b` unsigned (no borrow).
    fn sub_carry(a: Self::Num, b: Self::Num) -> Self::Bool;
    /// True when signed addition `a + b` overflows.
    fn add_signed_overflow(a: Self::Num, b: Self::Num) -> Self::Bool;
    /// True when signed subtraction `a - b` overflows.
    fn sub_signed_overflow(a: Self::Num, b: Self::Num) -> Self::Bool;

    // ── Boolean equality ─────────────────────────────────────────────────
    //
    // This is a required method (not `PartialEq`) because the result must be
    // `Self::Bool` — an SMT term in symbolic mode — rather than a plain `bool`.

    /// Returns a `Self::Bool` that is true iff `a == b`.
    fn bool_eq(a: Self::Bool, b: Self::Bool) -> Self::Bool;

    // ── Conditional selection (if-then-else) ─────────────────────────────

    /// Returns `t` when `cond` is true, `e` otherwise.  In symbolic mode this lowers
    /// to an SMT `ite` term; in concrete mode it is a plain `if`/`else`.
    fn select_num(cond: Self::Bool, t: Self::Num, e: Self::Num) -> Self::Num;
    /// Same as `select_num` but for boolean values.
    fn select_bool(cond: Self::Bool, t: Self::Bool, e: Self::Bool) -> Self::Bool;

    // ── Condition-code evaluation ─────────────────────────────────────────

    /// Returns a `Self::Bool` that is true iff the ARM condition code `cc` holds given
    /// the current flag state.  Implemented using the flag accessors and operator bounds
    /// above, so this default should rarely (if ever) need to be overridden.
    fn cond_holds(&self, cc: isa::CondCode) -> Self::Bool {
        use isa::CondCode::*;
        let (z, n, c, v) = self.get_flags_raw();
        match cc {
            Al => self.bool_lit(true),
            Eq => z,
            Ne => !z,
            Cs => c,
            Cc => !c,
            Mi => n,
            Pl => !n,
            Vs => v,
            Vc => !v,
            Hi => c & !z,
            Ls => !c | z,
            Ge => Self::bool_eq(n, v),
            Lt => n ^ v,
            Gt => !z & Self::bool_eq(n, v),
            Le => z | (n ^ v),
        }
    }
}

/// Marker sub-trait for concrete (non-symbolic) machine states.
#[allow(dead_code)]
pub trait ConcreteState<W: Word>: State<W> {}

/// Marker sub-trait for symbolic (SMT) machine states.
#[allow(dead_code)]
pub trait SymbolicState<W: Word>: State<W> {}

// ============================================================================
//                                SMT state types
// ============================================================================

#[derive(Clone, Copy, Debug)]
pub struct StateVars<'st, W: Word> {
    pub registers: [Const<'st, W::SymbolicBitVec<'st>>; Register::COUNT as usize],
    pub flags: FlagVars<'st>,
}

impl<'st, W: Word> StateVars<'st, W> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            registers: std::array::from_fn(|i| {
                W::SymbolicBitVec::new_const(st, &format!("{}_r{}", name, i))
            }),
            flags: FlagVars::new(st, name),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FlagVars<'st> {
    pub z: Const<'st, Bool<'st>>,
    pub n: Const<'st, Bool<'st>>,
    pub c: Const<'st, Bool<'st>>,
    pub v: Const<'st, Bool<'st>>,
}

impl<'st> FlagVars<'st> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            z: Bool::new_const(st, &format!("{}_z", name)),
            n: Bool::new_const(st, &format!("{}_n", name)),
            c: Bool::new_const(st, &format!("{}_c", name)),
            v: Bool::new_const(st, &format!("{}_v", name)),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct SmtState<'st, W: Word> {
    pub registers: [W::SymbolicBitVec<'st>; Register::COUNT as usize],
    pub flags: SymbolicFlags<'st>,
}

#[derive(Clone, Copy, Debug)]
pub struct SymbolicFlags<'st> {
    pub z: Bool<'st>,
    pub n: Bool<'st>,
    pub c: Bool<'st>,
    pub v: Bool<'st>,
}

impl<'st, W: Word> From<StateVars<'st, W>> for SmtState<'st, W> {
    fn from(value: StateVars<'st, W>) -> Self {
        Self {
            registers: value.registers.map(Into::into),
            flags: value.flags.into(),
        }
    }
}

impl<'st> From<FlagVars<'st>> for SymbolicFlags<'st> {
    fn from(value: FlagVars<'st>) -> Self {
        Self {
            z: value.z.into(),
            n: value.n.into(),
            c: value.c.into(),
            v: value.v.into(),
        }
    }
}

impl<'st, W: Word> SmtState<'st, W> {
    pub fn eq(&self, other: Self) -> Bool<'st> {
        let regs = self.registers.iter().zip(other.registers);
        let regs_eq = regs
            .map(|(ra, rb)| ra._eq(rb))
            .reduce(|b1, b2| b1 & b2)
            .unwrap();
        regs_eq & self.flags.eq(other.flags)
    }
}

impl<'st> SymbolicFlags<'st> {
    pub fn eq(&self, other: Self) -> Bool<'st> {
        self.z._eq(other.z) & self.n._eq(other.n) & self.c._eq(other.c) & self.v._eq(other.v)
    }
}

impl<'st, W: Word> State<W> for SmtState<'st, W> {
    type Num = W::SymbolicBitVec<'st>;
    type Bool = Bool<'st>;

    fn get_register(&self, reg: Register) -> Self::Num {
        self.registers[reg.0 as usize]
    }
    fn set_register(&mut self, reg: Register, val: Self::Num) {
        self.registers[reg.0 as usize] = val;
    }
    fn get_flags_raw(&self) -> (Self::Bool, Self::Bool, Self::Bool, Self::Bool) {
        (self.flags.z, self.flags.n, self.flags.c, self.flags.v)
    }
    fn set_flags_raw(&mut self, (z, n, c, v): (Self::Bool, Self::Bool, Self::Bool, Self::Bool)) {
        self.flags = SymbolicFlags { z, n, c, v };
    }
    fn bool_lit(&self, b: bool) -> Self::Bool {
        Bool::new(self.flags.z.st(), b)
    }
    fn from_word(&self, val: W::Unsigned) -> Self::Num {
        W::new_bit_vec(self.flags.z.st(), val)
    }
    fn num_add(a: Self::Num, b: Self::Num) -> Self::Num {
        a + b
    }
    fn num_sub(a: Self::Num, b: Self::Num) -> Self::Num {
        a + (-b)
    }
    fn num_mul(a: Self::Num, b: Self::Num) -> Self::Num {
        a * b
    }

    fn is_zero(a: Self::Num) -> Self::Bool {
        let zero = W::new_bit_vec(a.st(), 0.as_());
        a._eq(zero)
    }
    fn is_negative(a: Self::Num) -> Self::Bool {
        let zero = W::new_bit_vec(a.st(), 0.as_());
        a.bvslt(zero)
    }
    fn add_carry(a: Self::Num, b: Self::Num) -> Self::Bool {
        (a + b).bvult(a)
    }
    fn sub_carry(a: Self::Num, b: Self::Num) -> Self::Bool {
        a.bvuge(b)
    }
    fn add_signed_overflow(a: Self::Num, b: Self::Num) -> Self::Bool {
        let zero = W::new_bit_vec(a.st(), 0.as_());
        let sum = a + b;
        let a_neg = a.bvslt(zero);
        let b_neg = b.bvslt(zero);
        let sum_neg = sum.bvslt(zero);
        (!a_neg & !b_neg & sum_neg) | (a_neg & b_neg & !sum_neg)
    }
    fn sub_signed_overflow(a: Self::Num, b: Self::Num) -> Self::Bool {
        let zero = W::new_bit_vec(a.st(), 0.as_());
        let diff = a + (-b);
        let a_neg = a.bvslt(zero);
        let b_neg = b.bvslt(zero);
        let diff_neg = diff.bvslt(zero);
        (a_neg & !b_neg & !diff_neg) | (!a_neg & b_neg & diff_neg)
    }
    fn bool_eq(a: Self::Bool, b: Self::Bool) -> Self::Bool {
        !(a ^ b)
    }
    fn select_num(cond: Self::Bool, t: Self::Num, e: Self::Num) -> Self::Num {
        <W::SymbolicBitVec<'st> as SymbolicOps<'st>>::select(cond, t, e)
    }
    fn select_bool(cond: Self::Bool, t: Self::Bool, e: Self::Bool) -> Self::Bool {
        cond.ite(t, e)
    }
}

impl<'st, W: Word> SymbolicState<W> for SmtState<'st, W> {}

// ============================================================================
// SearchState — concrete state used during bidirectional search
// ============================================================================

/// The concrete machine state used during synthesis search.
///
/// Holds the live register values and the flags register.  Registers that are
/// not yet "live" (i.e. not yet written by any enumerated instruction) are
/// simply absent from the [`registers`] vector.
#[derive(Clone, Debug, Default, derive_more::Display, PartialEq, Eq, Hash)]
#[display(
    "Registers: {{{}}}, Flags: {}",
    registers
        .iter()
        .map(|(r, v)| format!("{r:?}: {v}"))
        .collect::<Vec<_>>()
        .join(", "),
    match &flags {
        Some(f) => format!("{f:?}"),
        None => "None".to_string(),
    }
)]
pub struct SearchState<W: Word> {
    /// This vector is always sorted by register.
    /// Registers that are not present are not "live".
    pub registers: Vec<(Register, W::Unsigned)>,
    /// The value of the flags register. If None, flags is not "live".
    pub flags: Option<Flags>,
}

impl<W: Word> SearchState<W> {
    /// Copies this state to another state object. Used to avoid clones, that in a loop, can
    /// allocate more.
    #[inline]
    pub(crate) fn clone_to(&self, other: &mut Self) {
        other.registers.clear();
        other.registers.extend(&self.registers);
        other.flags = self.flags;
    }

    pub(crate) fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> SearchState<WSmall> {
        SearchState {
            registers: self
                .registers
                .iter()
                .map(|(r, v)| (*r, reducer.reduce(*v, &Default::default())))
                .collect(),
            flags: self.flags,
        }
    }
}

impl<W: Word> State<W> for SearchState<W> {
    type Num = W::Unsigned;
    type Bool = bool;

    fn get_register(&self, reg: Register) -> W::Unsigned {
        for (r, v) in &self.registers {
            if *r == reg {
                return *v;
            }
        }
        panic!("Register {reg:?} not found in state.");
    }

    fn set_register(&mut self, reg: Register, value: W::Unsigned) {
        for (r, v) in &mut self.registers {
            if *r == reg {
                *v = value;
                return;
            }
        }
        self.registers.push((reg, value));
        self.registers.sort_by_key(|(r, _)| *r);
    }

    fn get_flags_raw(&self) -> (bool, bool, bool, bool) {
        let f = self.flags.unwrap_or_default();
        (
            f.contains(Flags::Z),
            f.contains(Flags::N),
            f.contains(Flags::C),
            f.contains(Flags::V),
        )
    }

    fn set_flags_raw(&mut self, (z, n, c, v): (bool, bool, bool, bool)) {
        self.flags = Some(Flags::new(z, n, c, v));
    }

    fn bool_lit(&self, b: bool) -> bool { b }
    fn from_word(&self, val: W::Unsigned) -> W::Unsigned { val }

    fn num_add(a: W::Unsigned, b: W::Unsigned) -> W::Unsigned { a.overflowing_add(b).0 }
    fn num_sub(a: W::Unsigned, b: W::Unsigned) -> W::Unsigned { a.overflowing_sub(b).0 }
    fn num_mul(a: W::Unsigned, b: W::Unsigned) -> W::Unsigned {
        let as_: W::Signed = a.as_();
        let bs_: W::Signed = b.as_();
        as_.overflowing_mul(bs_).0.as_()
    }

    fn is_zero(a: W::Unsigned) -> bool { a.is_zero() }
    fn is_negative(a: W::Unsigned) -> bool {
        let s: W::Signed = a.as_();
        s < 0.as_()
    }
    fn add_carry(a: W::Unsigned, b: W::Unsigned) -> bool { a.overflowing_add(b).1 }
    fn sub_carry(a: W::Unsigned, b: W::Unsigned) -> bool { !a.overflowing_sub(b).1 }
    fn add_signed_overflow(a: W::Unsigned, b: W::Unsigned) -> bool {
        let as_: W::Signed = a.as_();
        let bs_: W::Signed = b.as_();
        as_.overflowing_add(bs_).1
    }
    fn sub_signed_overflow(a: W::Unsigned, b: W::Unsigned) -> bool {
        let as_: W::Signed = a.as_();
        let bs_: W::Signed = b.as_();
        as_.overflowing_sub(bs_).1
    }

    fn bool_eq(a: bool, b: bool) -> bool { a == b }
    fn select_num(cond: bool, t: W::Unsigned, e: W::Unsigned) -> W::Unsigned {
        if cond { t } else { e }
    }
    fn select_bool(cond: bool, t: bool, e: bool) -> bool {
        if cond { t } else { e }
    }
}

impl<W: Word> ConcreteState<W> for SearchState<W> {}

impl<W: Word> collect_registers::State<W> for SearchState<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W::Unsigned)> {
        self.registers.iter().cloned()
    }
}

impl<W: Word> oracle::test_cases::State for SearchState<W> {
    fn clone_to(&self, output: &mut Self) {
        self.clone_to(output);
    }
}
