use crate::bit_vec::{BitVec as BitVecTrait, SmtBitVec as SmtBitVecTrait};
use crate::collect_registers;
use crate::isa::{self, Flags, Register};
use crate::reduce_bit_width::Reducer;
use crate::smtlib_utils::GetStorage;
use crate::some_traits::{Bool as BoolTrait, BoolEq, CloneTo};

use smtlib::prelude::*;
use smtlib::terms::Const;
use smtlib::{Storage, Bool};

// ============================================================================
//                                 State trait
// ============================================================================

type StateBool<S: State> = <S::BitVec as BitVecTrait>::Bool;

pub trait State: AsRef<<Self::BitVec as BitVecTrait>::FromContext> {
    type BitVec: BitVecTrait;

    // ── Register & flag access ────────────────────────────────────────────

    fn get_register(&self, reg: Register) -> Self::BitVec;
    fn set_register(&mut self, reg: Register, val: Self::BitVec);

    /// Returns the current `(Z, N, C, V)` flags.
    ///
    /// Concrete states return `(bool, bool, bool, bool)`.  Symbolic states return four
    /// independent SMT boolean terms.  Uninitialized flags are treated as all-`false`.
    fn get_flags_raw(&self) -> (StateBool<Self>, StateBool<Self>, StateBool<Self>, StateBool<Self>);
    /// Overwrites all four `(Z, N, C, V)` flags at once.
    fn set_flags_raw(&mut self, flags: (StateBool<Self>, StateBool<Self>, StateBool<Self>, StateBool<Self>));

    // ── Lifting concrete values ───────────────────────────────────────────

    /// Returns a `StateBool<Self>` that is true iff the ARM condition code `cc` holds given
    /// the current flag state.  Implemented using the flag accessors and operator bounds
    /// above, so this default should rarely (if ever) need to be overridden.
    fn cond_holds(&self, cc: isa::CondCode) -> StateBool<Self> {
        use isa::CondCode::*;
        let (z, n, c, v) = self.get_flags_raw();
        match cc {
            Al => StateBool::<Self>::r#true(),
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
            Ge => n.eq(&v),
            Lt => n ^ v,
            Gt => !z & n.eq(&v),
            Le => z | (n ^ v),
        }
    }
}

// ============================================================================
//                                SMT state types
// ============================================================================

#[derive(Clone, Copy, Debug)]
pub struct StateVars<'st, B> {
    pub registers: [Const<'st, B>; Register::COUNT as usize],
    pub flags: FlagVars<'st>,
}

impl<'st, B: StaticSorted<'st>> StateVars<'st, B> {
    pub fn new(st: &'st Storage, name: &str) -> Self {
        Self {
            registers: std::array::from_fn(|i| {
                B::new_const(st, &format!("{}_r{}", name, i))
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
pub struct SmtState<'st, W> {
    pub registers: [W; Register::COUNT as usize],
    pub flags: SymbolicFlags<'st>,
}

#[derive(Clone, Copy, Debug)]
pub struct SymbolicFlags<'st> {
    pub z: Bool<'st>,
    pub n: Bool<'st>,
    pub c: Bool<'st>,
    pub v: Bool<'st>,
}

impl<'st, W: SmtBitVecTrait<'st>> From<StateVars<'st, W>> for SmtState<'st, W> {
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

impl<'st, W> SmtState<'st, W> {
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

impl<'st, W: SmtBitVecTrait<'st>> State for SmtState<'st, W> {
    type BitVec = W;

    fn get_register(&self, reg: Register) -> Self::BitVec {
        self.registers[reg.0 as usize]
    }
    fn set_register(&mut self, reg: Register, val: Self::BitVec) {
        self.registers[reg.0 as usize] = val;
    }
    fn get_flags_raw(&self) -> (StateBool<Self>, StateBool<Self>, StateBool<Self>, StateBool<Self>) {
        (self.flags.z, self.flags.n, self.flags.c, self.flags.v)
    }
    fn set_flags_raw(&mut self, (z, n, c, v): (StateBool<Self>, StateBool<Self>, StateBool<Self>, StateBool<Self>)) {
        self.flags = SymbolicFlags { z, n, c, v };
    }
}

impl<'st, W: GetStorage<'st>> AsRef<smtlib::Storage> for SmtState<'st, W> {
    fn as_ref(&self) -> &smtlib::Storage {
        self.registers[0].st()
    }
}

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
pub struct SearchState<W> {
    /// This vector is always sorted by register.
    /// Registers that are not present are not "live".
    pub registers: Vec<(Register, W)>,
    /// The value of the flags register. If None, flags is not "live".
    pub flags: Option<Flags>,
}

impl<W> SearchState<W> {
    /// Copies this state to another state object. Used to avoid clones, that in a loop, can
    /// allocate more.
    #[inline]
    pub(crate) fn clone_to(&self, other: &mut Self) {
        other.registers.clear();
        other.registers.extend(&self.registers);
        other.flags = self.flags;
    }

    pub(crate) fn reduce<WSmall>(&self, reducer: &mut Reducer<W, WSmall>) -> SearchState<WSmall> {
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

impl<W> AsRef<()> for SearchState<W> {
    fn as_ref(&self) -> &() {
        &()
    }
}

impl<W: BitVecTrait<Bool=bool>> State for SearchState<W> {
    type BitVec = W;

    fn get_register(&self, reg: Register) -> W {
        for (r, v) in &self.registers {
            if *r == reg {
                return *v;
            }
        }
        panic!("Register {reg:?} not found in state.");
    }

    fn set_register(&mut self, reg: Register, value: W) {
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

}

impl<W: Clone> collect_registers::State<W> for SearchState<W> {
    fn registers(&self) -> impl Iterator<Item = (Register, W)> {
        self.registers.iter().cloned()
    }
}

impl<W> CloneTo for SearchState<W> {
    fn clone_to(&self, output: &mut Self) {
        self.clone_to(output);
    }
}
