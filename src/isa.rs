use crate::all_permutations::Iter as PermutationIter;
use crate::bit_vec::{BitVec, ConcreteeeBitVec};
use crate::iter_slice_or_single::Iter as SliceOrSingle;
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::some_traits::{IfThenElse, Run, Bool};
use crate::state::State;

use std::ops::ControlFlow;
use std::fmt::{self, Display, Formatter};

use derive_more::{Debug, Display};

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
#[derive(Clone, Copy, Debug, Display, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
#[debug("r{_0}")]
#[display("r{_0}")]
pub struct Register(pub u8);

impl Register {
    /// TODO: Does Lens only use the regular 16 registers?
    pub const COUNT: u8 = 16;
}

/// A single instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
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

impl Flags {
    #[rustfmt::skip]
    pub fn new(z: bool, n: bool, c: bool, v: bool) -> Self {
        let mut flags = Flags::empty();
        if z { flags |= Flags::Z; }
        if n { flags |= Flags::N; }
        if c { flags |= Flags::C; }
        if v { flags |= Flags::V; }
        flags
    }
}

#[cfg(test)]
impl proptest::arbitrary::Arbitrary for Flags {
    type Parameters = ();
    type Strategy = proptest::strategy::BoxedStrategy<Self>;

    #[rustfmt::skip]
    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use proptest::prelude::*;
        (any::<bool>(), any::<bool>(), any::<bool>(), any::<bool>())
            .prop_map(|(z, n, c, v)| {
                let mut flags = Flags::empty();
                if z { flags |= Flags::Z; }
                if n { flags |= Flags::N; }
                if c { flags |= Flags::C; }
                if v { flags |= Flags::V; }
                flags
            })
            .boxed()
    }
}

impl<W: ConcreteeeBitVec, S: State> Run<S> for Inst<W> {
    fn run(&self, state: &mut S) {
        use OpCode::*;

        let cond = state.cond_holds(self.cond_code);
        let st = *state.as_ref();

        macro_rules! r {
            ($i:literal) => {{
                debug_assert!($i < 3);
                debug_assert!(self.op_code.arg_types()[$i] == ArgType::Reg);
                let r = Register(Into::<u64>::into(self.args[$i]) as u8);
                state.get_register(r)
            }};
        }
        macro_rules! imm {
            ($i:literal) => {
                S::BitVec::from_i64(self.args[$i].into(), st)
            };
        }
        macro_rules! dst {
            ($i:literal) => {
                Register(Into::<u64>::into(self.args[$i]) as u8)
            };
        }

        // TODO
        match self.op_code {
            Nop => return,
            Add | AddI | Sub | SubI => {
                let left = r![1];
                let right = match self.op_code {
                    Add | Sub => r![2],
                    _ => imm![2],
                };
                let (new_val, new_c, new_v) = match self.op_code {
                    Add | AddI => (left + right, (left + right).unsigned_lt(left), {
                        let zero = S::BitVec::from_i64(0, st);
                        let sum = left + right;
                        let a_neg = left.signed_lt(zero);
                        let b_neg = right.signed_lt(zero);
                        let sum_neg = sum.signed_lt(zero);
                        (!a_neg & !b_neg & sum_neg) | (a_neg & b_neg & !sum_neg)
                    }),
                    _ => (left.sub(right), right.unsigned_le(left), {
                        let zero = S::BitVec::from_i64(0, st);
                        let diff = left + (-right);
                        let a_neg = left.signed_lt(zero);
                        let b_neg = right.signed_lt(zero);
                        let diff_neg = diff.signed_lt(zero);
                        (a_neg & !b_neg & !diff_neg) | (!a_neg & b_neg & diff_neg)
                    }),
                };
                let new_z = S::BitVec::is_zero(new_val);
                let new_n = S::BitVec::is_negative(new_val);

                let old = state.get_register(dst![0]);
                state.set_register(dst![0], cond.if_then_else(new_val, old));

                let (old_z, old_n, old_c, old_v) = state.get_flags_raw();
                state.set_flags_raw((
                    cond.if_then_else(new_z, old_z),
                    cond.if_then_else(new_z, old_z),
                    cond.if_then_else(new_n, old_n),
                    cond.if_then_else(new_c, old_c),
                ));
            }
            And | Eor | Orr | Mul | Mov | MovI => {
                let new_val = match self.op_code {
                    And => r![1] & r![2],
                    Eor => r![1] ^ r![2],
                    Orr => r![1] | r![2],
                    Mul => r![1] * r![2],
                    Mov => r![1],
                    MovI => imm![1],
                    _ => unreachable!(),
                };
                let new_z = new_val.is_zero();
                let new_n = new_val.is_negative();
                let new_c = Bool::r#false();
                let new_v = Bool::r#false();

                let old = state.get_register(dst![0]);
                state.set_register(dst![0], cond.if_then_else(new_val, old));

                let (old_z, old_n, old_c, old_v) = state.get_flags_raw();
                state.set_flags_raw((
                    cond.if_then_else(new_z, old_z),
                    cond.if_then_else(new_n, old_n),
                    cond.if_then_else(new_c, old_c),
                    cond.if_then_else(new_v, old_v),
                ));
            }
        }
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
            write!(f, "{op_code} {}, {}, {}", args[0], args[1], args[2])?;
        } else {
            write!(f, "{op_code}{cond_code} {}, {}, {}", args[0], args[1], args[2])?;
        }
        Ok(())
    }
}

impl<W: ConcreteeeBitVec> Inst<W> {
    pub fn reduce<WSmall: ConcreteeeBitVec>(&self, reducer: &mut Reducer<W, WSmall>) -> Inst<WSmall> {
        fn reduce_arg<W: ConcreteeeBitVec, WSmall: ConcreteeeBitVec>(
            reducer: &mut Reducer<W, WSmall>,
            arg: W,
            arg_type: ArgType,
            info: &ImmediateInfo,
        ) -> WSmall {
            match arg_type {
                ArgType::Imm => reducer.reduce(arg, info),
                ArgType::Reg | ArgType::Unused => arg.into_concrete_bit_vec(),
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

    pub fn extend<WBig: ConcreteeeBitVec>(
        &self,
        reducer: &Reducer<WBig, W>,
    ) -> impl Iterator<Item = Inst<WBig>> + Clone {
        fn extend_arg<WSmall: ConcreteeeBitVec, WBig: ConcreteeeBitVec>(
            reducer: &Reducer<WBig, WSmall>,
            arg: WSmall,
            arg_type: ArgType,
        ) -> SliceOrSingle<'_, WBig> {
            match arg_type {
                ArgType::Imm => reducer.extend(arg),
                ArgType::Reg | ArgType::Unused => SliceOrSingle::Single(arg.into_concrete_bit_vec()),
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
                    *args_iter.get(0).unwrap_or(&Default::default()).into(),
                    *args_iter.get(1).unwrap_or(&Default::default()).into(),
                    *args_iter.get(2).unwrap_or(&Default::default()).into(),
                ],
            }
        }
    };
    ( $op_code:ident, $( $arg:expr ),* $(,)? ) => {
        inst!($op_code Al, $( $arg ),* )
    };
}

pub fn extend_program_for_each<F, T, WBig: ConcreteeeBitVec, WSmall: ConcreteeeBitVec>(
    program: &[Inst<WSmall>],
    reducer: &Reducer<WBig, WSmall>,
    mut f: F,
) -> ControlFlow<T>
where
    F: FnMut(&[Inst<WBig>]) -> ControlFlow<T>,
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
    use crate::{bit_vec::ConcreteBitVec, state::SearchState};
    use std::collections::HashSet;

    // ── Helpers for execution tests ───────────────────────────────────────

    type S = SearchState<ConcreteBitVec<64>>;

    fn state(regs: &[(u8, u64)], z: bool, n: bool, c: bool, v: bool) -> S {
        S {
            registers: regs.iter().map(|&(r, v)| (Register(r), v.into())).collect(),
            flags: Some(Flags::new(z, n, c, v)),
        }
    }

    fn r(s: &S, reg: u8) -> <S as State>::BitVec {
        s.get_register(Register(reg))
    }

    /// Returns `(Z, N, C, V)`.
    fn f(s: &S) -> (bool, bool, bool, bool) {
        s.get_flags_raw()
    }

    // ── Flag tests ────────────────────────────────────────────────────────

    /// N flag should be set when the result is negative (MSB = 1).
    #[test]
    fn n_flag_set_on_negative_result() {
        // 0 - 1 = u64::MAX, which is negative as i64.
        let mut s = state(&[(0, 0), (1, 1)], false, false, false, false);
        inst!(Sub, 0, 0, 1).run(&mut s); // r0 = r0 - r1 = 0 - 1
        let (z, n, _c, _v) = f(&s);
        assert!(!z, "Z should be clear (result is non-zero)");
        assert!(n, "N should be set (result is negative)");
    }

    /// N flag should be clear when the result is positive.
    #[test]
    fn n_flag_clear_on_positive_result() {
        let mut s = state(&[(0, 5), (1, 3)], false, false, false, false);
        inst!(Sub, 0, 0, 1).run(&mut s); // r0 = 5 - 3 = 2
        let (_z, n, _c, _v) = f(&s);
        assert!(!n, "N should be clear (result is positive)");
    }

    /// Z flag should be set on a zero result.
    #[test]
    fn z_flag_set_on_zero_result() {
        let mut s = state(&[(0, 7), (1, 7)], false, false, false, false);
        inst!(Sub, 0, 0, 1).run(&mut s); // r0 = 7 - 7 = 0
        assert!(f(&s).0, "Z should be set");
    }

    /// C (carry) flag set on unsigned addition overflow.
    #[test]
    fn carry_flag_on_unsigned_add_overflow() {
        let mut s = state(&[(0, u64::MAX), (1, 1)], false, false, false, false);
        inst!(Add, 0, 0, 1).run(&mut s); // u64::MAX + 1 wraps to 0
        let (z, _n, c, v) = f(&s);
        assert!(z, "Z should be set (result is 0)");
        assert!(c, "C should be set (unsigned overflow)");
        assert!(
            !v,
            "V should be clear (signed: -1 + 1 = 0, no signed overflow)"
        );
    }

    /// V (overflow) flag set on signed addition overflow.
    #[test]
    fn overflow_flag_on_signed_add_overflow() {
        // i64::MAX + 1 overflows signed, but does NOT overflow unsigned.
        let mut s = state(&[(0, i64::MAX as u64), (1, 1)], false, false, false, false);
        inst!(Add, 0, 0, 1).run(&mut s);
        let (_z, _n, c, v) = f(&s);
        assert!(!c, "C should be clear (no unsigned overflow)");
        assert!(v, "V should be set (signed overflow)");
    }

    /// Logical instruction (And) sets Z and N, and clears C and V.
    #[test]
    fn logical_instruction_sets_z_n_clears_c_v() {
        // Start with C=true, V=true to verify they get cleared.
        let mut s = state(&[(0, 5), (1, 0)], false, false, true, true);
        inst!(And, 0, 0, 1).run(&mut s); // r0 = 5 & 0 = 0
        let (z, n, c, v) = f(&s);
        assert!(z, "Z should be set (result is 0)");
        assert!(!n, "N should be clear");
        assert!(!c, "C should be cleared by logical instruction");
        assert!(!v, "V should be cleared by logical instruction");
    }

    // ── Condition-code tests ──────────────────────────────────────────────

    /// When the condition is not met, the instruction is a no-op.
    #[test]
    fn condition_code_skips_when_not_met() {
        // Eq requires Z=1; Z=0 here, so the instruction should not execute.
        let mut s = state(&[(0, 5)], false, false, false, false); // Z=0
        inst!(AddI Eq, 0, 0, 1).run(&mut s);
        assert_eq!(r(&s, 0), 5.into(), "r0 should be unchanged");
    }

    /// When the condition is met, the instruction executes.
    #[test]
    fn condition_code_executes_when_met() {
        // Eq requires Z=1.
        let mut s = state(&[(0, 5)], true, false, false, false); // Z=1
        inst!(AddI Eq, 0, 0, 1).run(&mut s);
        assert_eq!(r(&s, 0), 6.into(), "r0 should be incremented");
    }

    /// When the condition is not met, the flags are preserved.
    #[test]
    fn condition_code_preserves_flags_when_skipped() {
        // Ne requires Z=0; Z=1 here, so the instruction should not execute.
        let mut s = state(&[(0, 1), (1, 1)], true, true, false, false); // Z=1, N=1
        inst!(Add Ne, 0, 0, 1).run(&mut s);
        assert_eq!(
            f(&s),
            (true, true, false, false),
            "flags should be unchanged"
        );
    }

    /// Cs (carry set) condition code: executes when C=1.
    #[test]
    fn cs_condition_code() {
        let mut s = state(&[(0, 0)], false, false, true, false); // C=1
        inst!(AddI Cs, 0, 0, 10).run(&mut s);
        assert_eq!(r(&s, 0), 10.into());
    }

    // ── Extend test (pre-existing) ────────────────────────────────────────

    #[test]
    fn basic_extend_test() {
        let program: [Inst<ConcreteBitVec<64>>; _] = [inst!(AddI, 0, 1, 1242), inst!(Sub, 2, 0, 1)];
        let mut reducer = Reducer::<ConcreteBitVec<64>, ConcreteBitVec<4>>::default();
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
