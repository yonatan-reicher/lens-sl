// Imports

use crate::all::All;
use crate::all_permutations::Iter as PermutationIter;
use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::arm::state::BitMask;
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

use rustc_hash::FxHashMap;

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
    | Add     | Reg(Inp) | Reg(Out) | Reg(Inp) | "add"  |    true     |     true      |
    | AddI    | Reg(Inp) | Reg(Out) | Imm      | "add"  |    false    |     true      |
    | Sub     | Reg(Inp) | Reg(Out) | Reg(Inp) | "sub"  |    false    |     true      |
    | SubI    | Reg(Inp) | Reg(Out) | Imm      | "sub"  |    false    |     true      |
    | And     | Reg(Inp) | Reg(Out) | Reg(Inp) | "and"  |    true     |     false     |
    | Eor     | Reg(Inp) | Reg(Out) | Reg(Inp) | "eor"  |    true     |     false     |
    | Mov     | Reg(Inp) | Reg(Out) | Unused   | "mov"  |    false    |     false     |
    | MovI    | Reg(Inp) | Imm      | Unused   | "mov"  |    false    |     false     |
    | Mul     | Reg(Inp) | Reg(Out) | Reg(Inp) | "mul"  |    true     |     false     |
    | Orr     | Reg(Inp) | Reg(Out) | Reg(Inp) | "orr"  |    true     |     false     |
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
pub enum ShiftCode {
    None,
    /// Arithmetic shift right - shift right but keep MSB the same.
    /// Must have 1 <= n <= 32.
    #[display("asr #{_0}")]
    Asr(u8),
    /// Logical shift left.
    /// NOTE: There exists a synonym called `asl`, very confusing. lol.
    /// Must have 1 <= n <= 31.
    #[display("lsl #{_0}")]
    Lsl(u8),
    /// Logical shift right.
    /// Must have 1 <= n <= 32
    #[display("lsr #{_0}")]
    Lsr(u8),
    /// Rotate right.
    /// Must have 1 <= n <= 31
    #[display("ror #{_0}")]
    Ror(u8),
    /// Rotate right one bit, sign extended.
    #[display("rrx")]
    Rrx,
}

/// A single instruction.
/// NOTE: This is missing the 'S' bit - an optional bit toggling whether condition codes (flags)
/// should be updated. In Lens, they pretend it doesn't exist and that only `cmp` and `tst` update
/// the flags. When in Rome, act like a Roman.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[cfg_attr(test, derive(proptest_derive::Arbitrary))]
pub struct Inst<W> {
    pub op_code: OpCode,
    pub cond_code: CondCode,
    pub shift: ShiftCode,
    pub args: [W; 3],
}

// =========================================== State ==============================================

pub mod state;

pub use state::{Flags, FlagsBitField, State, StateVars, SymbolicState};

fn run_instruction<W: Word>(inst: &Inst<W>, state: &mut State<W>) {
    /// Get a register value.
    macro_rules! r {
        ($i:literal) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i].is_reg());
            let r = Register(inst.args[$i].into_word::<Word8>().into());
            state.get_register(r)
        }};
    }
    /// Set a register value.
    macro_rules! set {
        (r![$i:literal] <- $value:expr) => {{
            debug_assert!($i < 3);
            debug_assert!(inst.op_code.arg_types()[$i].is_reg());
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
        ($i:literal) => {
            inst.args[$i]
        };
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

fn run_instruction_symbolic<'st, W: Word>(
    inst: &Inst<W>,
    state: &mut SymbolicState<'st, W::SmtWord<'st>>,
) {
    let enabled = inst.cond_code.check(state.flags);

    /// Get a register value.
    macro_rules! r {
        ($i:literal) => {
            state.registers[{
                debug_assert!($i < 3);
                debug_assert!(inst.op_code.arg_types()[$i].is_reg());
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
        <W as All>::Iter: Clone,
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
    pub fn new_recalculate(registers: &[Register]) -> Self
    where
        <W as All>::Iter: Clone,
    {
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
            for inst in Enumerator::new(ei) {
                input.clone_to(&mut output);
                inst.run(&mut output);
                // Store!
                let inputs = ret.entry((inst, output)).or_insert_with(Vec::new);
                if !inputs.contains(input) {
                    inputs.push(*input);
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

    // The parts of the state that this instruction reads.
    pub fn read_mask(&self) -> state::Mask {
        let mut ret = state::Mask::default();
        for (a, t) in self.args_with_types() {
            if let ArgType::Reg(RegArgType::Inp) = t {
                ret[a.into()] |= true
            }
        }
        ret.flags = self.cond_code != CondCode::Al;
        ret
    }

    /// The parts of the state that this instruction writes.
    pub fn write_mask(&self) -> state::Mask {
        let mut ret = state::Mask::default();
        for (a, t) in self.args_with_types() {
            if let ArgType::Reg(RegArgType::Out) = t {
                ret[a.into()] |= true
            }
        }
        ret.flags = self.op_code.affects_flags();
        ret
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

    pub fn run_masked(&self, masked: state::Masked<W>) -> Option<state::Masked<W>> {
        // Check if we can run
        let input_mask: BitMask = masked.mask();
        let missing_inputs_mask = self.read_mask().into_bit_mask() & !input_mask;
        if !missing_inputs_mask.is_empty() {
            return None;
        }
        // We can. Run!
        let mut state = *masked.state();
        self.run(&mut state);
        let change_mask = state.diff(masked.state());
        let output_mask = change_mask | input_mask.into_mask();
        Some(state.masked(output_mask))
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

    pub fn reduce<WSmall: Word>(&self, reducer: &mut Reducer<W, WSmall>) -> Inst<WSmall> {
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
        let info = ImmediateInfo {
            // TODO: is_shift: op_code.is_shift_instruction(),
            is_shift: false,
        };
        Inst {
            op_code: self.op_code,
            cond_code: self.cond_code,
            shift: self.shift,
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
                ArgType::Reg(..) | ArgType::Unused => SliceOrSingle::Single(arg.into_word()),
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
                    shift: self.shift,
                    args: [arg0, arg1, arg2],
                })
            })
        })
    }
}

pub mod enumerate;
impl<W: Word> Inst<W> {
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
    }: &Inst<&str>, // Look at this cute hack! Taking the arguments as strings.
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

impl<W: Debug> Debug for Inst<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let arg_strings = self.args.each_ref().map(|a| format!("{a:?}"));
        fmt_inst(
            &Inst {
                op_code: self.op_code,
                cond_code: self.cond_code,
                shift: self.shift,
                args: arg_strings.each_ref().map(|s| s.as_str()),
            },
            f,
        )
    }
}

impl<W: Display> Display for Inst<W> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let arg_strings = self.args.each_ref().map(|a| format!("{a}"));
        fmt_inst(
            &Inst {
                op_code: self.op_code,
                cond_code: self.cond_code,
                shift: self.shift,
                args: arg_strings.each_ref().map(|s| s.as_str()),
            },
            f,
        )
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

/// Returns a mask for the input and the masked output.
pub fn run_program_masked<W: Word>(
    prog: impl IntoIterator<Item = Inst<W>>,
    input: State<W>,
) -> (BitMask, state::Masked<W>) {
    let (mut input_mask, mut output_mask) = (BitMask::EMPTY, BitMask::EMPTY);
    let mut current_state = input;
    for inst in prog {
        let prev = current_state;
        inst.run(&mut current_state);
        let read_mask = inst.read_mask().into_bit_mask();
        let change_mask = current_state.diff(&prev).into_bit_mask();
        let old_output_mask = output_mask;
        // Add to input whatever you read and didn't write to earlier
        input_mask = input_mask | (read_mask & !old_output_mask);
        // Add to output whatever you are writing to
        output_mask = output_mask | change_mask;
    }
    (input_mask, current_state.masked(output_mask.into_mask()))
}

pub mod parse;
pub use parse::parse;

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
        for inst in Inst::enumerate(ei) {
            let x = &bm[(inst, state)];
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
