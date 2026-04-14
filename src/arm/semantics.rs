use super::state::{Flags, Mask, StateTrait};
use super::{CondCode, Inst, OpCode, Register, ShiftCode};
use crate::bool::prelude::*;
use crate::word::prelude::*;

// =================================================================================================
//                                        Run Instruction
// =================================================================================================

/// B - the boolean
/// W - the word type
/// WI - the word type for instruction arguments (this is different because we don't run symbolic
/// WIShift - the word type for instruction's shift arguments.
/// instructions, only concrete instructions).
/// read_mask - turns on whatever was read from the input. Does not clear.
/// write_mask - turns on whatever was written to, and is not cleared.
/// from_param - needed for creating default new words.
pub fn run<W, WI, WIShift, State>(
    inst: &Inst<WI, WIShift>,
    input: &State,
    read_mask: &mut Mask<W::Bool>,
    write_mask: &mut Mask<W::Bool>,
    from_param: &W::FromParam,
) -> State
where
    W: AbstractWord,
    WI: Word,
    WIShift: Word,
    State: Clone + StateTrait<W>,
    W::FromParam: Clone,
{
    // Condition code
    let enabled = if inst.cond_code == CondCode::Al {
        // As this can be determined without checking the flags, we should, to not mark them as
        // read.
        Bool::r#true()
    } else {
        read_mask.flags = Bool::r#true();
        inst.cond_code.check(input.flags())
    };

    // Initialize
    let mut output = input.clone();
    let mut s = ReadWriteTracker::new(&mut output, read_mask, write_mask, enabled);

    // Small short-hands to access things easier
    let args = inst.args;
    let regs = args.map(Register::from);
    #[rustfmt::skip] macro_rules! r { ($i:literal) => { s.reg(regs[$i]) } }
    #[rustfmt::skip] macro_rules! imm { ($i:literal) => { inst.args[$i].into_abstract_word::<W>(from_param.clone()) } }
    macro_rules! set_reg {
        ($r:expr, $e:expr) => {{
            // Need this to be in a variable to deal with borrowing rules.
            let res = $e;
            s.set_reg($r, res);
        }};
    }
    macro_rules! set_flags {
        ($e:expr) => {{
            // Need this to be in a variable to deal with borrowing rules.
            let res = $e;
            s.set_flags(res);
        }};
    }

    // Shift!
    let mut carry_out = None;
    let mut shift = |x: W| {
        let (x, c) = inst.shift.apply(
            x,
            input.flags().c, /* Give c without reading the flags */
        );
        carry_out = c;
        x
    };

    // The actual operation code behaviour
    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => set_reg!(regs[0], r![1] + shift(r![2])),
        AddI => set_reg!(regs[0], r![1] + shift(imm![2])),
        Sub => set_reg!(regs[0], r![1] + -shift(r![2])),
        SubI => set_reg!(regs[0], r![1] + -shift(imm![2])),
        Rsb => set_reg!(regs[0], -r![1] + shift(r![2])),
        RsbI => set_reg!(regs[0], -r![1] + shift(imm![2])),
        And => set_reg!(regs[0], r![1] & shift(r![2])),
        Bic => set_reg!(regs[0], r![1] & !shift(r![2])),
        Eor => set_reg!(regs[0], r![1] ^ shift(r![2])),
        Mov => set_reg!(regs[0], shift(r![1])),
        MovI => set_reg!(regs[0], shift(imm![1])),
        Movt => set_reg!(
            regs[0],
            r![0].bottom_half()
                | (shift(imm![1])
                    << W::Word::from(W::BITS / 2).into_abstract_word(from_param.clone()))
        ),
        Movw => set_reg!(
            regs[0],
            (r![0] << W::Word::from(W::BITS / 2).into_abstract_word(from_param.clone()))
                | shift(imm![1]).bottom_half()
        ),
        Mul => set_reg!(regs[0], r![1] * shift(r![2])),
        Orr => set_reg!(regs[0], r![1] | shift(r![2])),
        Cmp => set_flags!(Flags::from_sub(r![0], shift(r![1]))),
        CmpI => set_flags!(Flags::from_sub(r![0], shift(imm![1]))),
        Tst => set_flags!(Flags::from_and(r![0], shift(r![1]))),
        TstI => set_flags!(Flags::from_and(r![0], shift(imm![1]))),
    }

    // The shift could change the carry value (rrx). Let's update it.
    if let Some(c) = carry_out {
        let f = s.flags();
        s.set_flags(Flags { c, ..f });
    }

    output
}

// ================================================================================================
//                                      Read Write Tracking
// ================================================================================================

struct ReadWriteTracker<'a, B, W, State> {
    state: &'a mut State,
    read: &'a mut Mask<B>,
    write: &'a mut Mask<B>,
    /// Reading and writing should be ignored both (instruction's condition was false)
    enabled: B,
    _marker: std::marker::PhantomData<W>,
}

impl<'a, W: AbstractWord, State: StateTrait<W>> ReadWriteTracker<'a, W::Bool, W, State> {
    /// enabled - Reading and writing should be ignored both (instruction's condition was false)
    pub fn new(
        state: &'a mut State,
        read: &'a mut Mask<W::Bool>,
        write: &'a mut Mask<W::Bool>,
        enabled: W::Bool,
    ) -> Self {
        Self {
            state,
            read,
            write,
            enabled,
            _marker: Default::default(),
        }
    }

    // ========= Reading ========

    // pub fn maybe_reg(&mut self, r: Register, cond: W::Bool, or: W) -> W {
    //     let cond = self.enabled & cond;
    //     self.read[r] = self.read[r] | cond;
    //     cond.if_then_else(self.state.reg(r), or)
    // }

    /// Watch out - when self.enabled is false, still returns the actual value.
    pub fn reg(&mut self, r: Register) -> W {
        self.read[r] = self.read[r] | self.enabled;
        self.state.reg(r)
    }

    // pub fn maybe_flags(&mut self, cond: W::Bool, or: Flags<W::Bool>) -> Flags<W::Bool> {
    //     let cond = self.enabled & cond;
    //     self.read.flags = self.read.flags | cond;
    //     let f = self.state.flags();
    //     Flags {
    //         z: cond.if_then_else(f.z, or.z),
    //         n: cond.if_then_else(f.n, or.n),
    //         c: cond.if_then_else(f.c, or.c),
    //         v: cond.if_then_else(f.v, or.v),
    //     }
    // }

    pub fn flags(&mut self) -> Flags<W::Bool> {
        self.read.flags = self.read.flags | self.enabled;
        self.state.flags()
    }

    // ========= Writing ========

    pub fn set_reg(&mut self, r: Register, w: W) {
        self.state.maybe_set_reg(r, self.enabled, w);
        self.write[r] = self.write[r] | self.enabled;
    }

    pub fn set_flags(&mut self, f: Flags<W::Bool>) {
        self.state.maybe_set_flags(self.enabled, f);
        self.write.flags = self.write.flags | self.enabled;
    }
}

// =================================================================================================
//                                          Shift Code
// =================================================================================================

impl<WShift> ShiftCode<WShift> {
    fn apply<W: AbstractWord>(&self, x: W, carry_in: W::Bool) -> (W, Option<W::Bool>)
    where
        WShift: Word,
    {
        let from_param = || x.get_from_param();
        let convert = |i: WShift| i.into_abstract_word::<W>(from_param());
        let make_msb = |on| on << W::Word::from(W::BITS - 1).into_abstract_word::<W>(from_param());
        let msb = |x| x >> convert(31.into());
        let lsb = |x| x & convert(1.into());
        let mut carry = Option::None;
        use ShiftCode::*;
        let out = match *self {
            None => x,
            Asr(i) => (x >> convert(i)) | make_msb(msb(x)),
            Lsl(i) => x << convert(i),
            Lsr(i) => x >> convert(i),
            Ror(i) => (x >> convert(i)) | (x << convert(WShift::from(W::BITS) - i)),
            Rrx => {
                // GODAMNIT!!! This was hard to implement and will majorly slow us down, and
                // orginal Lens doesn't even f-ing implement it.
                // TODO: How much faster are we without this?
                carry = Some(lsb(x).is_zero());
                (x >> convert(1.into()))
                    | make_msb(carry_in.if_then_else(convert(1.into()), convert(0.into())))
            }
        };
        (out, carry)
    }

    pub fn affects_flags(&self) -> bool {
        match self {
            ShiftCode::None
            | ShiftCode::Asr(_)
            | ShiftCode::Lsl(_)
            | ShiftCode::Lsr(_)
            | ShiftCode::Ror(_) => false,
            ShiftCode::Rrx => true,
        }
    }

    pub fn reads_flags(&self) -> bool {
        match self {
            ShiftCode::None
            | ShiftCode::Asr(_)
            | ShiftCode::Lsl(_)
            | ShiftCode::Lsr(_)
            | ShiftCode::Ror(_) => false,
            ShiftCode::Rrx => true,
        }
    }
}

// =================================================================================================
//                                          Instruction =================================================================================================
impl<W, WShift> Inst<W, WShift> {
    pub fn affects_flags(&self) -> bool {
        self.op_code.affects_flags() || self.shift.affects_flags()
    }

    pub fn reads_flags(&self) -> bool {
        self.cond_code != CondCode::Al || self.shift.reads_flags()
    }
}

// ================================================================================================
//                                             Tests
// ================================================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{State, inst};

    #[test]
    fn registers_not_read_when_condition_false() {
        let inst: Inst<Word4, Word2> = inst!(Add Eq, 0, 0, 0);
        let state = State::<Word4> {
            registers: [1.into(); 16],
            flags: Flags::default().into(),
        };
        let (mut read, mut write) = Default::default();
        run(&inst, &state, &mut read, &mut write, &());
        assert_eq!(read, Mask::JUST_FLAGS);
        assert_eq!(write, Mask::EMPTY);
    }

    #[test]
    fn shift() {
        let inst: Inst<Word64, Word6> = inst!(MovI, 0, 15; shift Lsl(2.into()));
        let state = State::<Word64> {
            registers: [1.into(); 16],
            flags: Flags::default().into(),
        };
        let (mut read, mut write) = Default::default();
        let state = run(&inst, &state, &mut read, &mut write, &());
        assert_eq!(state.registers[0], (15 * 4).into());
    }
}
