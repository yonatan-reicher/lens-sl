use super::state::{Flags, Mask, StateTrait};
use super::{CondCode, Inst, OpCode, Register};
use crate::bool::prelude::*;
use crate::word::prelude::*;

/// B - the boolean
/// W - the word type
/// WI - the word type for instruction arguments (this is different because we don't run symbolic
/// instructions, only concrete instructions).
/// read_mask - turns on whatever was read from the input. Does not clear.
/// write_mask - turns on whatever was written to, and is not cleared.
/// from_param - needed for creating default new words.
pub fn run<W, WI, State>(
    inst: &Inst<WI>,
    input: &State,
    read_mask: &mut Mask<W::Bool>,
    write_mask: &mut Mask<W::Bool>,
    from_param: &W::FromParam,
) -> State
where
    W: AbstractWord,
    WI: Word,
    State: Clone + StateTrait<W>,
    W::FromParam: Clone,
{
    // Initialize
    let mut i = InputAndReadMask::new(input, read_mask);

    // Condition code
    let enabled = if inst.cond_code == CondCode::Al {
        // As this can be determined without checking the flags, we should, to not mark them as
        // read.
        Bool::r#true()
    } else {
        inst.cond_code.check(i.flags())
    };
    let mut output = input.clone();
    let mut o = OutputAndWriteMaskAndEnabledFlag::new(&mut output, write_mask, enabled);

    // Small short-hands to access things easier
    let args = inst.args;
    let regs = args.map(Register::from);
    let something = W::Word::from(0).into_abstract_word(from_param.clone()); // doesn't matter
    #[rustfmt::skip] macro_rules! r { ($i:literal) => { i.maybe_reg(regs[$i], enabled, something) } }
    #[rustfmt::skip] macro_rules! imm { ($i:literal) => { inst.args[$i].into_abstract_word::<W>(from_param.clone()) } }

    // The actual operation code behaviour
    use OpCode::*;
    match inst.op_code {
        Nop => (),
        Add => {
            o.set_flags(Flags::from_add(r![1], r![2]));
            o.set_reg(regs[0], r![1] + r![2]);
        }
        AddI => {
            o.set_flags(Flags::from_add(r![1], imm![2]));
            o.set_reg(regs[0], r![1] + imm![2]);
        }
        Sub => {
            o.set_flags(Flags::from_sub(r![1], r![2]));
            o.set_reg(regs[0], r![1] + -r![2]);
        }
        SubI => {
            o.set_flags(Flags::from_sub(r![1], imm![2]));
            o.set_reg(regs[0], r![1] + -imm![2]);
        }
        And => o.set_reg(regs[0], r![1] & r![2]),
        Eor => o.set_reg(regs[0], r![1] ^ r![2]),
        Mov => o.set_reg(regs[0], r![1]),
        MovI => o.set_reg(regs[0], imm![1]),
        Mul => o.set_reg(regs[0], r![1] * r![2]),
        Orr => o.set_reg(regs[0], r![1] | r![2]),
    }

    output
}

/// stores a state and those parts of the state that have been read.
struct InputAndReadMask<'a, B, W, State> {
    input: &'a State,
    read: &'a mut Mask<B>,
    _marker: std::marker::PhantomData<W>,
}

impl<'a, W: AbstractWord, State: StateTrait<W>> InputAndReadMask<'a, W::Bool, W, State> {
    pub fn new(input: &'a State, read: &'a mut Mask<W::Bool>) -> Self {
        Self {
            input,
            read,
            _marker: Default::default(),
        }
    }

    pub fn maybe_reg(&mut self, r: Register, cond: W::Bool, or: W) -> W {
        self.read[r] = self.read[r] | cond;
        cond.if_then_else(self.input.reg(r), or)
    }

    pub fn reg(&mut self, r: Register) -> W {
        self.read[r] = Bool::r#true();
        self.input.reg(r)
    }

    pub fn maybe_flags(&mut self, cond: W::Bool, or: Flags<W::Bool>) -> Flags<W::Bool> {
        self.read.flags = self.read.flags | cond;
        let f = self.input.flags();
        Flags {
            z: cond.if_then_else(f.z, or.z),
            n: cond.if_then_else(f.n, or.n),
            c: cond.if_then_else(f.c, or.c),
            v: cond.if_then_else(f.v, or.v),
        }
    }

    pub fn flags(&mut self) -> Flags<W::Bool> {
        self.read.flags = Bool::r#true();
        self.input.flags()
    }
}

/// Stores an output along with an enabled flag telling us whether the condition of the condition
/// code was true.
struct OutputAndWriteMaskAndEnabledFlag<'a, B, W, State> {
    output: &'a mut State,
    write: &'a mut Mask<B>,
    enabled: B,
    _marker: std::marker::PhantomData<W>,
}

impl<'a, W: AbstractWord, State: StateTrait<W>> OutputAndWriteMaskAndEnabledFlag<'a, W::Bool, W, State> {
    pub fn new(output: &'a mut State, write: &'a mut Mask<W::Bool>, enabled: W::Bool) -> Self {
        Self {
            output,
            enabled,
            write,
            _marker: Default::default(),
        }
    }

    pub fn set_reg(&mut self, r: Register, w: W) {
        self.output.maybe_set_reg(r, self.enabled, w);
        self.write[r] = self.enabled;
    }

    pub fn set_flags(&mut self, f: Flags<W::Bool>) {
        self.output.maybe_set_flags(self.enabled, f);
        self.write.flags = self.enabled;
    }
}
