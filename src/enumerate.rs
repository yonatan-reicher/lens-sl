//! Implements enumerating over instructions.

use crate::isa::{ArgType, CondCode, Inst, OpCode, Register};
use crate::word::prelude::*;

/// Enumerates over the instruction space. Needs an `EnumerationInfo` borrow in
/// order to actually enumerate. Does not go over all actual instructions, as
/// not all registers and immediates are used.
#[derive(Clone, Copy, Debug)]
pub struct Enumerator {
    /// The op-code of the current instruction of this enumerator.
    op_code: OpCode,
    /// The current condition code of the instruction.
    cond_code: CondCode,
    /// The indices into the slices of available registers and instructions.
    arg_indices: [usize; 3],
}

/// Configuration for an Enumerator. This is stored separately as we have
/// multiple enumerators using the same configuration.
#[derive(Clone, Copy, derive_more::Debug)]
pub enum EnumerationInfo<'a, W: Word> {
    Limited {
        /// The registers to use. Must not be empty.
        registers: &'a [Register],
        /// The immediates to use. Must not be empty.
        immediates: &'a [W::Unsigned],
    },
    /// The enumerated registers and immediates will be every register and immediate!
    Unlimited,
}

fn debug_assert_arg_in_range(arg: usize) {
    debug_assert!(
        arg < 3,
        "the argument selector must be in 0-2, but was {arg}."
    );
}

fn debug_assert_valid_enumeration_info<W: Word>(ei: &EnumerationInfo<W>) {
    match ei {
        EnumerationInfo::Limited {
            registers,
            immediates,
        } => {
            debug_assert!(
                !registers.is_empty(),
                "enumeration info registers mut not be empty! {ei:?}"
            );
            debug_assert!(
                !immediates.is_empty(),
                "enumeration info immediates slice must not be empty! {ei:?}"
            );
        }
        EnumerationInfo::Unlimited => (),
    }
}

impl Enumerator {
    pub fn new() -> Self {
        Self {
            op_code: unsafe { std::mem::transmute::<u8, OpCode>(0) },
            cond_code: unsafe { std::mem::transmute::<u8, CondCode>(0) },
            arg_indices: [0; 3],
        }
    }

    fn arg_types(&self) -> [ArgType; 3] {
        self.op_code.arg_types()
    }

    fn current_arg<W: Word>(&self, arg: usize, ei: &EnumerationInfo<W>) -> W::Unsigned {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        // Take the index, and index into the correct array.
        let i = self.arg_indices[arg];
        match self.arg_types()[arg] {
            ArgType::Reg => match ei {
                EnumerationInfo::Limited { registers, .. } => registers[i].0.as_(),
                EnumerationInfo::Unlimited => (i as u64).as_(),
            },
            ArgType::Imm => match ei {
                EnumerationInfo::Limited { immediates, .. } => immediates[i],
                EnumerationInfo::Unlimited => (i as u64).as_(),
            },
            ArgType::Unused => 0.as_(),
        }
    }

    /// Returns the length of the array that the given argument index indexes into.
    fn arg_max<W: Word>(&self, arg: usize, ei: &EnumerationInfo<W>) -> usize {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        match self.arg_types()[arg] {
            ArgType::Reg => match ei {
                EnumerationInfo::Limited { registers, .. } => registers.len() - 1,
                EnumerationInfo::Unlimited => Register::COUNT as usize - 1,
            },
            ArgType::Imm => match ei {
                EnumerationInfo::Limited { immediates, .. } => immediates.len() - 1,
                EnumerationInfo::Unlimited => W::Unsigned::MAX.as_::<u64>() as usize,
            },
            ArgType::Unused => 0,
        }
    }

    pub fn current<W: Word>(&self, ei: &EnumerationInfo<W>) -> Inst<W> {
        debug_assert_valid_enumeration_info(ei);
        Inst {
            op_code: self.op_code,
            cond_code: self.cond_code,
            args: [
                self.current_arg(0, ei).as_(),
                self.current_arg(1, ei).as_(),
                self.current_arg(2, ei).as_(),
            ],
        }
    }

    fn advance_op_code(&mut self) -> Option<()> {
        unsafe {
            let i: u8 = std::mem::transmute(self.op_code);
            let next = i + 1;
            if next == OpCode::COUNT {
                return None;
            }
            self.op_code = std::mem::transmute::<u8, OpCode>(next);
            Some(())
        }
    }

    fn advance_cond_code(&mut self) -> Option<()> {
        unsafe {
            let i: u8 = std::mem::transmute(self.cond_code);
            let next = i + 1;
            if next == CondCode::COUNT {
                return None;
            }
            self.cond_code = std::mem::transmute::<u8, CondCode>(next);
            Some(())
        }
    }

    fn advance_arg<W: Word>(&mut self, arg: usize, ei: &EnumerationInfo<W>) -> Option<()> {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        let max = self.arg_max(arg, ei);
        let current = self.arg_indices[arg];
        debug_assert!(current <= max);
        if current == max {
            return None;
        }
        self.arg_indices[arg] = current + 1;
        Some(())
    }

    pub fn advance<W: Word>(&mut self, ei: &EnumerationInfo<W>) -> Option<()> {
        debug_assert_valid_enumeration_info(ei);
        if self.advance_arg(0, ei).is_none() {
            self.arg_indices[0] = 0;
            if self.advance_arg(1, ei).is_none() {
                self.arg_indices[1] = 0;
                if self.advance_arg(2, ei).is_none() {
                    self.arg_indices[2] = 0;
                    if self.advance_op_code().is_none() {
                        self.op_code = unsafe { std::mem::transmute::<u8, OpCode>(0) };
                        self.advance_cond_code()?;
                    }
                }
            }
        }
        Some(())
    }

    pub fn into_iter<W: Word>(self, ei: &EnumerationInfo<W>) -> impl Iterator<Item = Inst<W>> {
        Iter {
            done: false,
            ei,
            enumerator: self,
        }
    }
}

impl Default for Enumerator {
    fn default() -> Self {
        Self::new()
    }
}

struct Iter<'a, W: Word> {
    done: bool,
    ei: &'a EnumerationInfo<'a, W>,
    enumerator: Enumerator,
}

impl<'a, W: Word> Iterator for Iter<'a, W> {
    type Item = Inst<W>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let ret = self.enumerator.current(self.ei);
        self.done = self.enumerator.advance(self.ei).is_none();
        Some(ret)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;
    use std::collections::HashSet;

    fn to_vec<W: Word>(ei: &EnumerationInfo<W>) -> Vec<Inst<W>> {
        let mut e = Enumerator::new();
        let mut ret = vec![];
        loop {
            ret.push(e.current(ei));
            let r = e.advance(ei);
            if r.is_none() {
                break;
            }
        }
        ret
    }

    #[test]
    pub fn test_count() {
        let v = to_vec(&EnumerationInfo::Limited::<Word8> {
            registers: &[Register(2)],
            immediates: &[42],
        });
        assert_eq!(v.len(), OpCode::COUNT as usize * CondCode::COUNT as usize);
    }

    #[property_test(config = ProptestConfig { cases: 20, ..ProptestConfig::default() })]
    fn all_registers_and_immediates_appear(
        #[strategy = prop::collection::hash_set(any::<Register>(), 1..15)] registers: HashSet<
            Register,
        >,
        #[strategy = prop::collection::hash_set(0..100u8, 1..15)] immediates: HashSet<u8>,
    ) {
        prop_assume!(!registers.is_empty());
        prop_assume!(!immediates.is_empty());
        let ei = EnumerationInfo::Limited::<Word8> {
            registers: &registers.iter().copied().collect::<Box<[_]>>(),
            immediates: &immediates.iter().copied().collect::<Box<[_]>>(),
        };
        let registers_used: std::collections::HashSet<_> = Enumerator::new()
            .into_iter(&ei)
            .flat_map(|inst| {
                inst.args
                    .iter()
                    .zip(inst.op_code.arg_types())
                    .filter_map(|(arg, arg_type)| {
                        if arg_type == ArgType::Reg {
                            Some(Register(arg.as_()))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        let immediates_used = Enumerator::new()
            .into_iter(&ei)
            .flat_map(|inst| {
                inst.args
                    .iter()
                    .zip(inst.op_code.arg_types())
                    .filter_map(|(arg, arg_type)| {
                        if arg_type == ArgType::Imm {
                            Some(*arg)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<std::collections::HashSet<_>>();
        prop_assert_eq!(registers_used, registers);
        prop_assert_eq!(immediates_used, immediates);
    }

    #[test]
    fn enumeration_info_unlimited_range_is_full() {
        let v = to_vec(&EnumerationInfo::Unlimited::<Word4>);
        let registers = v.iter().filter_map(|x| {
            if x.op_code.arg_types()[0] == ArgType::Reg {
                Some(Register(x.args[0].as_()))
            } else {
                None
            }
        }).collect::<std::collections::HashSet<_>>();
        let immediates = v.iter().filter_map(|x| {
            // For now, immediates appear mostly on the second argument
            if x.op_code.arg_types()[2] == ArgType::Imm {
                Some(x.args[2])
            } else {
                None
            }
        }).collect::<std::collections::HashSet<_>>();
        dbg!(&registers);
        dbg!(&immediates);
        assert_eq!(registers.len(), Register::COUNT as usize);
        assert_eq!(immediates.len(), u4::MAX.as_::<u64>() as usize + 1);
    }
}
