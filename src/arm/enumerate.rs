//! Implements enumerating over instructions.

use crate::arm::{ArgType, CondCode, Inst, OpCode, Register};
use crate::word::prelude::*;
use std::fmt::Debug;

#[derive(Clone, Copy, Debug)]
pub struct Enumerator<'a, W> {
    ei: EnumerationInfo<'a, W>,
    /// Are we done?
    done: bool,
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
pub struct EnumerationInfo<'a, W> {
    /// The registers to use. Must not be empty.
    pub registers: EnumerationInfoOptions<'a, Register>,
    /// The immediates to use. Must not be empty.
    pub immediates: EnumerationInfoOptions<'a, W>,
}

#[derive(Clone, Copy, derive_more::Debug)]
pub enum EnumerationInfoOptions<'a, T> {
    /// The options will be given from this slice.
    Limited(&'a [T]),
    /// The enumerated options will be every register and immediate!
    Unlimited,
}

impl<'a, W> Enumerator<'a, W> {
    pub fn new(ei: EnumerationInfo<'a, W>) -> Self {
        Self {
            ei,
            done: false,
            op_code: unsafe { std::mem::transmute::<u8, OpCode>(0) },
            cond_code: unsafe { std::mem::transmute::<u8, CondCode>(0) },
            arg_indices: [0; 3],
        }
    }

    fn arg_types(&self) -> [ArgType; 3] {
        self.op_code.arg_types()
    }
}

impl<'a, W: Word> Enumerator<'a, W> {
    fn try_current_arg(&self, arg: usize, ei: &EnumerationInfo<W>) -> Option<W> {
        // Take the index, and index into the correct array.
        let i = self.arg_indices[arg];
        match self.arg_types()[arg] {
            ArgType::Reg(..) => match &ei.registers {
                EnumerationInfoOptions::Limited(r) => r.get(i).map(|r| Word8::from(*r).into_word()),
                EnumerationInfoOptions::Unlimited => Some(i.into()),
            },
            ArgType::Imm => match &ei.immediates {
                EnumerationInfoOptions::Limited(im) => im.get(i).copied(),
                EnumerationInfoOptions::Unlimited => Some(i.into()),
            },
            ArgType::Unused => Some(0.into()),
        }
    }

    /// Returns the length of the array that the given argument index indexes into.
    fn try_arg_max(&self, arg: usize, ei: &EnumerationInfo<W>) -> Option<usize> {
        match self.arg_types()[arg] {
            ArgType::Reg(..) => match &ei.registers {
                EnumerationInfoOptions::Limited(r) => r.len().checked_sub(1),
                EnumerationInfoOptions::Unlimited => Some(Register::COUNT as usize - 1),
            },
            ArgType::Imm => match &ei.immediates {
                EnumerationInfoOptions::Limited(i) => i.len().checked_sub(1),
                EnumerationInfoOptions::Unlimited => Some(W::MAX.into()),
            },
            ArgType::Unused => Some(0),
        }
    }

    fn try_current(&self, ei: &EnumerationInfo<W>) -> Option<Inst<W>> {
        Some(Inst {
            op_code: self.op_code,
            cond_code: self.cond_code,
            args: [
                self.try_current_arg(0, ei)?,
                self.try_current_arg(1, ei)?,
                self.try_current_arg(2, ei)?,
            ],
        })
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

    fn advance_arg(&mut self, arg: usize, ei: &EnumerationInfo<W>) -> Option<()> {
        let max = self.try_arg_max(arg, ei)?;
        let current = self.arg_indices[arg];
        debug_assert!(current <= max);
        if current == max {
            return None;
        }
        self.arg_indices[arg] = current + 1;
        Some(())
    }

    pub fn advance(&mut self) -> Option<()> {
        let ei = &self.ei.clone();
        if self.advance_arg(0, ei).is_none() {
            self.arg_indices[0] = 0;
            if self.advance_arg(1, ei).is_none()
                || (self.op_code.commutative() && self.arg_indices[1] > self.arg_indices[2])
            {
                self.arg_indices[1] = 0;
                if self.advance_arg(2, ei).is_none() {
                    self.arg_indices[2] = 0;
                    if self.advance_op_code().is_none() {
                        self.op_code = unsafe { std::mem::transmute::<u8, OpCode>(0) };
                        if self.advance_cond_code().is_none() {
                            self.done = true;
                            return None;
                        }
                    }
                }
            }
        }
        Some(())
    }
}

impl<'a, W> Default for Enumerator<'a, W> {
    fn default() -> Self {
        Self::new(EnumerationInfo::default())
    }
}

impl<'a, W> Default for EnumerationInfo<'a, W> {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            immediates: Default::default(),
        }
    }
}

impl<'a> IntoIterator for EnumerationInfoOptions<'a, Register> {
    type Item = Register;
    type IntoIter = std::iter::Copied<std::slice::Iter<'a, Register>>;
    fn into_iter(self) -> Self::IntoIter {
        use EnumerationInfoOptions::{Limited, Unlimited};
        match self {
            Limited(items) => items.iter().copied(),
            Unlimited => Register::ALL.as_slice().iter().copied(),
        }
    }
}

impl<'a, T> Default for EnumerationInfoOptions<'a, T> {
    fn default() -> Self {
        Self::Unlimited
    }
}

impl<'a, W: Word> Iterator for Enumerator<'a, W> {
    type Item = Inst<W>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let Some(ret) = self.try_current(&self.ei) else {
            self.advance();
            return self.next();
        };
        self.advance();
        Some(ret)
    }
}

#[cfg(test)]
mod tests {
    use crate::inst;

    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;
    use std::collections::HashSet;

    fn to_vec<W: Word>(ei: &EnumerationInfo<W>) -> Vec<Inst<W>> {
        Enumerator::new(*ei).collect()
    }

    #[test]
    pub fn test_count() {
        let v = to_vec(&EnumerationInfo::<Word8> {
            registers: EnumerationInfoOptions::Limited(&[Register(2)]),
            immediates: EnumerationInfoOptions::Limited(&[42.into()]),
        });
        assert_eq!(v.len(), OpCode::COUNT as usize * CondCode::COUNT as usize);
    }

    #[property_test(config = ProptestConfig { cases: 20, ..ProptestConfig::default() })]
    fn all_registers_and_immediates_appear(
        #[strategy = prop::collection::hash_set(any::<Register>(), 1..17)] registers: HashSet<
            Register,
        >,
        #[strategy = prop::collection::hash_set(any::<Word8>(), 1..17)] immediates: HashSet<Word8>,
    ) {
        prop_assume!(!registers.is_empty());
        prop_assume!(!immediates.is_empty());
        let ei = EnumerationInfo::<Word8> {
            registers: EnumerationInfoOptions::Limited(
                &registers.iter().copied().collect::<Box<[_]>>(),
            ),
            immediates: EnumerationInfoOptions::Limited(
                &immediates.iter().copied().collect::<Box<[_]>>(),
            ),
        };
        let registers_used: std::collections::HashSet<_> = Enumerator::new(ei)
            .flat_map(|inst| {
                inst.args
                    .iter()
                    .zip(inst.op_code.arg_types())
                    .filter_map(|(arg, arg_type)| {
                        if arg_type.is_reg() {
                            Some(Register(u8::from(*arg)))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        let immediates_used = Enumerator::new(ei)
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
        println!("checking");
        prop_assert_eq!(registers_used, registers);
        prop_assert_eq!(immediates_used, immediates);
        println!("success");
    }

    #[test]
    fn enumeration_info_unlimited_range_is_full() {
        let v = to_vec(&EnumerationInfo::<Word4> {
            registers: EnumerationInfoOptions::Unlimited,
            immediates: EnumerationInfoOptions::Unlimited,
        });
        let registers = v
            .iter()
            .filter_map(|x| {
                if x.op_code.arg_types()[0].is_reg() {
                    Some(Register(x.args[0].into()))
                } else {
                    None
                }
            })
            .collect::<std::collections::HashSet<_>>();
        let immediates = v
            .iter()
            .filter_map(|x| {
                // For now, immediates appear mostly on the second argument
                if x.op_code.arg_types()[2] == ArgType::Imm {
                    Some(x.args[2])
                } else {
                    None
                }
            })
            .collect::<std::collections::HashSet<_>>();
        dbg!(&registers);
        dbg!(&immediates);
        assert_eq!(registers.len(), Register::COUNT as usize);
        assert_eq!(immediates.len() - 1, Word4::MAX.into());
    }

    #[test]
    fn commutatives_are_half_trimmed() {
        let v = to_vec(&EnumerationInfo::<Word4> {
            registers: EnumerationInfoOptions::Unlimited,
            immediates: EnumerationInfoOptions::Unlimited,
        });
        assert!(v.contains(&inst![Add, 1, 3, 5]));
        assert!(!v.contains(&inst![Add, 1, 5, 3]));
    }

    #[test]
    fn empty_registers() {
        for inst in Enumerator::new(EnumerationInfo::<Word4> {
            registers: EnumerationInfoOptions::Limited(&[]),
            immediates: EnumerationInfoOptions::Limited(&[1.into()]),
        }) {
            if false && inst.op_code.arg_types().into_iter().any(|x| x.is_reg()) {
                panic!(
                    "Sadly, the enumerator generated the following instruction: '{inst}'. It has \
                     a register argument, but that should not be possible."
                );
            }
        }
    }
}
