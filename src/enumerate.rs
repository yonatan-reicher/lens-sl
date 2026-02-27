//! Implements enumerating over instructions.

use crate::isa::{ArgType, CondCode, Inst, OpCode, Register};

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
pub struct EnumerationInfo<'a, W> {
    /// The registers to use. Must not be empty.
    pub registers: &'a [Register],
    /// The immediates to use. Must not be empty.
    pub immediates: &'a [W],
}

fn debug_assert_arg_in_range(arg: usize) {
    debug_assert!(
        arg < 3,
        "the argument selector must be in 0-2, but was {arg}."
    );
}

fn debug_assert_valid_enumeration_info<W>(ei: &EnumerationInfo<W>) {
    debug_assert!(
        !ei.registers.is_empty(),
        "enumeration info registers mut not be empty! {ei:?}"
    );
    debug_assert!(
        !ei.immediates.is_empty(),
        "enumeration info immediates slice must not be empty! {ei:?}"
    );
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

    fn current_arg<W>(&self, arg: usize, ei: &EnumerationInfo<W>) -> W {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        // Take the index, and index into the correct array.
        let i = self.arg_indices[arg];
        match self.arg_types()[arg] {
            ArgType::Reg => ei.registers[i].0.as_(),
            ArgType::Imm => ei.immediates[i],
            ArgType::Unused => 0.as_(),
        }
    }

    /// Returns the length of the array that the given argument index indexes
    /// into.
    fn arg_len<W>(&self, arg: usize, ei: &EnumerationInfo<W>) -> usize {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        match self.arg_types()[arg] {
            ArgType::Reg => ei.registers.len(),
            ArgType::Imm => ei.immediates.len(),
            ArgType::Unused => 1,
        }
    }

    pub fn current<W>(&self, ei: &EnumerationInfo<W>) -> Inst<W> {
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

    fn advance_arg<W>(&mut self, arg: usize, ei: &EnumerationInfo<W>) -> Option<()> {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        let len = self.arg_len(arg, ei);
        let next = self.arg_indices[arg] + 1;
        if len <= next {
            return None;
        }
        self.arg_indices[arg] = next;
        Some(())
    }

    pub fn advance<W>(&mut self, ei: &EnumerationInfo<W>) -> Option<()> {
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

    pub fn into_iter<W>(self, ei: &EnumerationInfo<W>) -> impl Iterator<Item = Inst<W>> {
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

struct Iter<'a, W> {
    done: bool,
    ei: &'a EnumerationInfo<'a, W>,
    enumerator: Enumerator,
}

impl<'a, W> Iterator for Iter<'a, W> {
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
    use crate::bit_vec::ConcreteBitVec;

    use super::*;
    use proptest::prelude::*;
    use proptest::property_test;
    use std::collections::HashSet;

    fn to_vec<W>(ei: &EnumerationInfo<W>) -> Vec<Inst<W>> {
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
        let v = to_vec(&EnumerationInfo::<ConcreteBitVec::<8>> {
            registers: &[Register(2)],
            immediates: &[42.into()],
        });
        assert_eq!(v.len(), OpCode::COUNT as usize * CondCode::COUNT as usize);
    }

    #[property_test(config = ProptestConfig { cases: 20, ..ProptestConfig::default() })]
    fn all_registers_and_immediates_appear(
        #[strategy = prop::collection::hash_set(any::<Register>(), 1..15)]
        registers: HashSet<Register>,
        #[strategy = prop::collection::hash_set(0..100, 1..15)]
        immediates: HashSet<ConcreteBitVec<8>>,
    ) {
        prop_assume!(!registers.is_empty());
        prop_assume!(!immediates.is_empty());
        let ei = EnumerationInfo::<ConcreteBitVec::<8>> {
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
}
