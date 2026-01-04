//! Implements enumerating over instructions.

use crate::isa::ArgType;
use crate::{Inst, OpCode, Register, Word64};

/// Enumerates over the instruction space. Needs an `EnumerationInfo` borrow in
/// order to actually enumerate. Does not go over all actual instructions, as
/// not all registers and immediates are used.
#[derive(Clone, Copy, Debug)]
pub struct Enumerator {
    /// The op-code of the current instruction of this enumerator.
    op_code: OpCode,
    /// The indices into the slices of available registers and instructions.
    arg_indices: [usize; 3],
}

/// Configuration for an Enumerator. This is stored separately as we have
/// multiple enumerators using the same configuration.
#[derive(Clone, Copy, Debug)]
pub struct EnumerationInfo<'a> {
    /// The registers to use. Must not be empty.
    pub registers: &'a [Register],
    /// The immediates to use. Must not be empty.
    pub immediates: &'a [u64],
}

fn debug_assert_arg_in_range(arg: usize) {
    debug_assert!(arg < 3, "the argument selector must be in 0-2, but was {arg}.");
}

fn debug_assert_valid_enumeration_info(ei: &EnumerationInfo) {
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
            op_code: unsafe { std::mem::transmute(0u8) },
            arg_indices: [0; 3],
        }
    }

    fn arg_types(&self) -> [ArgType; 3] {
        self.op_code.arg_types()
    }

    fn current_arg(&self, arg: usize, ei: &EnumerationInfo) -> u64 {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(&ei);
        // Take the index, and index into the correct array.
        let i = self.arg_indices[arg];
        match self.arg_types()[arg] {
            ArgType::Reg => ei.registers[i].0 as u64,
            ArgType::Imm => ei.immediates[i],
        }
    }

    /// Returns the length of the array that the given argument index indexes
    /// into.
    fn arg_len(&self, arg: usize, ei: &EnumerationInfo) -> usize {
        debug_assert_arg_in_range(arg);
        debug_assert_valid_enumeration_info(ei);
        match self.arg_types()[arg] {
            ArgType::Reg => ei.registers.len(),
            ArgType::Imm => ei.immediates.len(),
        }
    }

    pub fn current(&self, ei: &EnumerationInfo) -> Inst<Word64> {
        debug_assert_valid_enumeration_info(ei);
        Inst {
            op_code: self.op_code,
            args: [
                self.current_arg(0, ei),
                self.current_arg(1, ei),
                self.current_arg(2, ei),
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
            self.op_code = std::mem::transmute(next);
            Some(())
        }
    }

    fn advance_arg(&mut self, arg: usize, ei: &EnumerationInfo) -> Option<()> {
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

    pub fn advance(&mut self, ei: &EnumerationInfo) -> Option<()> {
        debug_assert_valid_enumeration_info(ei);
        if self.advance_arg(0, ei).is_none() {
            self.arg_indices[0] = 0;
            if self.advance_arg(1, ei).is_none() {
                self.arg_indices[1] = 0;
                if self.advance_arg(2, ei).is_none() {
                    self.arg_indices[2] = 0;
                    if self.advance_op_code().is_none() {
                        return None;
                    }
                }
            }
        }
        Some(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_vec(ei: &EnumerationInfo) -> Vec<Inst<Word64>> {
        let mut e = Enumerator::new();
        let mut ret = vec![];
        loop {
            ret.push(e.current(ei));
            let r = e.advance(ei);
            if r.is_none() { break; }
        }
        ret
    }

    #[test]
    pub fn test_basic() {
        let v = to_vec(&EnumerationInfo {
            registers: &[Register(2)],
            immediates: &[42],
        });
        assert_eq!(v.len(), OpCode::COUNT as usize);
    }
}
