use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state;
use crate::arm::{Inst, Register};
use crate::word::BitWord;
use crate::word::prelude::*;
use functionality::prelude::*;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};

type Input<W> = state::Masked<W>;
type Output<W> = state::Masked<W>;

#[derive(Debug, Clone)]
pub struct Bank<'a, W, WShift = BitWord<W>> {
    buckets: FxHashMap<Input<W>, Bucket<W, WShift>>,
    ei: EnumerationInfo<'a, W>,
}

#[derive(Debug, Clone)]
pub struct Bucket<W, WShift = BitWord<W>>(FxHashMap<Output<W>, FxHashSet<Inst<W, WShift>>>);

pub type Insts<W, WShift = BitWord<W>> = FxHashSet<Inst<W, WShift>>;

impl<'a, W: Word + HasBitWord> Bank<'a, W> {
    pub fn new(ei: EnumerationInfo<'a, W>) -> Self {
        Bank {
            buckets: FxHashMap::default(),
            ei,
        }
    }

    pub fn get(&mut self, input: &Input<W>) -> &mut Bucket<W> {
        self.buckets
            .entry(*input)
            .or_insert_with(|| Bucket::new(input, self.ei))
    }
}

impl<W: Word + HasBitWord> Bucket<W> {
    pub fn new(input: &Input<W>, ei: EnumerationInfo<W>) -> Self {
        // Initialize the bucket by running all instructions on the input and recording outputs.
        Inst::enumerate(EnumerationInfo {
            // Limit only to registers relevant to the input.
            inp_registers: Register::all()
                .filter(|r| {
                    input.mask().into_mask()[*r] && ei.inp_registers.into_iter().contains(r)
                })
                .collect::<Vec<_>>()
                .as_slice()
                .pipe(EnumerationInfoOptions::Limited),
            ..ei
        })
        // Filter instructions further! Masks need to match exactly.
        .filter(|inst| {
            assert!(
                inst.potential_read_mask()
                    .is_sub_mask(&input.mask().into_mask().mutate(|m| m.flags = true /* Flags aren't pruned above */)),
                "Instruction {inst} has potential read mask {} which is not a sub-mask of input mask {}",
                inst.potential_read_mask(),
                input.mask().into_mask(),
            );
            inst.potential_read_mask() == input.mask().into_mask()
        })
        // Run
        .map(|inst| {
            let mut output = *input.state();
            inst.run(&mut output);
            (output.masked(inst.potential_write_mask()), inst)
        })
        // Group by output.
        .fold(Bucket(FxHashMap::default()), |bucket, (output, inst)| {
            bucket.mutate(|bucket| {
                bucket.0.entry(output).or_default().insert(inst);
            })
        })
    }

    pub fn get(&mut self, output: &Output<W>) -> &Insts<W> {
        self.0.entry(*output).or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arm::state::{Mask, State};
    use crate::inst;

    #[test]
    fn test_bank() {
        let mut b = Bank::<Word4>::new(EnumerationInfo {
            inp_registers: EnumerationInfoOptions::Limited(&[Register(0), Register(1)]),
            out_registers: EnumerationInfoOptions::Limited(&[Register(0), Register(1)]),
            ..default()
        });
        let start = State::default()
            .mutate(|s| s[Register(0)] = 1.into())
            .masked(Mask::just_register(Register(0)));
        let end = State::default()
            .mutate(|s| s[Register(0)] = 15.into())
            .masked(Mask::just_register(Register(0)));
        let bucket = b.get(&start);
        println!("Bucket: {bucket:#?}");
        let insts = bucket.get(&end);
        println!("Insts: {insts:#?}");
        assert!(insts.contains(&inst!(AddI, 0, 0, 14)));
    }
}
