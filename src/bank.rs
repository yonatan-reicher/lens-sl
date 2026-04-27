use crate::arm::enumerate::{EnumerationInfo, EnumerationInfoOptions};
use crate::arm::state;
use crate::arm::{Inst, Register};
use crate::word::BitWord;
use crate::word::prelude::*;
use functionality::prelude::*;
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::{Ref, RefCell};
use std::ops::Deref;

type Input<W> = state::Masked<W>;
type Output<W> = state::Masked<W>;

#[derive(Debug, Clone)]
pub struct Bank<'a, W, WShift = BitWord<W>> {
    inp_to_bucket: RefCell<FxHashMap<Input<W>, BucketId>>,
    bucket_arena: RefCell<Vec<Bucket<W>>>,
    insts_arena: RefCell<Vec<Insts<W, WShift>>>,
    ei: EnumerationInfo<'a, W>,
}

type BucketId = usize;

type InstsId = usize;

#[derive(Debug, Clone)]
struct Bucket<W>(FxHashMap<Output<W>, InstsId>);

pub type Insts<W, WShift = BitWord<W>> = FxHashSet<Inst<W, WShift>>;

#[derive(Debug, Clone)]
pub struct BucketRef<'a, W, WShift = BitWord<W>>(&'a Bank<'a, W, WShift>, BucketId);

#[derive(Debug, Clone)]
pub struct InstsRef<'a, W, WShift = BitWord<W>>(&'a Bank<'a, W, WShift>, InstsId);

impl<'a, W: Word + HasBitWord> Bank<'a, W> {
    pub fn new(ei: EnumerationInfo<'a, W>) -> Self {
        Bank {
            inp_to_bucket: default(),
            bucket_arena: default(),
            insts_arena: RefCell::new(vec![
                // The 0th set is reserved for "no instructions", 0 is like a null pointer.
                FxHashSet::default(),
            ]),
            ei,
        }
    }

    #[allow(dead_code)]
    /// The difference between this and [Self::get] is that this doesn't create the bucket if it
    /// doesn't exist.
    pub fn try_get(&'a self, input: &Input<W>) -> Option<BucketRef<'a, W>> {
        self.inp_to_bucket
            .borrow()
            .get(input)
            .copied()
            .map(|id| BucketRef(self, id))
    }

    pub fn get(&self, input: &Input<W>) -> BucketRef<'_, W> {
        let id = *self
            .inp_to_bucket
            .borrow_mut()
            .entry(*input)
            .or_insert_with(|| {
                let bucket = Bucket::new(input, self.ei, &mut self.insts_arena.borrow_mut());
                let bucket_arena = &mut self.bucket_arena.borrow_mut();
                let id = bucket_arena.len();
                bucket_arena.push(bucket);
                id
            });
        BucketRef(self, id)
    }
}

impl<W: Word + HasBitWord> Bucket<W> {
    fn new(input: &Input<W>, ei: EnumerationInfo<W>, insts_arena: &mut Vec<Insts<W>>) -> Self {
        // Initialize the bucket by running all instructions on the input and recording outputs.
        let registers = Register::all()
            .filter(|r| input.mask().into_mask()[*r] && ei.inp_registers.into_iter().contains(r))
            .collect::<Vec<_>>();
        Inst::enumerate(EnumerationInfo {
            // Limit only to registers relevant to the input.
            inp_registers: EnumerationInfoOptions::Limited(registers.as_slice()),
            ..ei
        })
        // Filter instructions further! Masks need to match exactly.
        .filter(|inst| {
            // assert!(
            //     inst.potential_input_mask()
            //         .is_sub_mask(&input.mask().into_mask().mutate(|m| m.flags = true /* Flags aren't pruned above */)),
            //     "Instruction {inst} has potential input mask {} which is not a sub-mask of input mask {}",
            //     inst.potential_input_mask(),
            //     input.mask().into_mask(),
            // );
            inst.potential_input_mask() == input.mask().into_mask()
        })
        // Run
        .map(|inst| {
            let mut output = *input.state();
            inst.run(&mut output);
            (output.masked(inst.potential_output_mask()), inst)
        })
        // Group by output.
        .fold(Bucket(FxHashMap::default()), |bucket, (output, inst)| {
            bucket.mutate(|bucket| {
                let id = *bucket.0.entry(output).or_insert_with(|| {
                    let id = insts_arena.len();
                    insts_arena.push(FxHashSet::default());
                    id
                });
                let insts = &mut insts_arena[id];
                insts.insert(inst);
            })
        })
    }

    fn contains_key(&self, output: &Output<W>) -> bool {
        self.0.contains_key(output)
    }
}

impl<'a, W: Word + HasBitWord> BucketRef<'a, W> {
    fn bucket(&self) -> Ref<'a, Bucket<W>> {
        Ref::map(self.0.bucket_arena.borrow(), |a| &a[self.1])
    }

    pub fn contains_key(&self, output: &Output<W>) -> bool {
        self.bucket().contains_key(output)
    }

    pub fn get(&self, output: &Output<W>) -> InstsRef<'a, W> {
        let bucket = self.bucket();
        let insts_id = bucket.0.get(output).copied().unwrap_or(0);
        InstsRef(self.0, insts_id)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Output<W>, InstsRef<'a, W>)> + 'a {
        use std::collections::hash_map::Iter as HMIter;
        struct I<'a, W: Word + HasBitWord> {
            bank: &'a Bank<'a, W>,
            _bucket: Ref<'a, Bucket<W>>,
            iter: HMIter<'a, Output<W>, InstsId>,
        }
        let bucket: Ref<'a, Bucket<W>> = self.bucket();
        // As long as we hold the Ref, references to the buckets are valid. That means we can safely
        // have a reference to the bucket in the iterator, as long as the iterator doesn't outlive
        // the Ref instance.
        let evil_reference: &'a Bucket<W> = unsafe { &*(bucket.deref() as *const _) };
        let iter = evil_reference.0.iter();
        return I {
            bank: self.0,
            _bucket: bucket,
            iter,
        };

        impl<'a, W: Word + HasBitWord> Iterator for I<'a, W> {
            type Item = (Output<W>, InstsRef<'a, W>);
            fn next(&mut self) -> Option<Self::Item> {
                let (output, insts_id) = self.iter.next()?;
                Some((*output, InstsRef(self.bank, *insts_id)))
            }
        }
    }
}

impl<'a, W: Word + HasBitWord> InstsRef<'a, W> {
    pub fn borrow(&self) -> Ref<'a, Insts<W>> {
        Ref::map(self.0.insts_arena.borrow(), |a| &a[self.1])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arm::state::{Mask, State};
    use crate::inst;
    use proptest::property_test;

    #[test]
    fn test_bank() {
        let b = Bank::<Word4>::new(EnumerationInfo {
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
        assert!(insts.borrow().contains(&inst!(AddI, 0, 0, 14)));
    }

    #[ignore]
    #[property_test]
    fn test_inst_always_in_effect_bucket(inst: Inst<Word4>, inp: State<Word4>) {
        let inp = inp.masked(inst.potential_input_mask());
        let out = (*inp.state())
            .mutate(|s| inst.run(s))
            .masked(inst.potential_output_mask());
        println!("Testing instruction {inst} on state {inp} (output {out})");
        let b = Bank::<Word4>::new(EnumerationInfo {
            inp_registers: EnumerationInfoOptions::Unlimited,
            out_registers: EnumerationInfoOptions::Unlimited,
            include_nop: true,
            ..default()
        });
        let bucket = b.get(&inp);
        let insts = bucket.get(&out);
        assert!(
            insts.borrow().contains(&inst),
            "Instruction {inst} should be in the bucket for input {inp} and output {out}, but it isn't. Bucket contents: {:?}",
            insts.borrow().iter().join(", ")
        );
    }
}
