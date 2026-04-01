use super::{EnumerationInfo, EnumerationInfoOptions, Inst, Register, State};
use crate::all::All;
use crate::word::prelude::*;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

/// A hash-map between instructions and output states to input states that send to the output. The
/// states use a liveness mask to mark which registers are ignored, and they all ignore the
/// condition flag. Currently, states also don't represent memory, so we don't need to worry about
/// that.
///
/// About the condition flag again: all instructions in the map have a condition flag of always.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BackwardMap<W: Word, WShift: Word = BitWord<W>> {
    #[allow(clippy::type_complexity)]
    pub map: FxHashMap<(Inst<W, WShift>, State<W>), Inputs<W>>,
    empty_vec: Vec<State<W>>,
    // The registers to consider when indexing into the map. These are the registers that are
    // "live" in the states, and other registers should be ignored.
    registers: Vec<Register>,
}
pub type Inputs<W> = Vec<State<W>>;

impl<W: Word + HasBitWord> BackwardMap<W, BitWord<W>> {
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
            for inst in Inst::enumerate(ei) {
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

impl<W: Word, WShift: Word> std::ops::Index<(Inst<W, WShift>, State<W>)>
    for BackwardMap<W, WShift>
{
    type Output = [State<W>];

    fn index(&self, (inst, mut state): (Inst<W, WShift>, State<W>)) -> &Self::Output {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Flags, inst};

    #[test]
    fn test_backward_map_some_not_empty() {
        type W = Word4;
        let bm = BackwardMap::<W, BitWord<W>>::new_recalculate(&[Register(1)]);
        assert!(bm.map.iter().any(|((inst, state), inputs)| {
            println!("Instruction: {inst}, Output State: {state}");
            println!("Input States:");
            inputs.iter().for_each(|input| print!("  {input}"));
            println!();
            !inputs.is_empty()
        }));
    }

    #[test]
    fn test_backward_map_some_has_more_than_4_inputs() {
        type W = Word4;
        let bm = BackwardMap::<W>::new_recalculate(&[Register(1)]);
        assert!(bm.map.iter().any(|((inst, state), inputs)| {
            println!("Instruction: {inst}, Output State: {state}");
            println!("Input States:");
            inputs.iter().for_each(|input| print!("  {input}"));
            println!();
            inputs.len() > 4
        }));
    }

    #[test]
    fn test_backward_map() {
        type W = Word4;
        let bm = BackwardMap::<W>::new_recalculate(&[Register(1)]);
        let inst: Inst<W, BitWord<W>> = inst!(MovI, 1, 12);
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
        let inst: Inst<W, BitWord<W>> = inst!(Nop,);
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
