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
