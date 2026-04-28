use crate::arm::state::BitMask;
use crate::arm::{Inst, enumerate::EnumerationInfo};
use crate::word::prelude::*;
use functionality::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path::Path;

#[derive(Debug, Serialize, Deserialize)]
pub struct InstInputTable<W: Word, WShift: Word = BitWord<W>> {
    map: FxHashMap<BitMask, FxHashSet<Inst<W, WShift>>>,
}

#[derive(Debug, Default)]
pub struct InstInputTableParams {
    pub verbose: bool,
}

impl<W: Word + HasBitWord<BitWord: DeserializeOwned> + DeserializeOwned> InstInputTable<W> {
    pub fn new(InstInputTableParams { verbose }: InstInputTableParams) -> io::Result<Self> {
        let file_path = Path::new(".").join("inst_input_table.postcard");
        if file_path.exists() {
            let f = File::open(file_path)?;
            let reader = BufReader::new(&f);
            Self::load(reader)
        } else {
            if verbose {
                println!(
                    "Creating instruction input table at '{}'",
                    file_path.display()
                );
            }
            let this = Self::new_recalculate(verbose);
            if verbose {
                println!(
                    "Saving instruction input table to '{}'",
                    file_path.display()
                );
            }
            let mut writer = BufWriter::new(File::create(file_path)?);
            this.save(&mut writer)?;
            Write::flush(&mut writer)?;
            Ok(this)
        }
    }

    pub fn load(r: impl Read) -> io::Result<Self> {
        let mut buf = [0; 1024 * 1024];
        let (this, _) = postcard::from_io((r, &mut buf)).map_err(io::Error::other)?;
        Ok(this)
    }

    pub fn save(&self, w: impl Write) -> io::Result<()> {
        postcard::to_io(self, w).map_err(std::io::Error::other)?;
        Ok(())
    }

    pub fn new_recalculate(verbose: bool) -> Self {
        let mut ret = Self {
            map: FxHashMap::default(),
        };
        let enumeration_info = EnumerationInfo {
            include_nop: true,
            ..default()
        };
        let n_total = Inst::enumerate(enumeration_info).count();
        if verbose {
            println!("Recalculating instruction input table");
        }
        let delta_for_print = std::time::Duration::from_secs_f32(1. / 12.);
        let start_time = std::time::Instant::now();
        let mut last_print_time = start_time - delta_for_print * 2;
        for (i, inst) in Inst::enumerate(enumeration_info).enumerate() {
            // Printing
            let now = std::time::Instant::now();
            if verbose && now - last_print_time >= delta_for_print {
                last_print_time = now;
                let progress = (100 * i) / n_total;
                print!("\rProgress: {progress}%");
                if let Ok(estimated_time) = {
                    std::time::Duration::try_from_secs_f64(
                        (now - start_time).as_secs_f64() * ((n_total - i) as f64 / i as f64),
                    )
                } {
                    let estimated_time = humantime::Duration::from(estimated_time);
                    print!(" ET: {estimated_time}");
                    // Print some spaces to remove characters that were printed previously if the
                    // line was longer last time
                    print!("        ");
                }
                let _ = std::io::Write::flush(&mut std::io::stdout());
            }
            // Actual work
            let mask = inst.potential_input_mask().into_bit_mask();
            ret.map.entry(mask).or_default().insert(inst);
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arm::Register;
    use crate::arm::state::Mask;
    use crate::inst;

    #[test]
    #[ignore]
    fn test_inst_input_table() {
        let table = InstInputTable::<Word4>::new(InstInputTableParams { verbose: true }).unwrap();
        // Check that the table contains some expected entries.
        let mask =
            (Mask::just_register(Register(0)) | Mask::just_register(Register(2))).into_bit_mask();
        let insts = &table.map[&mask];
        dbg!(insts, mask);
        assert!(insts.contains(&inst!(Add, 0, 2, 0)));
        assert!(!insts.contains(&inst!(Add, 0, 0, 0)));
        let mask = Mask::just_register(Register(0)).into_bit_mask();
        let insts = &table.map[&mask];
        assert!(!insts.contains(&inst!(MovI, 0, 5)));
        assert!(
            insts.contains(&inst!(MovI Cc, 0, 5)) /* Conditional mov can have the output register be an input register when the condition is not met */
        );
    }
}
