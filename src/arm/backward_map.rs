use super::state::{self, Flags, State};
use super::{
    ArgType, CondCode, EnumerationInfo, EnumerationInfoOptions, Inst, OpCode, RegArgType, Register,
};
use crate::all::All;
use crate::word::prelude::*;
use functionality::prelude::*;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

// A backwards map is a mapping between instructions and output states, to input states that go to
// that output state by that instruction. But in reality, it's more complicated than that. The
// actual information stored follows the following rules:
//
// ## Always Rule
// Instructions all have the 'always' condition.
//
// ## No Nop Rule
// Does not contain Nop instructions.
//
// ## Output Rule
// The output states only contain the instruction's input/output registers. It only contains the
// flags if the instruction reads or writes the flags.
//
// ## Input Rule
// The input states only contain the instruction's input registers. They only contain the flags if
// the state reads the flags.

/// A mapping between instructions and output states, to input states that go to that output states.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BackwardMap<W: Word, WShift: Word = BitWord<W>> {
    #[allow(clippy::type_complexity)]
    pub map: FxHashMap<(Inst<W, WShift>, State<W>), Inputs<W>>,
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

        let ei = EnumerationInfo {
            registers: EnumerationInfoOptions::Limited(registers),
            immediates: EnumerationInfoOptions::Unlimited,
            include_nop: false,
            skip_cond_code: true,
        };
        let n_total = Inst::enumerate(ei).count();

        println!("Recalculating backwards map for {registers:?}");
        let delta_for_print = std::time::Duration::from_secs_f32(1. / 12.);
        let start_time = std::time::Instant::now();
        let mut last_print_time = start_time - delta_for_print * 2;
        for (i, inst) in Inst::enumerate(ei).enumerate() {
            // Printing
            let now = std::time::Instant::now();
            if now - last_print_time >= delta_for_print {
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
            // Actual stuff
            let registers_in_inst = inst
                .args_with_types()
                .filter_map(|(a, t)| {
                    if t.is_reg() {
                        Some(Register::from(a))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            State::all_each(
                &EnumerationInfoOptions::Limited(&registers_in_inst),
                |input| {
                    let mut output = *input;
                    inst.run(&mut output);
                    Self::store(&mut ret, inst, output, *input);
                },
            );
        }
        println!();
        Self {
            map: ret,
            registers: registers.to_vec(),
        }
    }

    #[allow(clippy::type_complexity)]
    fn store(
        map: &mut FxHashMap<(Inst<W>, State<W>), Vec<State<W>>>,
        inst: Inst<W>,
        out: State<W>,
        inp: State<W>,
    ) {
        let inst = normalize_inst(inst);
        let out = normalize_output_state(&inst, out);
        let inp = normalize_input_state(&inst, &out, inp);
        let vec = map.entry((inst, out)).or_default();
        if vec.contains(&inp) {
            return;
        }
        vec.push(inp);
    }

    pub fn get(
        &self,
        inst: Inst<W>,
        out_orig: State<W>,
    ) -> impl Iterator<Item = State<W>> + use<W> {
        use itertools::Either;
        // Edge cases:
        // 1. Nop!
        if inst.op_code == OpCode::Nop {
            return Either::Left(vec![out_orig].into_iter());
        }
        // 2. Condition is false, and flags aren't affected. The instruction was just skipped.
        if !inst.affects_flags() && !inst.cond_code.check(out_orig.flags.into()) {
            return Either::Left(vec![out_orig].into_iter());
        }
        // 3. Condition is false (now), and the flags are affected. It could be both that the
        // condition was false and the instruction didn't run, and it could be that the condition
        // was true and the instruction did run, and the flags were just changed.
        if inst.affects_flags() && !inst.cond_code.check(out_orig.flags.into()) {
            return Either::Left(
                self.get(normalize_inst(inst), out_orig)
                    .collect::<Vec<_>>()
                    .mutate(|v| {
                        v.push(out_orig);
                    })
                    .into_iter(),
            );
        }
        let inst = normalize_inst(inst);
        let out = normalize_output_state(&inst, out_orig);
        Either::Right(
            self.map
                .get(&(inst, out))
                .cloned()
                .unwrap_or_default()
                .into_iter()
                .flat_map(move |inp| {
                    let inst = inst;
                    let out_orig = out_orig;
                    unnormalize_input_state(&inst, &out_orig, inp)
                }),
        )
    }
}

fn normalize_inst<W, WShift>(inst: Inst<W, WShift>) -> Inst<W, WShift> {
    Inst {
        cond_code: CondCode::Al,
        ..inst
    }
}

fn normalize_output_state<W: Copy + Default + Into<Register>, WShift: Copy>(
    inst: &Inst<W, WShift>,
    s: State<W>,
) -> State<W> {
    *s.masked(output_state_mask(inst)).state()
}

fn normalize_input_state<W: Copy + Default + Into<Register>, WShift: Copy>(
    inst: &Inst<W, WShift>,
    out: &State<W>,
    inp: State<W>,
) -> State<W> {
    *inp.masked(input_state_mask(inst, out)).state()
}

/// Get the mask for the output state, as written in the top of the file.
fn output_state_mask<W: Copy + Into<Register>, WShift>(inst: &Inst<W, WShift>) -> state::Mask {
    let registers = Register::ALL.map(|r| {
        inst.args_with_types()
            .any(|(a, t)| t.is_reg() && r == a.into())
    });
    let flags = inst.affects_flags() || inst.reads_flags();
    state::Mask { registers, flags }
}

/// Get the mask for the input states
fn input_state_mask<W: Copy + Into<Register>, WShift>(
    inst: &Inst<W, WShift>,
    _out: &State<W>,
) -> state::Mask {
    let registers = Register::ALL.map(|r| {
        inst.args_with_types().any(|(a, t)| {
            matches!(t, ArgType::Reg(RegArgType::Inp | RegArgType::InpOut)) && r == a.into()
        })
    });
    state::Mask {
        flags: inst.reads_flags(),
        registers,
    }
}

// Should this be unnormalize, or unormalize?
fn unnormalize_input_state<W: Word, WShift>(
    inst: &Inst<W, WShift>,
    original_output: &State<W>,
    inp: State<W>,
) -> impl Iterator<Item = State<W>> + use<W, WShift> {
    use itertools::{Either, Itertools};
    let out_mask = output_state_mask(inst);
    let inp_mask = input_state_mask(inst, original_output);
    // We have three kinds of interesting things. We have the inputs that appear in the input mask.
    // We have the outputs that appear in the output mask, but not in the input mask. And we have
    // those things that don't appear in both masks.
    // Things in the input mask are as they are. Things in the output mask are the most interesting:
    // they are overwritten and can have any possible value. Things which aren't in both masks
    // actually are not written and not read, so they should be as given by the original output.
    let only_in_output = out_mask & !inp_mask;
    let ret = *(inp.masked(inp_mask) | state::Masked::from(*original_output)).state();
    only_in_output
        .registers()
        .map(|r| W::all().map(move |w| (r, w)))
        .multi_cartesian_product()
        .flat_map(move |to_set| {
            let mut ret = ret;
            for (r, w) in to_set {
                ret[r] = w;
            }
            if only_in_output.flags {
                Either::Left(
                    Flags::ALL
                        .into_iter()
                        .map(move |f| ret.mutate(move |r| r.flags = f.into())),
                )
            } else {
                Either::Right(std::iter::once(ret))
            }
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arm::{Flags, FlagsBitField};
    use crate::inst;
    use functionality::Mutate;

    #[test]
    fn normalize_inst_example() {
        assert_eq!(
            normalize_inst::<Word4, Word2>(inst!(Add Cc, 0, 0, 1)),
            inst!(Add, 0, 0, 1),
        );
    }

    #[test]
    fn normalize_output_state_example() {
        let inst: Inst<Word4> = normalize_inst(inst!(Add Cc, 0, 0, 1));
        let state = State {
            flags: Flags {
                z: true,
                n: false,
                // Look, this actually makes the condition false
                c: true,
                v: false,
            }
            .into(),
            registers: [2.into(); _],
        };
        let state_normalized = normalize_output_state(&inst, state);
        assert_eq!(
            state_normalized,
            State::default().mutate(|s| {
                s[Register(0)] = 2.into();
                s[Register(1)] = 2.into();
            }),
        );
    }

    #[test]
    fn normalize_input_state_example() {
        let inst: Inst<Word4> = normalize_inst(inst!(Add Cc, 0, 1, 2));
        let output = State {
            flags: Flags {
                z: true,
                n: false,
                c: true,
                v: false,
            }
            .into(),
            registers: [2.into(); _],
        };
        let input = normalize_output_state(
            &inst,
            State {
                flags: Flags {
                    z: true,
                    n: false,
                    c: true,
                    v: false,
                }
                .into(),
                registers: [3.into(); _],
            },
        );
        let input_normalized = normalize_input_state(&inst, &output, input);
        assert_eq!(
            input_normalized,
            State::default().mutate(|s| {
                // These were input registers
                s[Register(1)] = 3.into();
                s[Register(2)] = 3.into();
                // Register 0 was not!
            }),
        );
    }

    #[test]
    fn normalize_input_state_inst_that_reads_flags() {
        let inst: Inst<Word32> = inst!(Add, 0, 1, 2; shift Rrx);
        let out = State::default();
        let inp_orig = State::default().mutate(|s| s.flags |= FlagsBitField::C);
        let inp = normalize_input_state(&inst, &out, inp_orig);
        assert_eq!(inp_orig, inp);
    }

    #[test]
    #[ignore]
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
    #[ignore]
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
    #[ignore]
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
    #[ignore]
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
            ..EnumerationInfo::default()
        };
        for inst in Inst::enumerate(ei) {
            let x = bm.get(inst, state).collect::<Vec<_>>();
            println!("Instruction: {inst}, Output State: {state}");
            println!("Input States: {x:?}");
            if !x.is_empty() {
                println!("Found non-empty input states for this instruction and output state!");
                return;
            }
        }
        panic!("No instruction produced non-empty input states for the given output state!");
    }

    #[test]
    #[ignore]
    fn no_missing() {
        type W = Word4;
        let bm = BackwardMap::<W>::new(&[Register(0), Register(1)]).unwrap();
        let flush = || {
            let _ = std::io::Write::flush(&mut std::io::stdout());
        };

        let ei = EnumerationInfo {
            registers: EnumerationInfoOptions::Limited(&[Register(0), Register(1)]),
            immediates: EnumerationInfoOptions::Limited(&[
                0.into(),
                1.into(),
                2.into(),
                3.into(),
                14.into(),
                15.into(),
            ]),
            ..EnumerationInfo::default()
        };
        let total = Inst::enumerate(ei).count();
        let mut width = 7;
        for (i, inst) in Inst::enumerate(ei).enumerate() {
            let inst_str = format!("{inst}");
            width = width.max(inst_str.len());
            print!("\rInst {inst_str:width$} [{} / {total}]:", i + 1);
            flush();
            State::all_each(&ei.registers, |inp| {
                let out = (*inp).mutate(|i| inst.run(i));
                let success = bm.get(inst, out).any(|i| i == *inp);
                if !success {
                    println!("Failed!!");
                    print!("  inp {inp}");
                    print!("  out {out}:");
                    bm.get(inst, out).for_each(|s| print!("  {s}"));
                    flush();
                    panic!();
                }
            });
        }
    }
}
