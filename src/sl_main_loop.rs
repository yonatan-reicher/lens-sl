use crate::all::All;
use crate::arm::state::{BitMask, Flags, FlagsBitField, Mask, State};
use crate::arm::{Inst, Register};
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph::Graph;
use crate::programs::Programs;
use crate::tui::TuiHook;
use crate::word::prelude::*;
use functionality::Mutate;

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
struct Masked<W> {
    state: State<W>,
    mask: BitMask,
}

#[derive(Default)]
pub struct Bank<W> {
    vec: Vec<Entry<W>>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Entry<W> {
    input: Masked<W>,   // TODO be a vector
    prog: Vec<Inst<W>>, // A class
    output: Masked<W>,
}

impl<W, M: Into<BitMask>> From<(State<W>, M, Vec<Inst<W>>, State<W>, M)> for Entry<W> {
    fn from((i, i_m, p, o, o_m): (State<W>, M, Vec<Inst<W>>, State<W>, M)) -> Self {
        Entry {
            input: Masked {
                state: i,
                mask: i_m.into(),
            },
            prog: p,
            output: Masked {
                state: o,
                mask: o_m.into(),
            },
        }
    }
}

// === Algorithm ===

pub fn optimize<WBig: Word, W: Word>(
    program: &[Inst<W>],
    examples: &[&[(Register, WBig)]],
    tui: &impl for<'g> TuiHook<&'g Graph<State<W>, Programs<Inst<W>>>, State<W>>,
) -> Option<Vec<Inst<W>>>
where
    <W as All>::Iter: Clone,
{
    let mut bank = Bank::default();

    let initial_state = todo!();
    let ei = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&[
            Register(0),
            Register(1),
            Register(2),
            Register(3),
            Register(4),
            Register(5),
            Register(6),
            Register(7),
            Register(8),
            Register(9),
            Register(10),
            Register(11),
        ]),
        immediates: EnumerationInfoOptions::Unlimited,
    };
    for inst in Enumerator::new().into_iter(&ei) {
        let 
    }






    /*
    // Initialize with the empty program in all sorts of forms
    println!("Adding empty programs");
    let (s, m) = (State::default(), Mask::EMPTY);
    bank.vec.push((s, m, vec![], s, m).into());
    for f in Flags::ALL {
        let s = State::<W>::default().mutate(|s| s.flags = f.into());
        let m = Mask::JUST_FLAGS;
        bank.vec.push((s, m, vec![], s, m).into());
    }
    for r in Register::all() {
        for v in W::all() {
            let s = State::default().mutate(|s| s.set_register(r, v));
            let m = Mask::just_register(r);
            bank.vec.push((s, m, vec![], s, m).into());
        }
    }

    // Add single instruction programs println!("Single instruction programs");
    let ei = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&[
            Register(0),
            Register(1),
            Register(2),
            Register(3),
            Register(4),
            Register(5),
            Register(6),
            Register(7),
            Register(8),
            Register(9),
            Register(10),
            Register(11),
        ]),
        immediates: EnumerationInfoOptions::Unlimited,
    };
    for inst in Enumerator::new().into_iter(&ei) {
        let (i_m, o_m) = (inst.read_mask(), inst.read_mask() | inst.write_mask());
        let rs = Register::all().filter(|r| i_m[*r]).collect::<Box<[_]>>();
        let ei = EnumerationInfoOptions::Limited(&rs);
        State::all_each(&ei, |s| {
            let p = vec![inst];
            let i = *s;
            let mut o = *s;
            inst.run(&mut o);
            bank.vec.push((i, i_m, p, o, o_m).into());
        });
    }
    */

    // loop {
    //     let mut to_add = vec![];
    //     // Our F, for now, is concatenation.
    //     for (a, b) in collect_children(&bank) {
    //         let x = eval(a, b);
    //         // did we win?
    //         if x.output.state.registers[1] == 1.into() && x.output.state.registers[2] == 2.into() && x.output.state.registers[3] == 3.into() {
    //             return Some(x.prog);
    //         }
    //         // Check if there exists a more general EC.
    //         if bank.vec.iter().map(|e| e.input).all(|i| todo!()) {
    //             if x.input not in
    //         }
    //     }
    //     dbg!(to_add);
    //     break;
    // }

    println!("Reached end!");

    println!("{}", bank.vec.len());
    None
}

// TODO: Move to state.rs
use std::ops::{BitAnd, BitOr};
impl<W: Copy + Default> BitAnd<Mask> for Masked<W> {
    type Output = Self;
    fn bitand(self, mask: Mask) -> Self {
        Self {
            mask: self.mask & mask.into(),
            state: self.state.mask(mask),
            // mask mask mask mask...
        }
    }
}
impl<W: Copy + Default> BitOr for Masked<W> {
    type Output = Self;
    fn bitor(self, other: Self) -> Self {
        // self takes priority
        let mask = self.mask | other.mask;
        // Convert the bit-masks to real masks
        let (self_mask, other_mask) = (Mask::from(self.mask), Mask::from(other.mask));
        let state = State {
            flags: if self_mask.flags {
                self.state.flags
            } else if other_mask.flags {
                other.state.flags
            } else {
                FlagsBitField::default()
            },
            registers: Register::ALL.map(|r| {
                if self_mask[r] {
                    self.state.get_register(r)
                } else if other_mask[r] {
                    other.state.get_register(r)
                } else {
                    W::default()
                }
            }),
        };
        Masked { state, mask }
    }
}

// ======== Collect Children ======================================================================

/// In contrast to Sobeq, we only care about a single constructor - concatenation. It has 2
/// arguments, which makes implementation and reasoning easier for us.
fn collect_children<'a, W: Word>(
    bank: &'a Bank<W>,
) -> impl Iterator<Item = (&'a Entry<W>, &'a Entry<W>)> {
    I {
        bank,
        indices: (0, 0),
    }
}

struct I<'a, W: Word> {
    bank: &'a Bank<W>,
    indices: (usize, usize),
}

impl<'a, W: Word> Iterator for I<'a, W> {
    type Item = (&'a Entry<W>, &'a Entry<W>);
    fn next(&mut self) -> Option<Self::Item> {
        // Check if the second index is done
        if !(self.indices.1 < self.bank.vec.len()) {
            self.indices.1 = 0;
            self.indices.0 += 1;
            return self.next();
        }
        // Check if the first index is done
        if !(self.indices.0 < self.bank.vec.len()) {
            return None;
        }
        let a = &self.bank.vec[self.indices.0];
        let b = &self.bank.vec[self.indices.1];
        // Check frame rule!
        // This marks the state that appears in the output of the first program and the input of
        // the first.
        let conflict_mask = Mask::from(a.output.mask & b.input.mask);
        if a.output.state.mask(conflict_mask) != b.input.state.mask(conflict_mask) {
            self.indices.1 += 1;
            return self.next();
        }
        // Frame rule succeeds!
        self.indices.1 += 1;
        Some((a, b))
    }
}

// =============== Eval ===========================================================================

/// This [eval] is the 'Eval' from Sobeq, which takes children and a constructor and combined them
/// together. Here the constructor is always concatenation.
fn eval<W: Word>(a: &Entry<W>, b: &Entry<W>) -> Entry<W> {
    let conflict_mask = a.output.mask & b.input.mask;
    // We need to mask the second's inputs which are fed by the outputs of the first.
    let input = a.input | (b.input & (!conflict_mask).into());
    let output = b.output | a.output;
    Entry {
        input,
        prog: a.prog.clone().mutate(|v| v.extend_from_slice(&b.prog)),
        output,
    }
}
