use crate::all::All;
use crate::arm::state::{BitMask, Mask, Masked as MaskedState, State};
use crate::arm::{Inst, Register};
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph::Graph;
use crate::oracle::Oracle;
use crate::reduce_bit_width::Reducer;
use crate::tui::TuiHook;
use crate::word::prelude::*;
use functionality::{Mutate, Pipe};
use rustc_hash::FxHashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
struct Effect<W> {
    input: MaskedState<W>,
    output: MaskedState<W>,
}

#[derive(Clone, Debug, Default)]
enum Programs<W> {
    #[default]
    Empty,
    Inst(Inst<W>),
    /// This and that
    Extend(Rc<(Programs<W>, Programs<W>)>),
    /// This and then that
    Concat(Rc<(Programs<W>, Programs<W>)>),
}

impl<W: Clone> Programs<W> {
    pub fn concat(self, other: Self) -> Programs<W> {
        Programs::Concat(Rc::new((self, other)))
    }
}

impl<W> From<Inst<W>> for Programs<W> {
    fn from(i: Inst<W>) -> Programs<W> {
        Programs::Inst(i)
    }
}

impl<W: Clone + Debug + Eq + Hash> Extend<Programs<W>> for Programs<W> {
    fn extend<I: IntoIterator<Item = Programs<W>>>(&mut self, iter: I) {
        for x in iter {
            *self = Programs::Extend(Rc::new((self.clone(), x)));
        }
    }
}

impl<W: Clone> From<Programs<W>> for Vec<Vec<Inst<W>>> {
    fn from(p: Programs<W>) -> Self {
        match p {
            Programs::Empty => vec![],
            Programs::Inst(inst) => vec![vec![inst]],
            Programs::Extend(rc) => {
                rc.0.clone()
                    .pipe(Vec::from)
                    .mutate(|v| v.extend_from_slice(&rc.0.clone().pipe(Vec::from)))
            }
            Programs::Concat(rc) => {
                let mut ret = vec![];
                for y in rc.1.clone().pipe(Vec::from) {
                    for x in rc.0.clone().pipe(Vec::from) {
                        ret.push(x.mutate(|v| v.extend_from_slice(&y)));
                    }
                }
                ret
            }
        }
    }
}

type Bank<W> = crate::bank::Bank<Effect<W>, Programs<W>>;

// =================== Main Algorithm ===================================================

const DEBUG: bool = true;

pub fn optimize<WBig: Word, W: Word>(
    program: &[Inst<WBig>],
    examples: &[&[(Register, WBig)]],
    tui: &impl for<'g> TuiHook<&'g Graph<State<W>, crate::programs::Programs<Inst<W>>>, State<W>>,
) -> Option<Vec<Inst<W>>>
where
    <W as All>::Iter: Clone,
{
    let mut bank = Bank::<W>::default();
    let mut counter_examples = vec![];

    let mut reducer = Reducer::default();
    let reduced_program = program.iter().map(|i| i.reduce(&mut reducer)).collect();
    let mut oracle = crate::oracle::SmtOracle::new(reduced_program);

    // Check the empty program. Obviously, this probably fails, but a. It might not, and b. We need
    // it to generate the first counter example.
    let initial_state: State<W> = match oracle.check_program(&[]) {
        Ok(()) => return Some(vec![]),
        Err(ce) => {
            counter_examples.push(ce);
            ce.0
        }
    };

    init_bank(&mut bank, initial_state.into());

    let mut outputs_seen = FxHashSet::default();
    let mut to_init = vec![];
    let mut iters_left = 10;
    loop {
        if DEBUG {
            dbg!(iters_left);
        }
        // Our F, for now, is concatenation.
        for (a, b) in collect_children(&bank) {
            let x = eval((a.0, a.1.clone()), (b.0, b.1.clone()));
            // did we win?
            if x.0.output.state().registers[1] == 1.into()
                && x.0.output.state().registers[2] == 2.into()
                && x.0.output.state().registers[3] == 3.into()
            {
                return Some(x.1.pipe(Vec::from)[0].clone());
            }
            if false {
            // Check if we have a sub-masked-state that is not in the bank yet!
            if !outputs_seen.contains(x.0.output.state()) {
                outputs_seen.insert(*x.0.output.state());
                dbg!(outputs_seen.len());
                to_init.push(x.0.output);
            }
            }
        }
        if DEBUG {
            println!("done");
        }
        for s in to_init.drain(..) {
            init_bank(&mut bank, s);
        }
        iters_left -= 1;
        if iters_left == 0 {
            break;
        }
    }

    println!("Reached end!");

    println!("{}", bank.n_effects());
    None
}

// ======== Collect Children ======================================================================

/// In contrast to Sobeq, we only care about a single constructor - concatenation. It has 2
/// arguments, which makes implementation and reasoning easier for us.
fn collect_children<'a, W: Word>(bank: &'a Bank<W>) -> CollectChildrenIter<'a, W> {
    CollectChildrenIter {
        bank,
        iters: (bank.iter().fuse().peekable(), bank.iter()),
    }
}

use std::iter::{Fuse, Peekable};
struct CollectChildrenIter<'a, W> {
    bank: &'a Bank<W>,
    iters: (
        Peekable<Fuse<crate::bank::Iter<'a, Effect<W>, Programs<W>>>>,
        crate::bank::Iter<'a, Effect<W>, Programs<W>>,
    ),
}

impl<'a, W: Word> Iterator for CollectChildrenIter<'a, W> {
    type Item = (
        (&'a Effect<W>, &'a Programs<W>),
        (&'a Effect<W>, &'a Programs<W>),
    );
    fn next(&mut self) -> Option<Self::Item> {
        // Check if the second iterator is done,
        let Some(b) = self.iters.1.next() else {
            self.iters.1 = self.bank.iter();
            self.iters.0.next();
            if self.iters.0.len() % 100 == 0 {
            dbg!(self.iters.0.len());
            }
            return self.next();
        };
        // then check the first!
        // Notice that if the first iterator is done, we
        let a = *self.iters.0.peek()?;
        // Check frame rule!
        // This marks the state that appears in the output of the first program and the input of
        // the first.
        let conflict_mask = Mask::from(a.0.output.mask() & b.0.input.mask());
        if a.0.output.state().mask(conflict_mask) != b.0.input.state().mask(conflict_mask) {
            return self.next();
        }
        // Frame rule succeeds!
        Some((a, b))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.iters.0.len() * self.iters.1.len()))
    }
}

// =============== Eval ===========================================================================

/// This [eval] is the 'Eval' from Sobeq, which takes children and a constructor and combined them
/// together. Here the constructor is always concatenation.
fn eval<W: Word>(
    a: (&Effect<W>, Programs<W>),
    b: (&Effect<W>, Programs<W>),
) -> (Effect<W>, Programs<W>) {
    let conflict_mask = a.0.output.mask() & b.0.input.mask();
    // We need to mask the second's inputs which are fed by the outputs of the first.
    let input = a.0.input | (b.0.input & (!conflict_mask).into());
    let output = b.0.output | a.0.output;
    let prog = a.1.concat(b.1);
    (Effect { input, output }, prog)
}

// =============== Init Bank ======================================================================

fn init_bank<W: Word>(bank: &mut Bank<W>, state: MaskedState<W>) {
    // Initial the bank with our atomic single-instruction programs, on the initial and final
    // states of our lonely counter-example.
    let ei = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&[Register(0)/*, Register(1) */]),
        immediates: EnumerationInfoOptions::<W>::Unlimited,
    };
    let n = Enumerator::new().into_iter(&ei).count();
    let mut i = 0;
    for inst in Enumerator::new().into_iter(&ei) {
        let unmasked_input = *state.state();
        let unmasked_output = (*state.state()).mutate(|s| inst.run(s));
        let change = unmasked_output.diff(&unmasked_input).into_bit_mask();
        let read_mask = inst.read_mask().into_bit_mask();
        let (input_mask, output_mask) = (read_mask, read_mask | change);
        // Now we just want to mask the input, mask the output and add to the bank right? Wrong. We
        // actually want to add to the masks. This has to do with how we aren't adding empty
        // programs to the bank. In the original algorithm, Sobeq, you do add the equivalent of
        // empty programs - programs which just return the value of a variable! Again, in our
        // domain, the equivalent for that is empty programs that have a single register
        // pre&post-condition (example, {r0=5}ε{r0=5}). As an optimization we leave those out, and
        // to make up for that, we need to include variations of the post-conditions with these
        // registers that aren't actually used in the instruction. Wall of text out.
        for mask in BitMask::all() {
            // TODO: instead of skipping, just iterate only those we need.
            if mask.into_mask().registers().count() > 2 {
                continue;
            }
            let (input_mask, output_mask) = (input_mask | mask, output_mask | mask);
            let input = MaskedState::from(unmasked_input) & input_mask.into();
            let output = MaskedState::from(unmasked_output) & output_mask.into();
            let effect = Effect { input, output };
            let programs = Programs::from(inst);
            bank.insert(effect, programs);
        }
        i += 1;
        println!("{i}/{n}");
    }
}
