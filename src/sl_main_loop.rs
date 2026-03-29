use crate::all::All;
use crate::all_permutations::Iter as PermutationIter;
use crate::arm::state::{BitMask, Mask, Masked as MaskedState, State};
use crate::arm::{Inst, Register, extend_program_for_each};
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph::Graph;
use crate::oracle::{Oracle, SmtOracle};
use crate::reduce_bit_width::{ImmediateInfo, Reducer};
use crate::tui::TuiHook;
use crate::word::prelude::*;
use functionality::{Mutate, Pipe};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use std::fmt::Debug;
use std::hash::{BuildHasher, Hash, Hasher};
use std::ops::ControlFlow;
use std::rc::Rc;

#[derive(Debug, Default)]
// Note: the inputs should always have the same mask in all of them i think
struct Effect<W>(FxHashMap<MaskedState<W>, MaskedState<W>>);

impl<W> Effect<W> {
    pub fn inputs(&self) -> impl Iterator<Item = &MaskedState<W>> {
        self.0.iter().map(|x| x.0)
    }
    pub fn outputs(&self) -> impl Iterator<Item = &MaskedState<W>> {
        self.0.iter().map(|x| x.1)
    }
}

impl<W: Eq + Hash> PartialEq for Effect<W> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<W: Eq + Hash> Eq for Effect<W> {}

impl<W: Hash> Hash for Effect<W> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // We need to make sure we hash the elements in some sort of order. This makes sure the
        // order of the set's iterator does not matter.
        let mut hashes: Box<[_]> = self.0.iter().map(|x| FxBuildHasher.hash_one(x)).collect();
        hashes.sort();
        hashes.hash(state);
    }
}

impl<W: Eq + Hash> FromIterator<(MaskedState<W>, MaskedState<W>)> for Effect<W> {
    fn from_iter<T: IntoIterator<Item = (MaskedState<W>, MaskedState<W>)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
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
                    .mutate(|v| v.extend_from_slice(&rc.1.clone().pipe(Vec::from)))
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

type Program<W> = [Inst<W>];
type OwnedProgram<W> = Vec<Inst<W>>;

// =================== Main Algorithm ===================================================

#[derive(derive_more::Debug)]
struct Globals<'a, WBig, W> {
    // does not contain the bank because it needs it's borrow to be tracked independently.
    // bank: Bank<W>,
    counter_examples: Vec<(State<W>, State<W>)>,
    oracle: &'a mut dyn Oracle<Program<WBig>, State<WBig>>,
    oracle_reduced: &'a mut dyn Oracle<Program<W>, State<W>>,
    reducer: Reducer<WBig, W>,
    registers: Vec<Register>,
    immediates: Vec<W>,
}

const DEBUG: bool = true;

pub fn optimize<WBig: Word, W: Word>(
    program: &[Inst<WBig>],
    additional_registers: impl IntoIterator<Item = Register>,
    additional_immediates: impl IntoIterator<Item = WBig>,
    tui: &impl for<'g> TuiHook<&'g Graph<State<W>, crate::programs::Programs<Inst<W>>>, State<W>>,
) -> Option<Vec<Inst<WBig>>>
where
    <W as All>::Iter: Clone,
{
    // Reduce the program and the given immediates to the lower bit-width
    let mut reducer = Reducer::default();
    let reduced_program: Vec<_> = program.iter().map(|i| i.reduce(&mut reducer)).collect();
    for i in additional_immediates {
        reducer.reduce(i, &ImmediateInfo { is_shift: false });
    }
    // Collect all the registers and immediates that might be useful for synthesis.
    let immediates: Vec<W> = reducer.immediates().collect();
    let registers = crate::collect_registers::Collector::new()
        .mutate(|c| c.program(program))
        .pipe(|c| c.registers)
        .mutate(|v| v.extend(additional_registers));

    // Algorithm state
    let bank = &mut Bank::<W>::default();
    let g = &mut Globals {
        counter_examples: vec![],
        oracle: &mut SmtOracle::new(program.to_vec()),
        oracle_reduced: &mut SmtOracle::new(reduced_program),
        reducer,
        registers,
        immediates,
    };

    // Check the empty program. Obviously, this probably fails, but a. It might not, and b. We need
    // it to generate the first counter example.
    let initial_state: State<W> = match verify(&[], g) {
        Some(Some(p)) => return Some(p),
        Some(None) => panic!(),
        None => g.counter_examples[0].0,
    };

    init_bank(bank, initial_state.into(), g);

    let mut outputs_seen = FxHashSet::default();
    let mut to_init = vec![];
    let mut i = 0;
    loop {
        if DEBUG {
            i += 1;
            println!("=== Iteration {i} ===");
        }
        // Our F, for now, is concatenation.
        for (a, b) in collect_children(&bank) {
            let x = eval((a.0, a.1.clone()), (b.0, b.1.clone()));
            // did we win?
            if matches_all_counter_examples(&x.0, &g.counter_examples) {
                println!("all match!");
                let v = Vec::from(x.1);
                let l = v.len();
                dbg!(v.len());
                for (i, p) in v.into_iter().enumerate() {
                    println!("[{i}/{l}]");
                    match verify(&p, g) {
                        Some(Some(p)) => return Some(p),
                        Some(None) => (),
                        None => (),
                    }
                }
            }
            // Check if we have a sub-masked-state that is not in the bank yet!
            let outputs = x.0.outputs().map(|x| x.state()).collect::<Box<[_]>>();
            if !outputs_seen.contains(&outputs) {
                outputs_seen.insert(outputs);
                if DEBUG {
                    println!("Another one added! {}", outputs_seen.len());
                }
                to_init.push(x.0.outputs());
            }
        }
        if DEBUG {
            println!("Finished collecting children");
        }
        for s in to_init.drain(..) {
            init_bank(bank, s, g);
        }
        if i == 10 {
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
    let both = a.0.0.iter().zip(&b.0.0);
    let effect = both
        .map(|(&(a_input, a_output), &(b_input, b_output))| {
            let conflict_mask = a_output.mask() & b_input.mask();
            // We need to mask the second's inputs which are fed by the outputs of the first.
            let input = a_input | (b_input & (!conflict_mask).into());
            let output = b_output | a_output;
            (input, output)
        })
        .collect::<Vec<_>>()
        .pipe(Effect);
    let prog = a.1.concat(b.1);
    (effect, prog)
}

// =============== Init Bank ======================================================================

fn init_bank<WBig, W: Word>(bank: &mut Bank<W>, states: &[MaskedState<W>], g: &Globals<WBig, W>) {
    // Initial the bank with our atomic single-instruction programs, on the initial and final
    // states of our lonely counter-example.
    let ei = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&g.registers),
        immediates: EnumerationInfoOptions::<W>::Limited(&g.immediates),
    };
    let n = Enumerator::new().into_iter(&ei).count();
    let mut i = 0;
    for inst in Enumerator::new().into_iter(&ei) {
        let effects = states
            .iter()
            .map(|state| {
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
                BitMask::all().filter_map(|mask| {
                    // TODO: instead of skipping, just iterate only those we need.
                    if mask.into_mask().registers().count() > 2 {
                        return None;
                    }
                    let (input_mask, output_mask) = (input_mask | mask, output_mask | mask);
                    let input = MaskedState::from(unmasked_input) & input_mask.into();
                    let output = MaskedState::from(unmasked_output) & output_mask.into();
                    Some((input, output))
                })
            })
            .collect::<Box<[_]>>()
            .pipe(move |mut s| {
                PermutationIter::new(&mut s).into_iter().map(|x| x.into_iter().collect())
            });
        let programs = Programs::from(inst);
        effects.for_each(|effect| bank.insert(effect, programs));
        i += 1;
        println!("{i}/{n}");
    }
}

// =============== Verify ======================================================================

fn verify<WBig: Word, W: Word>(
    p: &Program<W>,
    g: &mut Globals<WBig, W>,
) -> Option<Option<OwnedProgram<WBig>>> {
    match g.oracle_reduced.check_program(p) {
        Err(ce) => {
            if !g.counter_examples.contains(&ce) {
                g.counter_examples.push(ce);
            }
            None
        }
        Ok(()) => {
            let ret = extend_program_for_each(p, &g.reducer, |p| match g.oracle.check_program(p) {
                Ok(()) => ControlFlow::Break(p.to_vec()),
                Err(ce) => {
                    dbg!(ce);
                    ControlFlow::Continue(())
                }
            });
            Some(match ret {
                ControlFlow::Continue(()) => None,
                ControlFlow::Break(p) => Some(p),
            })
        }
    }
}

// ================ Build ======================================================

fn build<WBig, W>(
    effect: Effect<W>,
    prog: Programs<W>,
    bank: &mut Bank<W>,
    g: &mut Globals<WBig, W>,
) {
    if effect.0.len() < g.counter_examples.len() {
        todo!()
    }
}

// ================ Other ======================================================

fn matches_all_counter_examples<'a, W: Word>(
    effect: &Effect<W>,
    counter_examples: impl IntoIterator<Item = &'a (State<W>, State<W>)>,
) -> bool {
    // For now, an effect has only a singe input-output thingy. In just a bitty, we'll have
    // more.
    let pairs = &[(effect.input, effect.output)];
    for ((i, o), (i_ce, o_ce)) in pairs.iter().zip(counter_examples) {
        let fine = MaskedState::from(*i_ce) & i.mask().into() == *i
            && MaskedState::from(*o_ce) & o.mask().into() == *o;
        if !fine {
            return false;
        }
    }
    true
}
