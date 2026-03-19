use crate::all::All;
use crate::arm::state::{BitMask, Flags, FlagsBitField, Mask, Masked as MaskedState, State};
use crate::arm::{Inst, Register};
use crate::enumerate::{EnumerationInfo, EnumerationInfoOptions, Enumerator};
use crate::graph::Graph;
use crate::oracle::Oracle;
use crate::reduce_bit_width::Reducer;
use crate::tui::TuiHook;
use crate::word::prelude::*;
use functionality::{Mutate, Pipe};
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

type Bank<W> = crate::bank::Bank<Effect<W>, Programs<W>>;

// === Algorithm ===

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

    // Initial the bank with our atomic single-instruction programs, on the initial and final
    // states of our lonely counter-example.
    let ei = EnumerationInfo {
        registers: EnumerationInfoOptions::Limited(&[
            Register(0),
            // Register(1),
            // Register(2),
            // Register(3),
            // Register(4),
            // Register(5),
            // Register(6),
            // Register(7),
            // Register(8),
            // Register(9),
            // Register(10),
            // Register(11),
        ]),
        immediates: EnumerationInfoOptions::<W>::Unlimited,
    };
    let n = Enumerator::new().into_iter(&ei).count();
    let mut i = 0;
    for inst in Enumerator::new().into_iter(&ei) {
        let unmasked_input = initial_state;
        let unmasked_output = initial_state.mutate(|s| inst.run(s));
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
            if mask.into_mask().registers().count() > 4 {
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

    println!("{}", bank.n_effects());
    None
}

// ======== Collect Children ======================================================================

/// In contrast to Sobeq, we only care about a single constructor - concatenation. It has 2
/// arguments, which makes implementation and reasoning easier for us.
fn collect_children<'a, W: Word>(
    bank: &'a Bank<W>,
) -> impl Iterator<
    Item = (
        (&'a Effect<W>, &'a Programs<W>),
        (&'a Effect<W>, &'a Programs<W>),
    ),
> {
    I {
        bank,
        iters: (bank.iter().fuse().peekable(), bank.iter()),
    }
}

use std::iter::{Fuse, Peekable};
struct I<'a, W: Word> {
    bank: &'a Bank<W>,
    iters: (
        Peekable<Fuse<crate::bank::Iter<'a, Effect<W>, Programs<W>>>>,
        crate::bank::Iter<'a, Effect<W>, Programs<W>>,
    ),
}

impl<'a, W: Word> Iterator for I<'a, W> {
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
        let a = self.iters.0.peek()?;
        // Check frame rule!
        // This marks the state that appears in the output of the first program and the input of
        // the first.
        let conflict_mask = Mask::from(a.0.output.mask() & b.0.input.mask());
        if a.0.output.state().mask(conflict_mask) != b.0.input.state().mask(conflict_mask) {
            return self.next();
        }
        // Frame rule succeeds!
        Some((*a, b))
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
    let prog = a.1.mutate(|p| p.extend([b.1]));
    (Effect { input, output }, prog)
}
