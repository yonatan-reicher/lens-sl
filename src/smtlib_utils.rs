use smtlib::{
    Int,
    lowlevel::ast::{Identifier, QualIdentifier, SpecConstant, Term},
    terms::STerm,
};

fn spec_constant_to_u128(spec: &SpecConstant) -> u128 {
    match spec {
        SpecConstant::Numeral(n) => n.into_u128().unwrap(),
        _ => panic!("unexpected non-numeral constant: {}", spec),
    }
}

fn term_to_i128(term: &Term) -> i128 {
    match term {
        Term::SpecConstant(sc) => spec_constant_to_u128(sc) as i128,
        Term::Application(QualIdentifier::Identifier(Identifier::Simple(sym)), [arg])
            if sym.0 == "-" =>
        {
            let arg = term_to_i128(arg);
            -arg
        }
        _ => panic!("cannot turn term to i128: {}", term),
    }
}

pub fn int_term_to_i128(int: Int<'_>) -> i128 {
    let sterm = STerm::from(int);
    term_to_i128(sterm.term())
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::property_test;
    use smtlib::{Real, Storage, lowlevel::lexicon::Symbol, prelude::*, terms::IntoWithStorage};

    #[property_test]
    // This test uses i64 input because `smtlib` can only convert i64 to `Int`, despite seeming to
    // support i128 terms and even more.
    fn test_int_term_to_i128_always_returns_same(i: i64) {
        let st = Storage::new();
        let int: Int = i.into_with_storage(&st);
        assert_eq!(int_term_to_i128(int) as i64, i);
    }

    #[test]
    #[should_panic]
    fn test_int_term_to_i128_crashes_on_bad_input() {
        let st = Storage::new();
        let real: Real = 6942.into_with_storage(&st);
        let int = Int::from(STerm::from(real));
        int_term_to_i128(int);
    }

    #[test]
    #[should_panic]
    fn test_int_term_to_i128_cannot_handle_application() {
        let st = Storage::new();
        let t = Term::Application(QualIdentifier::Identifier(Identifier::Simple(Symbol("+"))), &[
            STerm::from(Int::new_const(&st, "x")).term(),
            STerm::from(Int::new_const(&st, "y")).term(),
        ]);
        let int = Int::from(STerm::new(&st, t));
        int_term_to_i128(int);
    }
}
