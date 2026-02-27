use smtlib::lowlevel::ast::{Identifier, QualIdentifier, SpecConstant, Term};
use smtlib::lowlevel::lexicon::{Binary, Symbol};
use smtlib::terms::STerm;
use smtlib::{BitVec, Bool, Int, Sorted, Storage};

// Some helper functions

fn binary_to_u128<'st>(b: Binary<'st>) -> u128 {
    let s = b.0;
    debug_assert!(s.starts_with("#b"));
    u128::from_str_radix(&s[2..], 2).expect("Binary literals should be valid binary numbers")
}

fn spec_constant_to_u128(spec: &SpecConstant) -> Option<u128> {
    match spec {
        SpecConstant::Numeral(n) => Some(n.into_u128().unwrap()),
        SpecConstant::Binary(b) => Some(binary_to_u128(*b)),
        _ => None,
    }
}

fn term_to_i128(term: &Term) -> Option<i128> {
    match term {
        Term::SpecConstant(sc) => Some(spec_constant_to_u128(sc)? as i128),
        // For some reason, some binary numbers appear as identifiers
        Term::Identifier(QualIdentifier::Identifier(Identifier::Simple(sym)))
            if sym.0.starts_with("#b") =>
        {
            Some(binary_to_u128(Binary(sym.0)) as i128)
        }
        Term::Application(QualIdentifier::Identifier(Identifier::Simple(sym)), [arg])
            if sym.0 == "-" =>
        {
            let arg = term_to_i128(arg)?;
            Some(-arg)
        }
        _ => None,
    }
}

fn term_to_bool(term: &Term) -> Option<bool> {
    match term {
        Term::Identifier(QualIdentifier::Identifier(Identifier::Simple(Symbol("true")))) => {
            Some(true)
        }
        Term::Identifier(QualIdentifier::Identifier(Identifier::Simple(Symbol("false")))) => {
            Some(false)
        }
        _ => None,
    }
}

#[allow(dead_code)]
pub fn int_term_to_i128(int: Int<'_>) -> Option<i128> {
    let sterm = STerm::from(int);
    term_to_i128(sterm.term())
}

pub fn bool_term_to_bool(b: Bool<'_>) -> Option<bool> {
    let sterm = STerm::from(b);
    term_to_bool(sterm.term())
}

pub fn bit_vec_term_to_i128<const M: usize>(int: BitVec<'_, M>) -> Option<i128> {
    let sterm = STerm::from(int);
    term_to_i128(sterm.term())
}

// static things

std::thread_local! {
    static STATIC_STORAGE: Storage = Storage::new();
    static TRUE: Bool<'static> = {
        let st = static_storage();
        Bool::new(st, true)
    };
    static FALSE: Bool<'static> = {
        let st = static_storage();
        Bool::new(st, false)
    };
}

pub fn static_storage() -> &'static Storage {
    STATIC_STORAGE.with(|st| {
        // This is not enough, because now we have a reference that lasts only for the current
        // function. Let's extend it.
        // SAFETY: This is safe because we know it stays there for the rest of the program!
        unsafe {
            let ptr = st as *const _;
            &*ptr
        }
    })
}

pub fn static_true() -> Bool<'static> {
    TRUE.with(|x| *x)
}

pub fn static_false() -> Bool<'static> {
    FALSE.with(|x| *x)
}

// === Get Storage trait ===

pub trait GetStorage<'st> {
    fn st(&self) -> &'st Storage;
}

impl<'st, T: Sorted<'st>> GetStorage<'st> for T {
    fn st(&self) -> &'st Storage {
        self.st()
    }
}

// === Equality trait ===

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::{prop_assert_eq, property_test};
    use smtlib::{Real, Storage, lowlevel::lexicon::Symbol, prelude::*, terms::IntoWithStorage};

    #[property_test]
    // This test uses i64 input because `smtlib` can only convert i64 to `Int`, despite seeming to
    // support i128 terms and even more.
    fn test_int_term_to_i128_always_returns_same(i: i64) {
        let st = Storage::new();
        let int: Int = i.into_with_storage(&st);
        prop_assert_eq!(int_term_to_i128(int).unwrap() as i64, i);
    }

    #[test]
    fn test_int_term_to_i128_crashes_on_bad_input() {
        let st = Storage::new();
        let real: Real = 6942.into_with_storage(&st);
        let int = Int::from(STerm::from(real));
        assert_eq!(int_term_to_i128(int), None);
    }

    #[test]
    fn test_int_term_to_i128_cannot_handle_application() {
        let st = Storage::new();
        let t = Term::Application(
            QualIdentifier::Identifier(Identifier::Simple(Symbol("+"))),
            &[
                STerm::from(Int::new_const(&st, "x")).term(),
                STerm::from(Int::new_const(&st, "y")).term(),
            ],
        );
        let int = Int::from(STerm::new(&st, t));
        assert_eq!(int_term_to_i128(int), None);
    }

    #[property_test]
    fn test_bool_term_to_bool_always_returns_same(b: bool) {
        let st = Storage::new();
        let bool_term: Bool = b.into_with_storage(&st);
        dbg!(bool_term);
        prop_assert_eq!(bool_term_to_bool(bool_term), Some(b));
    }

    #[test]
    fn test_bool_term_to_bool_crashes_on_application() {
        let st = Storage::new();
        let a: Bool = true.into_with_storage(&st);
        let b: Bool = false.into_with_storage(&st);
        assert_eq!(bool_term_to_bool(a & b), None);
    }
}
