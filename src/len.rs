pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Implement for references
use std::rc::Rc;
#[rustfmt::skip] impl<T: Len> Len for &T {     fn len(&self) -> usize{ T::len(self) } }
#[rustfmt::skip] impl<T: Len> Len for &mut T { fn len(&self) -> usize{ T::len(self) } }
#[rustfmt::skip] impl<T: Len> Len for Rc<T> { fn len(&self) -> usize{ T::len(self) } }

// Implement for common types
use rustc_hash::FxHashMap;
use std::collections::HashMap;
#[rustfmt::skip] impl<T> Len for [T] {                fn len(&self) -> usize { self.len() } }
#[rustfmt::skip] impl<T> Len for Vec<T> {             fn len(&self) -> usize { self.len() } }
#[rustfmt::skip] impl<K, V> Len for HashMap<K, V> {   fn len(&self) -> usize { self.len() } }
#[rustfmt::skip] impl<K, V> Len for FxHashMap<K, V> { fn len(&self) -> usize { self.len() } }
