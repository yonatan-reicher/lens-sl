pub trait Len {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Implement for references
macro_rules! ref_impl {
    ($t:ty) => {
        impl<T: Len> Len for $t {
            #[rustfmt::skip]    fn len(&self) -> usize     { T::len(self) }
            #[rustfmt::skip]    fn is_empty(&self) -> bool { T::is_empty(self) }
        }
    };
}
ref_impl!(&T);
ref_impl!(&mut T);
ref_impl!(std::rc::Rc<T>);

// Implement for common types
macro_rules! common_impl {
    (<$($p:ident),*> $t:ty) => {
        #[rustfmt::skip]
        impl<$($p),*> Len for $t {
            fn len(&self) -> usize     { self.len() }
            fn is_empty(&self) -> bool { self.is_empty() }
        }
    };
    ($t:ty) => {
        #[rustfmt::skip]
        impl<T> Len for $t {
            fn len(&self) -> usize     { self.len() }
            fn is_empty(&self) -> bool { self.is_empty() }
        }
    };
}
common_impl!([T]);
common_impl!(Vec<T>);
common_impl!(<K, V, S> std::collections::HashMap<K, V, S>);
