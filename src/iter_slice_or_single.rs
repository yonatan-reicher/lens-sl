// We need to implement an iterator that yields either from a slice or just a single value.
#[derive(Clone, Debug)]
pub enum Iter<'a, T> {
    Slice(&'a [T]),
    Single(T),
}
impl<'a, T: Clone + Copy> Iterator for Iter<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Iter::Slice([]) => None,
            Iter::Slice([h, t @ ..]) => {
                *self = Iter::Slice(t);
                Some(*h)
            }
            Iter::Single(x) => {
                let x = *x;
                *self = Iter::Slice(&[]);
                Some(x)
            }
        }
    }
}
