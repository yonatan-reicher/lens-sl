/// Iterates over all permutations of items in the given collections.
///
//                                      Implementation Notes
// This is done by maintaining all iterators of the collections, and resetting them when they reach
// their end. When an iterator is reset, we move the next iterator forward. We also keep all items
// last yielded from each iterator. We keep it in a buffer so that we can return a slice to it.
pub struct Iter<'i, I>
where
    I: IntoIterator + Clone,
{
    collections: &'i [I],
    buf: Vec<I::Item>,
    iterators: Vec<I::IntoIter>,
}

impl<'i, I> Iter<'i, I>
where
    I: IntoIterator + Clone,
{
    pub const fn empty() -> Self {
        Self {
            collections: &[],
            buf: Vec::new(),
            iterators: Vec::new(),
        }
    }

    pub fn new(collections: &'i [I]) -> Self {
        Self {
            collections,
            buf: vec![],
            iterators: vec![],
        }
    }

    fn is_initialized(&self) -> bool {
        self.collections.len() == self.buf.len()
    }

    /// Called before the iterator yields for the first time.
    fn init(&mut self) -> bool {
        assert!(!self.collections.is_empty());
        assert!(self.buf.is_empty());
        assert!(self.iterators.is_empty());
        // Make all the iterators.
        let mut iterators: Vec<_> = self
            .collections
            .iter()
            .map(|c| c.clone().into_iter())
            .collect();
        // Then advance them all by one.
        let maybe_buf: Option<Vec<_>> = iterators.iter_mut().map(|it| it.next()).collect();
        // If even one is empty, no permutations.
        match maybe_buf {
            None => false,
            Some(buf) => {
                self.buf = buf;
                self.iterators = iterators;
                true
            }
        }
    }

    /// A version of `next` that does not allocate another buffer every call.
    pub fn next_slice<'a>(&'a mut self) -> Option<&'a [I::Item]> {
        let success = if self.is_initialized() {
            advance(&mut self.collections, &mut self.buf, &mut self.iterators)
        } else {
            self.init()
        };
        if success { Some(&self.buf) } else { None }
    }
}

fn advance<'a, 'i, I>(
    collections: &'i [I],
    buf: &'a mut [I::Item],
    iterators: &mut [I::IntoIter],
) -> bool
where
    I: IntoIterator + Clone,
{
    assert_eq!(collections.len(), buf.len());
    assert_eq!(collections.len(), iterators.len());
    match iterators {
        [] => false,
        [h, t @ ..] => {
            let x = buf.first_mut().unwrap();
            match h.next() {
                None => {
                    // Done! Reset this iterator, and advance the next one.
                    *h = collections.first().unwrap().clone().into_iter();
                    *x = h.next().unwrap();
                    advance(&collections[1..], &mut buf[1..], t)
                }
                Some(item) => {
                    *x = item;
                    true
                }
            }
        }
    }
}

impl<'i, I> Iterator for Iter<'i, I>
where
    I: IntoIterator + Clone,
    I::Item: Clone,
{
    type Item = Vec<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_slice().map(|s| s.to_vec())
    }
}
