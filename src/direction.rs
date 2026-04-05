#[derive(Clone, Copy, Debug, derive_more::Display, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    #[display("Forward")]
    Forward,
    #[display("Backward")]
    Backward,
}

impl Direction {
    pub const fn from_is_forward(is_forward: bool) -> Self {
        if is_forward {
            Self::Forward
        } else {
            Self::Backward
        }
    }
}
