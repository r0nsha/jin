#[derive(Debug, PartialOrd, Ord, Clone, Copy, Default)]
pub struct Once<T>(Option<T>);

impl<T> Once<T> {
    pub fn new() -> Self {
        Self(None)
    }

    #[inline]
    pub fn get(self) -> Option<T> {
        self.0
    }

    /// # Panics
    /// If the value is None
    #[inline]
    #[track_caller]
    pub fn unwrap(self) -> T {
        self.0.unwrap()
    }

    /// # Panics
    /// If the value is already set
    #[inline]
    #[track_caller]
    pub fn set(&mut self, value: T) {
        assert!(self.0.is_none(), "already set");
        self.0 = Some(value);
    }
}

impl<T: PartialEq> PartialEq<T> for Once<T> {
    fn eq(&self, other: &T) -> bool {
        self.0.as_ref() == Some(other)
    }
}

impl<T: PartialEq> PartialEq for Once<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq> Eq for Once<T> {}
