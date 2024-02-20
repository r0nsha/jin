use std::marker::PhantomData;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Counter<T> {
    value: usize,
    marker: PhantomData<T>,
}

impl<T> Default for Counter<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> From<usize> for Counter<T> {
    fn from(value: usize) -> Self {
        Self { value, marker: PhantomData }
    }
}

impl<T> Counter<T> {
    #[must_use]
    pub fn new() -> Self {
        Self { value: 0, marker: PhantomData }
    }
}

impl<T: From<usize>> Counter<T> {
    #[must_use]
    pub fn value(&self) -> T {
        self.value.into()
    }

    pub fn increment(&mut self) -> T {
        self.increment_by(1)
    }

    pub fn increment_by(&mut self, by: usize) -> T {
        self.value += by;
        self.value()
    }
}

impl<T: core::fmt::Debug> core::fmt::Debug for Counter<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Counter({})", self.value)
    }
}
