use std::marker::PhantomData;

use derive_more::{From, Into};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into,
)]
pub struct Counter<T> {
    value: usize,
    marker: PhantomData<T>,
}

impl<T> Default for Counter<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Counter<T> {
    pub fn new() -> Self {
        Self { value: 0, marker: PhantomData }
    }
}

impl<T: From<usize>> Counter<T> {
    pub fn value(&self) -> T {
        self.value.into()
    }

    pub fn increment(&mut self) -> T {
        self.value += 1;
        self.value()
    }
}
