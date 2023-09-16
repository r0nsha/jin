use std::{marker::PhantomData, ops};

#[derive(Debug, Clone)]
pub struct IndexVec<K: Key, V> {
    vec: Vec<V>,
    marker: PhantomData<K>,
}

impl<K: Key, V> Default for IndexVec<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Key, V> IndexVec<K, V> {
    pub fn new() -> Self {
        Self { vec: vec![], marker: PhantomData }
    }

    pub fn push(&mut self, value: V) -> K {
        let key = self.next_key();
        self.vec.push(value);
        key
    }

    pub fn push_with_key(&mut self, f: impl FnOnce(K) -> V) -> K {
        let key = self.next_key();
        self.vec.push(f(key));
        key
    }

    #[inline]
    pub fn get(&self, key: K) -> Option<&V> {
        self.vec.get(key.into())
    }

    #[inline]
    #[allow(unused)]
    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.vec.get_mut(key.into())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &V> {
        self.vec.iter()
    }

    #[inline]
    #[allow(unused)]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.vec.iter_mut()
    }

    #[inline]
    pub fn as_slice(&self) -> &[V] {
        &self.vec
    }

    #[inline]
    pub fn as_slice_mut(&mut self) -> &mut [V] {
        &mut self.vec
    }

    #[inline]
    pub fn next_key(&self) -> K {
        self.vec.len().into()
    }
}

impl<K: Key, T> ops::Index<K> for IndexVec<K, T> {
    type Output = T;

    fn index(&self, index: K) -> &Self::Output {
        &self.vec[index.into()]
    }
}

impl<K: Key, T> ops::IndexMut<K> for IndexVec<K, T> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.vec[index.into()]
    }
}

impl<K: Key, T> AsRef<[T]> for IndexVec<K, T> {
    fn as_ref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<K: Key, T> AsMut<[T]> for IndexVec<K, T> {
    fn as_mut(&mut self) -> &mut [T] {
        self.as_slice_mut()
    }
}

impl<K: Key, T> IntoIterator for IndexVec<K, T> {
    type Item = T;

    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'a, K: Key, T> IntoIterator for &'a IndexVec<K, T> {
    type Item = &'a T;

    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl<'a, K: Key, T> IntoIterator for &'a mut IndexVec<K, T> {
    type Item = &'a mut T;

    type IntoIter = std::slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter_mut()
    }
}

pub trait Key: From<usize> + Into<usize> + Copy {}

macro_rules! new_key_type {
    ($name: ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(usize);

        impl From<usize> for $name {
            #[inline]
            fn from(value: usize) -> Self {
                Self(value)
            }
        }

        impl From<$name> for usize {
            #[inline]
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl crate::common::Key for $name {}

        impl $name {
            pub const INVALID: Self = Self(usize::MAX);

            #[allow(unused)]
            #[inline]
            pub fn is_null(self) -> bool {
                self == Self::INVALID
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.is_null() {
                    f.write_str("INVALID")
                } else {
                    f.write_str(&self.0.to_string())
                }
            }
        }
    };
}

pub(crate) use new_key_type;
