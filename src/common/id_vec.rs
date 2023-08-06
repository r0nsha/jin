use std::{marker::PhantomData, ops};

#[derive(Debug)]
pub(crate) struct IdVec<I: Id, T> {
    vec: Vec<T>,
    marker: PhantomData<I>,
}

impl<I: Id, T> IdVec<I, T> {
    pub(crate) fn new() -> Self {
        Self {
            vec: vec![],
            marker: PhantomData,
        }
    }

    pub(crate) fn push(&mut self, value: T) -> I {
        let id = self.next_id();
        self.vec.push(value);
        id
    }

    pub(crate) fn push_with_id(&mut self, f: impl FnOnce(I) -> T) -> I {
        let id = self.next_id();
        self.vec.push(f(id));
        id
    }

    #[inline]
    pub(crate) fn get(&self, id: I) -> Option<&T> {
        self.vec.get(id.into())
    }

    #[inline]
    pub(crate) fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.vec.get_mut(id.into())
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.vec.len()
    }

    #[inline]
    pub(crate) fn iter(&self) -> impl Iterator<Item = &T> {
        self.vec.iter()
    }

    #[inline]
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.vec.iter_mut()
    }

    #[inline]
    pub(crate) fn as_slice(&self) -> &[T] {
        &self.vec
    }

    #[inline]
    fn next_id(&self) -> I {
        self.vec.len().into()
    }
}

impl<I: Id, T> ops::Index<I> for IdVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.vec[index.into()]
    }
}

impl<I: Id, T> ops::IndexMut<I> for IdVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.vec[index.into()]
    }
}

pub(crate) trait Id: From<usize> + Into<usize> + Copy {}

macro_rules! new_id_type {
    ($name: ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub(crate) struct $name(usize);

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                Self(value)
            }
        }

        impl Into<usize> for $name {
            fn into(self) -> usize {
                self.0
            }
        }

        impl crate::common::Id for $name {}

        impl $name {
            #[allow(unused)]
            #[inline]
            pub fn null() -> Self {
                Self(usize::MAX)
            }

            #[allow(unused)]
            #[inline]
            pub fn is_null(&self) -> bool {
                self.0 == usize::MAX
            }
        }
    };
}

pub(crate) use new_id_type;
