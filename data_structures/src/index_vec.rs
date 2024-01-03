use typed_index_collections::{TiSlice, TiVec};

pub type IndexVec<K, V> = TiVec<K, V>;
pub type IndexSlice<K, V> = TiSlice<K, V>;

pub trait IndexVecExt<K, V> {
    fn push_with_key(&mut self, f: impl FnOnce(K) -> V) -> K;
}

impl<K: From<usize> + Copy, V> IndexVecExt<K, V> for TiVec<K, V> {
    fn push_with_key(&mut self, f: impl FnOnce(K) -> V) -> K {
        let key = self.next_key();
        self.push(f(key));
        key
    }
}

pub trait Key:
    From<usize>
    + Copy
    + Clone
    + Default
    + Eq
    + PartialEq
    + Ord
    + PartialOrd
    + core::hash::Hash
    + core::fmt::Debug
{
    fn data(self) -> usize;

    fn null() -> Self {
        usize::MAX.into()
    }

    fn is_null(self) -> bool {
        self == Self::null()
    }
}

#[macro_export(local_inner_macros)]
macro_rules! new_key_type {
    ( $(#[$outer:meta])* $vis:vis struct $name:ident; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Default,
                 Eq, PartialEq, Ord, PartialOrd,
                 Hash)]
        #[repr(transparent)]
        $vis struct $name(usize);

        impl From<usize> for $name {
            fn from(k: usize) -> Self {
                $name(k)
            }
        }

        impl From<$name> for usize {
            fn from(value: $name) -> Self {
                value.0
            }
        }

        impl $crate::index_vec::Key for $name {
            fn data(self) -> usize {
                self.0
            }
        }

        impl core::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use $crate::index_vec::Key as _;

                if self.is_null() {
                    f.write_str("NULL")
                } else {
                    f.write_str(&self.0.to_string())
                }
            }
        }

        impl core::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                core::write!(f, "{}({})", core::stringify!($name), self)
            }
        }

        $crate::new_key_type!($($rest)*);
    };

    () => {}
}
