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

// macro_rules! new_key_type {
//     ($name: ident) => {
//         #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
//         pub struct $name(usize);
//
//         impl From<usize> for $name {
//             #[inline]
//             fn from(value: usize) -> Self {
//                 Self(value)
//             }
//         }
//
//         impl From<$name> for usize {
//             #[inline]
//             fn from(value: $name) -> Self {
//                 value.0
//             }
//         }
//
//         impl $name {
//             pub const INVALID: Self = Self(usize::MAX);
//
//             #[allow(unused)]
//             #[inline]
//             pub fn is_invalid(self) -> bool {
//                 self == Self::INVALID
//             }
//         }
//
//         impl std::fmt::Display for $name {
//             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//                 if self.is_invalid() {
//                     f.write_str("INVALID")
//                 } else {
//                     f.write_str(&self.0.to_string())
//                 }
//             }
//         }
//     };
// }
//

macro_rules! new_key_type {
    ( $(#[$outer:meta])* $vis:vis struct $name:ident; $($rest:tt)* ) => {
        $(#[$outer])*
        #[derive(Copy, Clone, Default,
                 Eq, PartialEq, Ord, PartialOrd,
                 Hash, Debug)]
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

        impl $name {
            pub const INVALID: Self = Self(usize::MAX);

            pub fn is_invalid(self) -> bool {
                self == Self::INVALID
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.is_invalid() {
                    f.write_str("INVALID")
                } else {
                    f.write_str(&self.0.to_string())
                }
            }
        }

        $crate::data_structures::index_vec::new_key_type!($($rest)*);
    };

    () => {}
}

pub(crate) use new_key_type;
