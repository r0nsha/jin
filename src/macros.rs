macro_rules! create_bool_enum {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum $name {
            Yes,
            No,
        }

        impl From<bool> for $name {
            fn from(value: bool) -> Self {
                if value {
                    Self::Yes
                } else {
                    Self::No
                }
            }
        }

        impl From<$name> for bool {
            fn from(value: $name) -> Self {
                matches!(value, $name::Yes)
            }
        }

        impl std::ops::Not for $name {
            type Output = bool;

            fn not(self) -> Self::Output {
                !bool::from(self)
            }
        }
    };
}

pub(crate) use create_bool_enum;
