pub mod ty {
    pub const I8: &str = "I8";
    pub const I16: &str = "I16";
    pub const I32: &str = "I32";
    pub const I64: &str = "I64";
    pub const INT: &str = "Int";

    pub const U8: &str = "U8";
    pub const U16: &str = "U16";
    pub const U32: &str = "U32";
    pub const U64: &str = "U64";
    pub const UINT: &str = "Uint";

    pub const F32: &str = "F32";
    pub const F64: &str = "F64";

    pub const STR: &str = "Str";
    pub const BOOL: &str = "Bool";
    pub const NEVER: &str = "Never";
}

pub mod field {
    pub const CAP: &str = "cap";
    pub const LEN: &str = "len";
    pub const PTR: &str = "ptr";
}
