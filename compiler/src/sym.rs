pub mod ty {
    pub const I8: &str = "i8";
    pub const I16: &str = "i16";
    pub const I32: &str = "i32";
    pub const I64: &str = "i64";
    pub const INT: &str = "int";

    pub const U8: &str = "u8";
    pub const U16: &str = "u16";
    pub const U32: &str = "u32";
    pub const U64: &str = "u64";
    pub const UINT: &str = "uint";

    pub const F32: &str = "f32";
    pub const F64: &str = "f64";

    pub const STR: &str = "str";
    pub const BOOL: &str = "bool";
    pub const NEVER: &str = "never";
}

pub mod field {
    pub const DATA: &str = "data";
    pub const LEN: &str = "len";
    pub const CAP: &str = "cap";
}
