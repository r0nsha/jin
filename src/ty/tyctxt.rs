use crate::ty::{IntType, Type, TypeKind};

#[derive(Debug)]
pub struct TyCtxt {
    pub types: CommonTypes,
}

impl TyCtxt {
    pub fn new() -> Self {
        Self { types: CommonTypes::new() }
    }
}

#[derive(Debug)]
pub struct CommonTypes {
    pub int: Type,
    pub bool: Type,
    pub unit: Type,
    pub never: Type,
    pub unknown: Type,
}

impl CommonTypes {
    pub fn new() -> Self {
        Self {
            int: Type::new(TypeKind::Int(IntType::Int)),
            bool: Type::new(TypeKind::Bool),
            unit: Type::new(TypeKind::Unit),
            never: Type::new(TypeKind::Never),
            unknown: Type::new(TypeKind::Unknown),
        }
    }
}
