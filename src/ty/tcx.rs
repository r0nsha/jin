use crate::{
    hir::HirMap,
    ty::{adjust::Adjustments, IntTy, Ty, TyKind},
};

#[derive(Debug)]
pub struct TyCtxt {
    pub types: CommonTypes,
    pub adjustments: HirMap<Adjustments>,
}

impl TyCtxt {
    pub fn new() -> Self {
        Self { types: CommonTypes::new(), adjustments: HirMap::new() }
    }
}

#[derive(Debug)]
pub struct CommonTypes {
    pub int: Ty,
    pub bool: Ty,
    pub unit: Ty,
    pub never: Ty,
    pub typ: Ty,
    pub unknown: Ty,
}

impl CommonTypes {
    pub fn new() -> Self {
        Self {
            int: Ty::new(TyKind::Int(IntTy::Int)),
            bool: Ty::new(TyKind::Bool),
            unit: Ty::new(TyKind::Unit),
            never: Ty::new(TyKind::Never),
            typ: Ty::new(TyKind::Type),
            unknown: Ty::new(TyKind::Unknown),
        }
    }
}
