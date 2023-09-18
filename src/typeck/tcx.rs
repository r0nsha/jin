use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, DefId, ModuleId},
    ty::{InferTy, IntVar, Ty, TyKind, TyVar},
    typeck::normalize::NormalizeTy,
};

#[derive(Debug)]
pub struct TyCtxt<'db> {
    pub db: &'db mut Db,
    pub storage: RefCell<TyStorage>,
}

impl<'db> TyCtxt<'db> {
    pub fn new(db: &'db mut Db) -> Self {
        Self { db, storage: RefCell::new(TyStorage::new()) }
    }

    pub fn lookup(&self, id: DefId) -> Ty {
        let def = &self.db[id];
        assert!(
            *def.ty != TyKind::Unknown,
            "definition `{}` ({}) wasn't assigned a Type",
            def.qpath,
            def.id
        );
        def.ty
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::TyVar(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty_unification_table.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::IntVar(
            self.storage.borrow_mut().int_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn normalize(&self, ty: Ty) -> Ty {
        ty.normalize(&mut self.storage.borrow_mut())
    }
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    module_id: ModuleId,
    fn_id: Option<DefId>,
}

impl Env {
    pub fn new(module_id: ModuleId, fn_id: Option<DefId>) -> Self {
        Self { module_id, fn_id }
    }

    #[inline]
    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }

    #[inline]
    pub fn fn_id(&self) -> Option<DefId> {
        self.fn_id
    }
}
