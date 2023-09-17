use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, DefId},
    ty::{InferTy, IntVar, Ty, TyKind, TyVar},
    typeck::normalize::NormalizeTy,
};

#[derive(Debug)]
pub struct TyCtxt<'db> {
    pub db: &'db mut Db,
    pub storage: RefCell<TyCtxtStorage>,
}

impl<'db> TyCtxt<'db> {
    pub fn new(db: &'db mut Db) -> Self {
        Self { db, storage: RefCell::new(TyCtxtStorage::new()) }
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
pub struct TyCtxtStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl TyCtxtStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}

#[derive(Debug)]
pub struct Env {
    pub fn_id: Option<DefId>,
}

impl Env {
    pub fn new(fn_id: Option<DefId>) -> Self {
        Self { fn_id }
    }
}
