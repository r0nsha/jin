use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, DefId},
    ty::{InferTy, IntVar, Ty, TyKind, TyVar},
};

pub struct InferCtxt<'db> {
    pub db: &'db mut Db,
    pub inner: RefCell<InferCtxtInner>,
}

impl<'db> InferCtxt<'db> {
    pub fn new(db: &'db mut Db) -> Self {
        Self { db, inner: RefCell::new(InferCtxtInner::new()) }
    }

    pub fn lookup(&self, id: DefId) -> Ty {
        let def = &self.db[id];
        assert!(*def.ty != TyKind::Unknown, "definition `{}` wasn't assigned a Type", def.qpath);
        def.ty
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::TyVar(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.inner.borrow_mut().ty_unification_table.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::IntVar(
            self.inner.borrow_mut().int_unification_table.new_key(None),
        )))
    }
}

pub struct InferCtxtInner {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl InferCtxtInner {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}
