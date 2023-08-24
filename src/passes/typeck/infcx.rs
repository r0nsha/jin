use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, SymbolId},
    ty::{tcx::TyCtxt, InferTy, IntVar, Ty, TyKind, TyVar},
};

pub struct InferCtxt<'db> {
    pub db: &'db mut Db,
    pub tcx: &'db TyCtxt,
    pub inner: RefCell<InferCtxtInner>,
}

impl<'db> InferCtxt<'db> {
    pub fn new(db: &'db mut Db, tcx: &'db TyCtxt) -> Self {
        Self { db, tcx, inner: RefCell::new(InferCtxtInner::new()) }
    }

    pub fn lookup(&mut self, id: SymbolId) -> Ty {
        let sym = &self.db[id];
        assert!(*sym.ty != TyKind::Unknown, "symbol `{}` wasn't assigned a Type", sym.qpath);
        sym.ty
    }

    #[inline]
    pub fn fresh_ty_var(&mut self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::TyVar(
            self.inner.borrow_mut().ty_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn fresh_int_var(&mut self) -> Ty {
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
