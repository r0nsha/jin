use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, DefId},
    hir::Fn,
    ty::{InferTy, IntVar, Ty, TyKind, TyVar},
};

#[derive(Debug)]
pub struct InferCtxt<'db> {
    pub db: &'db mut Db,
    pub storage: RefCell<InferCtxtStorage>,
    pub fx: Option<FnCtxt>,
}

impl<'db> InferCtxt<'db> {
    pub fn new(db: &'db mut Db) -> Self {
        Self { db, storage: RefCell::new(InferCtxtStorage::new()), fx: None }
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
}

#[derive(Debug)]
pub struct InferCtxtStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl InferCtxtStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}

#[derive(Debug)]
pub struct FnCtxt {
    pub id: DefId,
    pub ret_ty: Ty,
    // pub ty_params: Vec<ParamTy>,
}

impl FnCtxt {
    pub fn from_fn(db: &Db, fun: &Fn) -> Self {
        FnCtxt {
            id: fun.id,
            ret_ty: db[fun.id].ty.as_fn().unwrap().ret,
            // ty_params: fun
            //     .sig
            //     .ty_params
            //     .iter()
            //     .map(|tp| {
            //         db[tp.id]
            //             .kind
            //             .as_ty()
            //             .expect("to be a type")
            //             .as_param()
            //             .expect("to be a param type")
            //     })
            //     .cloned()
            //     .collect(),
        }
    }
}
