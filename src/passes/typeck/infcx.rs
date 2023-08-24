use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, SymbolId},
    ty::{tyctxt::TyCtxt, InferType, IntVar, Type, TypeKind, TypeVar},
};

pub struct InferCtxt<'db> {
    pub db: &'db mut Db,
    pub tcx: &'db TyCtxt,
    pub ty_unification_table: InPlaceUnificationTable<TypeVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl<'db> InferCtxt<'db> {
    pub fn new(db: &'db mut Db, tcx: &'db TyCtxt) -> Self {
        Self {
            db,
            tcx,
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub fn lookup(&mut self, id: SymbolId) -> Type {
        let sym = &self.db[id];
        assert!(*sym.ty != TypeKind::Unknown, "symbol `{}` wasn't assigned a Type", sym.qpath);
        sym.ty
    }

    #[inline]
    pub fn fresh_ty_var(&mut self) -> Type {
        Type::new(TypeKind::Infer(InferType::TypeVar(self.ty_unification_table.new_key(None))))
    }

    #[inline]
    pub fn fresh_int_var(&mut self) -> Type {
        Type::new(TypeKind::Infer(InferType::IntVar(self.int_unification_table.new_key(None))))
    }
}
