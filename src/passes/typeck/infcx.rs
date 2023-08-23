use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, SymbolId, TypeId},
    passes::typeck::constraint::{Constraint, Constraints},
    span::Span,
    ty::{InferType, IntVar, Type, TypeVar},
};

pub struct InferCtxt<'db> {
    pub db: &'db mut Db,
    pub ty_unification_table: InPlaceUnificationTable<TypeVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
    pub constraints: Constraints,
}

impl<'db> InferCtxt<'db> {
    pub fn new(db: &'db mut Db) -> Self {
        Self {
            db,
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
            constraints: Constraints::new(),
        }
    }

    pub fn lookup(&mut self, id: SymbolId) -> TypeId {
        let sym = &self.db[id];
        assert!(!sym.ty.is_null(), "symbol `{}` wasn't assigned a TypeId", sym.qpath);
        sym.ty
    }

    #[inline]
    pub fn fresh_ty_var(&mut self, span: Span) -> Type {
        Type::Infer(InferType::TypeVar(self.ty_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_ty_var(&mut self, span: Span) -> TypeId {
        let ftv = self.fresh_ty_var(span);
        self.db.alloc_ty(ftv)
    }

    #[inline]
    pub fn fresh_int_var(&mut self, span: Span) -> Type {
        Type::Infer(InferType::IntVar(self.int_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_int_var(&mut self, span: Span) -> TypeId {
        let ftv = self.fresh_int_var(span);
        self.db.alloc_ty(ftv)
    }

    #[inline]
    pub fn add_eq_constraint(&mut self, expected: TypeId, actual: TypeId) {
        self.constraints.push(Constraint::Eq { expected, actual });
    }
}
