use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, SymbolId},
    passes::typeck::constraint::{Constraint, Constraints},
    span::Span,
    ty::{InferType, IntVar, Type, TypeKind, TypeVar},
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

    pub fn lookup(&mut self, id: SymbolId) -> Type {
        let sym = &self.db[id];
        assert!(*sym.ty != TypeKind::Unknown, "symbol `{}` wasn't assigned a Type", sym.qpath);
        sym.ty
    }

    #[inline]
    pub fn fresh_ty_var(&mut self, span: Span) -> Type {
        Type::new(TypeKind::Infer(
            InferType::TypeVar(self.ty_unification_table.new_key(None)),
            span,
        ))
    }

    #[inline]
    pub fn fresh_int_var(&mut self, span: Span) -> Type {
        Type::new(TypeKind::Infer(
            InferType::IntVar(self.int_unification_table.new_key(None)),
            span,
        ))
    }

    #[inline]
    pub fn add_eq_constraint(&mut self, expected: Type, actual: Type) {
        self.constraints.push(Constraint::Eq { expected, actual });
    }
}
