use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Db, SymbolId},
    passes::typeck::{
        constraint::{Constraint, Constraints},
        unify::InferError,
    },
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

// pub struct At<'db, 'a> {
//     infcx: &'a mut InferCtxt<'db>,
//     a: Span,
//     b: Span,
//     a_is_expected: bool
// }
//
// impl At<'_, '_> {
//     pub fn eq(&mut self, a: Type, b: Type) -> Result<(), InferError> {
//         self.infcx.unify_ty_ty(a, b)
//     }
// }
//
// #[derive(Clone, Copy, Debug, PartialEq, Eq, TypeFoldable, TypeVisitable, Lift)]
// pub struct ExpectedFound<T> {
//     pub expected: T,
//     pub found: T,
// }
//
// impl<T> ExpectedFound<T> {
//     pub fn new(a_is_expected: bool, a: T, b: T) -> Self {
//         if a_is_expected {
//             ExpectedFound { expected: a, found: b }
//         } else {
//             ExpectedFound { expected: b, found: a }
//         }
//     }
// }
