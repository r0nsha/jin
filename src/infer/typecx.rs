use ena::unify::InPlaceUnificationTable;

use crate::{
    db::{Database, TypeId},
    span::Span,
    ty::*,
};

pub(crate) struct TypeCx {
    unification_table: InPlaceUnificationTable<TypeVar>,
}

impl TypeCx {
    pub(crate) fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub(crate) fn fresh_type_var(&mut self, db: &mut Database, span: Span) -> TypeId {
        Type::alloc(
            db,
            TypeKind::Var(self.unification_table.new_key(None)),
            span,
        )
    }
}
