use ena::unify::InPlaceUnificationTable;

use crate::{db::Database, span::Span, ty::*};

pub(crate) struct TypeCx {
    pub(crate) unification_table: InPlaceUnificationTable<TypeVar>,
}

impl TypeCx {
    pub(crate) fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub(crate) fn fresh_type_var(&mut self, db: &mut Database, span: Span) -> Type {
        Type::var(self.unification_table.new_key(None), span)
    }
}
