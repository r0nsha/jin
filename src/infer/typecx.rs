use ena::unify::InPlaceUnificationTable;

use crate::{span::Span, ty::*};

pub(crate) struct TypeCx {
    unification_table: InPlaceUnificationTable<TypeVar>,
}

impl TypeCx {
    pub(crate) fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub(crate) fn fresh_type_var(&mut self, span: Span) -> Type {
        Type::var(self.unification_table.new_key(None), span)
    }
}
