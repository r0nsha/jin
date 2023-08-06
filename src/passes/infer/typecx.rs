use ena::unify::InPlaceUnificationTable;

use crate::{span::Span, ty::*};

pub(crate) struct TypeCx {
    pub(crate) unification_table: InPlaceUnificationTable<TypeVar>,
}

impl TypeCx {
    pub(crate) fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub(crate) fn fresh_type_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
    }
}
