use ena::unify::InPlaceUnificationTable;

use crate::{span::Span, ty::*};

pub struct TypeContext {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
    }
}
