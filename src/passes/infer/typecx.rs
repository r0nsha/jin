use ena::unify::InPlaceUnificationTable;

use crate::{
    span::Span,
    ty::{Ty, TyVar},
};

pub struct TypeCx {
    pub unification_table: InPlaceUnificationTable<TyVar>,
}

impl TypeCx {
    pub fn new() -> Self {
        Self { unification_table: InPlaceUnificationTable::new() }
    }

    pub fn fresh_type_var(&mut self, span: Span) -> Ty {
        Ty::Var(self.unification_table.new_key(None), span)
    }
}
