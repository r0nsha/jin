use ena::unify::InPlaceUnificationTable;

use crate::common::{new_key_type, IndexVec};
use crate::{
    span::Span,
    ty::{Ty, TyVar},
};

pub struct TypeCx {
    pub types: IndexVec<TyId, Ty>,
    pub unification_table: InPlaceUnificationTable<TyVar>,
}

impl TypeCx {
    pub fn new() -> Self {
        Self {
            types: IndexVec::new(),
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
    }

    pub fn alloc_ty_var(&mut self, span: Span) -> TyId {
        let ty = self.fresh_ty_var(span);
        self.alloc_ty(ty)
    }

    pub fn alloc_ty(&mut self, ty: Ty) -> TyId {
        self.types.push(ty)
    }
}

new_key_type!(TyId);

#[allow(unused)]
impl std::ops::Index<TyId> for TypeCx {
    type Output = Ty;

    fn index(&self, index: TyId) -> &Self::Output {
        &self.types[index]
    }
}

#[allow(unused)]
impl std::ops::IndexMut<TyId> for TypeCx {
    fn index_mut(&mut self, index: TyId) -> &mut Self::Output {
        &mut self.types[index]
    }
}
