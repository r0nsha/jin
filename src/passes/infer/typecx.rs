use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::ty::{InferTy, IntVar, IntVarValue};
use crate::{
    span::Span,
    ty::{Ty, TyVar},
};

pub struct TypeCx {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl TypeCx {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }

    #[inline]
    pub fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::Infer(InferTy::TyVar(self.ty_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn fresh_int_var(&mut self, span: Span) -> Ty {
        Ty::Infer(InferTy::IntVar(self.int_unification_table.new_key(None)), span)
    }
}

impl UnifyKey for TyVar {
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

impl EqUnifyValue for Ty {}

impl UnifyKey for IntVar {
    type Value = Option<IntVarValue>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "IntTy"
    }
}

impl EqUnifyValue for IntVarValue {}
