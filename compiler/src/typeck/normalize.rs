use std::cell::RefMut;

use crate::{
    ty::{fold::TyFolder, InferTy, Ty, TyKind},
    typeck::{TyStorage, Typeck},
};

impl Typeck<'_> {
    #[inline]
    pub(super) fn normalize(&self, ty: Ty) -> Ty {
        Normalize::from(self).fold(ty)
    }
}

pub struct Normalize<'a> {
    storage: RefMut<'a, TyStorage>,
}

impl<'db, 'a> From<&'a Typeck<'db>> for Normalize<'a> {
    fn from(value: &'a Typeck<'db>) -> Self {
        Self { storage: value.storage.borrow_mut() }
    }
}

impl TyFolder for Normalize<'_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Infer(InferTy::Ty(var)) => {
                self.storage.ty.probe_value(*var).map_or(ty, |ty| self.fold(ty))
            }
            TyKind::Infer(InferTy::Int(var)) => self
                .storage
                .int
                .probe_value(*var)
                .map_or(ty, |ty| TyKind::from(ty).into()),
            TyKind::Infer(InferTy::Float(var)) => self
                .storage
                .float
                .probe_value(*var)
                .map_or(ty, |ty| TyKind::from(ty).into()),
            _ => self.super_fold(ty),
        }
    }
}
