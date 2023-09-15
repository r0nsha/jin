use crate::{
    typeck::tcx::TyCtxtStorage,
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind},
};

pub trait NormalizeTy {
    fn normalize(self, storage: &mut TyCtxtStorage) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, storage: &mut TyCtxtStorage) -> Self {
        match self.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam { name: param.name, ty: param.ty.normalize(storage) })
                    .collect(),
                ret: fun.ret.normalize(storage),
            })
            .into(),
            TyKind::Infer(InferTy::TyVar(var)) => storage
                .ty_unification_table
                .probe_value(*var)
                .map_or(self, |ty| ty.normalize(storage)),
            _ => self,
        }
    }
}
