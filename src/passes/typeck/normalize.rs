use crate::{
    passes::typeck::infcx::InferCtxtStorage,
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind},
};

pub trait NormalizeTy {
    fn normalize(self, infcx: &mut InferCtxtStorage) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, infcx: &mut InferCtxtStorage) -> Self {
        match self.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam { name: param.name, ty: param.ty.normalize(infcx) })
                    .collect(),
                ret: fun.ret.normalize(infcx),
            })
            .into(),
            TyKind::Infer(InferTy::TyVar(var)) => {
                infcx.ty_unification_table.probe_value(*var).map_or(self, |ty| ty.normalize(infcx))
            }
            _ => self,
        }
    }
}
