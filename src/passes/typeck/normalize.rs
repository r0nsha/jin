use crate::{
    passes::typeck::infcx::InferCtxtInner,
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind},
};

pub trait NormalizeTy {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self {
        match self.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                ty_params: fun.ty_params.clone(),
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
