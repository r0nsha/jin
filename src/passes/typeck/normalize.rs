use crate::{
    passes::typeck::infcx::InferCtxtInner,
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind},
};

pub trait NormalizeTy {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self {
        match self.as_ref() {
            TyKind::Fn(FnTy { ret, params }) => TyKind::Fn(FnTy {
                ret: ret.normalize(infcx),
                params: params
                    .iter()
                    .map(|param| FnTyParam {
                        name: param.name,
                        ty: param.ty.normalize(infcx),
                    })
                    .collect(),
            })
            .into(),
            TyKind::Infer(InferTy::TyVar(var)) => {
                infcx.ty_unification_table.probe_value(*var).map_or(self, |ty| ty.normalize(infcx))
            }
            _ => self,
        }
    }
}
