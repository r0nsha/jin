use crate::{
    passes::typeck::TypeCx,
    ty::{FunctionParamTy, FunctionTy, InferTy, Ty},
};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut TypeCx) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, tcx: &mut TypeCx) -> Self {
        match self {
            Self::Function(FunctionTy { ret, params, span }) => Self::Function(FunctionTy {
                ret: Box::new(ret.normalize(tcx)),
                params: params
                    .into_iter()
                    .map(|param| FunctionParamTy { name: param.name, ty: param.ty.normalize(tcx) })
                    .collect(),
                span,
            }),
            Self::Infer(InferTy::TyVar(var), _) => {
                tcx.ty_unification_table.probe_value(var).map_or(self, |ty| ty.normalize(tcx))
            }
            _ => self,
        }
    }
}
