use crate::{
    passes::typeck::infcx::InferCtxt,
    ty::{FunctionType, FunctionTypeParam, InferType, Type},
};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut InferCtxt) -> Self;
}

impl NormalizeTy for Type {
    fn normalize(self, tcx: &mut InferCtxt) -> Self {
        match self {
            Self::Function(FunctionType { ret, params, span }) => Self::Function(FunctionType {
                ret: Box::new(ret.normalize(tcx)),
                params: params
                    .into_iter()
                    .map(|param| FunctionTypeParam {
                        name: param.name,
                        ty: param.ty.normalize(tcx),
                    })
                    .collect(),
                span,
            }),
            Self::Infer(InferType::TypeVar(var), _) => {
                tcx.ty_unification_table.probe_value(var).map_or(self, |ty| ty.normalize(tcx))
            }
            _ => self,
        }
    }
}
