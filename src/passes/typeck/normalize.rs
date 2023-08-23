use crate::{
    passes::typeck::infcx::InferCtxt,
    ty::{FunctionType, FunctionTypeParam, InferType, Type, TypeKind},
};

pub trait NormalizeTy {
    fn normalize(self, infcx: &mut InferCtxt) -> Self;
}

impl NormalizeTy for Type {
    fn normalize(self, infcx: &mut InferCtxt) -> Self {
        match self.as_ref() {
            TypeKind::Function(FunctionType { ret, params, span }) => {
                TypeKind::Function(FunctionType {
                    ret: ret.normalize(infcx),
                    params: params
                        .iter()
                        .map(|param| FunctionTypeParam {
                            name: param.name,
                            ty: param.ty.normalize(infcx),
                        })
                        .collect(),
                    span: *span,
                })
                .into()
            }
            TypeKind::Infer(InferType::TypeVar(var), _) => {
                infcx.ty_unification_table.probe_value(*var).map_or(self, |ty| ty.normalize(infcx))
            }
            _ => self,
        }
    }
}
