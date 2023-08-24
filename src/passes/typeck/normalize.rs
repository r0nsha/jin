use crate::{
    passes::typeck::infcx::InferCtxtInner,
    ty::{FunctionType, FunctionTypeParam, InferType, Type, TypeKind},
};

pub trait NormalizeTy {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self;
}

impl NormalizeTy for Type {
    fn normalize(self, infcx: &mut InferCtxtInner) -> Self {
        match self.as_ref() {
            TypeKind::Function(FunctionType { ret, params }) => TypeKind::Function(FunctionType {
                ret: ret.normalize(infcx),
                params: params
                    .iter()
                    .map(|param| FunctionTypeParam {
                        name: param.name,
                        ty: param.ty.normalize(infcx),
                    })
                    .collect(),
            })
            .into(),
            TypeKind::Infer(InferType::TypeVar(var)) => {
                infcx.ty_unification_table.probe_value(*var).map_or(self, |ty| ty.normalize(infcx))
            }
            _ => self,
        }
    }
}
