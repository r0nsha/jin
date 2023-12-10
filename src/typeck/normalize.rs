use crate::{
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind},
    typeck::TyStorage,
};

pub trait NormalizeTy {
    fn normalize(self, storage: &mut TyStorage) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, storage: &mut TyStorage) -> Self {
        match self.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam {
                        name: param.name,
                        ty: param.ty.normalize(storage),
                    })
                    .collect(),
                ret: fun.ret.normalize(storage),
                is_c_variadic: fun.is_c_variadic,
            })
            .into(),
            TyKind::RawPtr(pointee) => {
                TyKind::RawPtr(pointee.normalize(storage)).into()
            }
            TyKind::Infer(InferTy::Ty(var)) => storage
                .ty_unification_table
                .probe_value(*var)
                .map_or(self, |ty| ty.normalize(storage)),
            TyKind::Infer(InferTy::Int(var)) => storage
                .int_unification_table
                .probe_value(*var)
                .map_or(self, |ty| TyKind::from(ty).into()),
            TyKind::Infer(InferTy::Float(var)) => storage
                .float_unification_table
                .probe_value(*var)
                .map_or(self, |ty| TyKind::from(ty).into()),
            TyKind::Struct(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => self,
        }
    }
}
