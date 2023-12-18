use crate::ty::{FnTy, FnTyParam, Ty, TyKind};

pub trait TyFolder {
    fn fold(&mut self, ty: Ty) -> Ty;

    fn super_fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam {
                        name: param.name,
                        ty: self.fold(param.ty),
                    })
                    .collect(),
                ret: self.fold(fun.ret),
                is_c_variadic: fun.is_c_variadic,
            })
            .into(),
            TyKind::RawPtr(pointee) => {
                TyKind::RawPtr(self.fold(*pointee)).into()
            }
            TyKind::Struct(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => ty,
        }
    }
}
