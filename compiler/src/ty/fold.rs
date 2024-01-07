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
            TyKind::Ref(inner, mutability) => {
                TyKind::Ref(self.fold(*inner), *mutability).into()
            }
            TyKind::RawPtr(inner) => TyKind::RawPtr(self.fold(*inner)).into(),
            TyKind::Type(inner) => TyKind::Type(self.fold(*inner)).into(),
            TyKind::Adt(id, targs) => TyKind::Adt(
                *id,
                targs.iter().map(|ty| self.fold(*ty)).collect(),
            )
            .into(),
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Module(_)
            | TyKind::Unknown => ty,
        }
    }
}
