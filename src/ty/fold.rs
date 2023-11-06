use crate::ty::{FnTy, FnTyParam, Ty, TyKind};

pub trait TyFolder {
    fn fold(&mut self, ty: Ty) -> Ty;

    fn super_fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam { name: param.name, ty: self.fold(param.ty) })
                    .collect(),
                ret: self.fold(fun.ret),
                is_c_variadic: fun.is_c_variadic,
            })
            .into(),
            _ => ty,
        }
    }
}
