use crate::ty::{FnTy, FnTyParam, Instantiation, Ty, TyKind};

pub(super) fn instantiate(ty: Ty, instantiation: Instantiation) -> Ty {
    Instantiate::new(ty, instantiation).instantiate()
}

struct Instantiate {
    ty: Ty,
    instantiation: Instantiation,
}

impl Instantiate {
    fn new(ty: Ty, instantiation: Instantiation) -> Self {
        Self { ty, instantiation }
    }

    fn instantiate(&self) -> Ty {
        self.instantiate_inner(self.ty)
    }

    fn instantiate_inner(&self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam {
                        name: param.name,
                        ty: self.instantiate_inner(param.ty),
                    })
                    .collect(),
                ret: self.instantiate_inner(fun.ret),
            })
            .into(),
            TyKind::Param(p) => match self.instantiation.get(&p.var) {
                Some(ty) => *ty,
                None => {
                    ty
                    // NOTE: It currently makes sense to not instantiate params that are part of
                    // the currently typechecked function.
                    // panic!(
                    //     "type param `{:?}` ({:?}/{:?}) out of when instantiating, args={:?}",
                    //     p, self.ty, p.var, self.instantiation
                    // )
                }
            },
            _ => ty,
        }
    }
}
