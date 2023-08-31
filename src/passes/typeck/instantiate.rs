use crate::ty::{FnTy, FnTyParam, Ty, TyKind};

pub(super) fn instantiate(ty: Ty, args: Vec<Ty>) -> Ty {
    Instantiate::new(ty, args).instantiate()
}

struct Instantiate {
    ty: Ty,
    args: Vec<Ty>,
}

impl Instantiate {
    fn new(ty: Ty, args: Vec<Ty>) -> Self {
        Self { ty, args }
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
            TyKind::Param(p) => match self.args.get(p.index) {
                Some(ty) => *ty,
                None => {
                    panic!(
                        "type param `{:?}` ({:?}/{}) out of when instantiating, args={:?}",
                        p, self.ty, p.index, self.args
                    )
                }
            },
            TyKind::Int(..)
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Infer(..)
            | TyKind::Unknown => ty,
        }
    }
}
