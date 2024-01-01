use crate::ty::{FnTy, FnTyParam, Instantiation, Ty, TyKind};

pub(super) fn instantiate(ty: Ty, instantiation: &Instantiation) -> Ty {
    Instantiate::new(ty, instantiation).instantiate()
}

struct Instantiate<'a> {
    ty: Ty,
    instantiation: &'a Instantiation,
}

impl<'a> Instantiate<'a> {
    fn new(ty: Ty, instantiation: &'a Instantiation) -> Self {
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
                is_c_variadic: fun.is_c_variadic,
            })
            .into(),
            TyKind::Ref(inner, mutability) => {
                TyKind::Ref(self.instantiate_inner(*inner), *mutability).into()
            }
            TyKind::RawPtr(pointee) => {
                TyKind::RawPtr(self.instantiate_inner(*pointee)).into()
            }
            TyKind::Param(p) => match self.instantiation.get(p.var) {
                Some(ty) => ty,
                None => {
                    // NOTE: It currently makes sense to not instantiate params that are part of
                    // the currently typechecked function.
                    panic!(
                        "type param `{:?}` ({:?}/{:?}) not part of \
                         instantation, args={:?}",
                        p, self.ty, p.var, self.instantiation
                    )
                }
            },
            TyKind::Adt(id, targs) => TyKind::Adt(
                *id,
                targs.iter().map(|ty| self.instantiate_inner(*ty)).collect(),
            )
            .into(),
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => ty,
        }
    }
}
