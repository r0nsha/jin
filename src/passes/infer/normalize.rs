use crate::passes::infer::typecx::TypeCx;
use crate::ty::{Ty, TyKind};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut TypeCx) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, tcx: &mut TypeCx) -> Self {
        match self.kind {
            TyKind::Function(fun) => {
                let ret = fun.ret.normalize(tcx);
                Ty::fun(ret, self.span)
            }
            TyKind::Var(var) => match tcx.unification_table.probe_value(var) {
                Some(ty) => ty.normalize(tcx),
                None => self,
            },
            TyKind::Int(_) | TyKind::Unit | TyKind::Never => self,
        }
    }
}
