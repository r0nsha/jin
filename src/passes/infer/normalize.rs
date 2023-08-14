use crate::ty::typecx::TypeCx;
use crate::ty::{Ty, TyKind};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut TypeCx) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, tcx: &mut TypeCx) -> Self {
        match self.kind {
            TyKind::Function(fun) => {
                let ret = fun.ret.normalize(tcx);
                Self::fun(ret, self.span)
            }
            TyKind::Var(var) => tcx
                .unification_table
                .probe_value(var)
                .map_or(self, |ty| ty.normalize(tcx)),
            TyKind::Int(_) | TyKind::Unit | TyKind::Never => self,
        }
    }
}
