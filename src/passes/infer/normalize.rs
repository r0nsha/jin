use crate::passes::infer::typecx::TypeCx;
use crate::ty::{FunctionTy, Ty};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut TypeCx) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, tcx: &mut TypeCx) -> Self {
        match self {
            Self::Function(FunctionTy { ret, span }) => {
                Self::Function(FunctionTy {
                    ret: Box::new(ret.normalize(tcx)),
                    span,
                })
            }
            Self::Var(var, _) => tcx
                .unification_table
                .probe_value(var)
                .map_or(self, |ty| ty.normalize(tcx)),
            Self::Int(..) | Self::Unit(_) | Self::Never(_) => self,
        }
    }
}
