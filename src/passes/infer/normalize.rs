use crate::{
    passes::infer::typecx::TypeCx,
    ty::{FunctionTy, InferTy, Ty},
};

pub trait NormalizeTy {
    fn normalize(self, tcx: &mut TypeCx) -> Self;
}

impl NormalizeTy for Ty {
    fn normalize(self, tcx: &mut TypeCx) -> Self {
        match self {
            Self::Function(FunctionTy { ret, span }) => {
                Self::Function(FunctionTy { ret: Box::new(ret.normalize(tcx)), span })
            }
            Self::Infer(InferTy::TyVar(var), _) => {
                tcx.ty_unification_table.probe_value(var).map_or(self, |ty| ty.normalize(tcx))
            }
            _ => self,
        }
    }
}
