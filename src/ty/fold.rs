use crate::ty::{Ty, TyKind};

pub trait TyFolder {
    fn fold(&mut self, ty: &TyKind) -> TyKind;

    fn fold_ty(&mut self, ty: Ty) -> Ty {
        self.fold(ty.kind()).into()
    }
}
