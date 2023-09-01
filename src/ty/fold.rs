use crate::ty::Ty;

pub trait TyFolder {
    fn fold(&mut self, ty: Ty) -> Ty;
}
