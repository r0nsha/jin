use crate::{
    middle::Pat,
    span::{Span, Spanned},
    ty::{fold::TyFolder, Instantiation, Ty, TyKind},
};

pub trait SubstTy {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty;
}

pub trait Subst<S: SubstTy> {
    fn subst(&mut self, s: &mut S);
}

impl<S: SubstTy> Subst<S> for Pat {
    fn subst(&mut self, s: &mut S) {
        match self {
            Pat::Name(name) => {
                name.ty = s.subst_ty(name.ty, name.span());
            }
            Pat::Discard(_) => (),
        }
    }
}

pub fn subst_instantation(
    s: &mut impl SubstTy,
    instantiation: &mut Instantiation,
    span: Span,
) {
    for ty in instantiation.tys_mut() {
        *ty = s.subst_ty(*ty, span);
    }
}

pub struct ParamFolder<'a> {
    pub instantiation: &'a Instantiation,
}

impl<'a> ParamFolder<'a> {
    pub fn new(instantiation: &'a Instantiation) -> Self {
        Self { instantiation }
    }
}

impl<'a> From<&'a Instantiation> for ParamFolder<'a> {
    fn from(value: &'a Instantiation) -> Self {
        Self::new(value)
    }
}

impl SubstTy for ParamFolder<'_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }
}

impl TyFolder for ParamFolder<'_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => self.instantiation[p.var],
            _ => self.super_fold(ty),
        }
    }
}
