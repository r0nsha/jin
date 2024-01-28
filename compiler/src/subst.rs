use crate::{
    middle::Pat,
    span::{Span, Spanned},
    ty::{Instantiation, Ty},
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

pub fn subst_instantation(s: &mut impl SubstTy, instantiation: &mut Instantiation, span: Span) {
    for ty in instantiation.tys_mut() {
        *ty = s.subst_ty(*ty, span);
    }
}
