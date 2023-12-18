use crate::{
    db::Db,
    middle::Pat,
    span::{Span, Spanned},
    ty::{fold::TyFolder, Instantiation, Ty, TyKind},
};

pub trait SubstTy {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty;
    fn db(&mut self) -> &mut Db;
}

pub trait Subst<S: SubstTy> {
    fn subst(&mut self, s: &mut S);
}

impl<S: SubstTy> Subst<S> for Pat {
    fn subst(&mut self, s: &mut S) {
        match self {
            Pat::Name(name) => {
                let ty = s.db()[name.id].ty;
                s.db()[name.id].ty = s.subst_ty(ty, name.span());
            }
            Pat::Discard(_) => (),
        }
    }
}

pub struct ParamFolder<'db, 'a> {
    pub db: &'db mut Db,
    pub instantiation: &'a Instantiation,
}

impl SubstTy for ParamFolder<'_, '_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }

    fn db(&mut self) -> &mut Db {
        self.db
    }
}

impl TyFolder for ParamFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => self.instantiation[&p.var],
            _ => self.super_fold(ty),
        }
    }
}
