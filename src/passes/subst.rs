use crate::{
    db::Db,
    hir::{Expr, ExprKind, Fn, FnSig, Item, ItemKind},
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

impl<S: SubstTy> Subst<S> for Expr {
    fn subst(&mut self, s: &mut S) {
        match &mut self.kind {
            ExprKind::If(if_) => {
                if_.cond.subst(s);
                if_.then.subst(s);

                if let Some(o) = &mut if_.otherwise {
                    o.subst(s);
                }
            }
            ExprKind::Block(blk) => {
                for stmt in &mut blk.exprs {
                    stmt.subst(s);
                }
            }
            ExprKind::Return(ret) => {
                ret.expr.subst(s);
            }
            ExprKind::Call(call) => {
                call.callee.subst(s);

                for arg in &mut call.args {
                    arg.expr.subst(s);
                }
            }
            ExprKind::BinOp(bin) => {
                bin.lhs.subst(s);
                bin.rhs.subst(s);
            }
            ExprKind::Name(name) => {
                for ty in name.instantiation.values_mut() {
                    *ty = s.subst_ty(*ty, self.span);
                }
            }
            ExprKind::Lit(_) => (),
        }

        self.ty = s.subst_ty(self.ty, self.span);
    }
}

impl<S: SubstTy> Subst<S> for Item {
    fn subst(&mut self, s: &mut S) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.subst(s),
        }

        self.ty = s.subst_ty(self.ty, self.span());
    }
}

impl<S: SubstTy> Subst<S> for Fn {
    fn subst(&mut self, s: &mut S) {
        self.sig.subst(s);
        self.body.subst(s);
        self.ty = s.subst_ty(self.ty, self.span);
        s.db()[self.id].ty = self.ty;
    }
}

impl<S: SubstTy> Subst<S> for FnSig {
    fn subst(&mut self, s: &mut S) {
        for param in &mut self.params {
            param.ty = s.subst_ty(param.ty, param.span);
            s.db()[param.id].ty = param.ty;
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
