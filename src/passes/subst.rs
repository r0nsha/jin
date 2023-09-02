use crate::{
    db::Db,
    hir::{Bin, Block, Call, Expr, Fn, FnSig, If, Item, ItemKind, Lit, Name, Return},
    span::{Span, Spanned},
    ty::{Ty, Typed},
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
        match self {
            Self::Item(inner) => inner.subst(s),
            Self::If(inner) => inner.subst(s),
            Self::Block(inner) => inner.subst(s),
            Self::Return(inner) => inner.subst(s),
            Self::Call(inner) => inner.subst(s),
            Self::Bin(inner) => inner.subst(s),
            Self::Name(inner) => inner.subst(s),
            Self::Lit(inner) => inner.subst(s),
        }

        self.set_ty(s.subst_ty(self.ty(), self.span()));
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

impl<S: SubstTy> Subst<S> for If {
    fn subst(&mut self, s: &mut S) {
        self.cond.subst(s);
        self.then.subst(s);

        if let Some(o) = &mut self.otherwise {
            o.subst(s);
        }
    }
}

impl<S: SubstTy> Subst<S> for Block {
    fn subst(&mut self, s: &mut S) {
        for stmt in &mut self.exprs {
            stmt.subst(s);
        }
    }
}

impl<S: SubstTy> Subst<S> for Return {
    fn subst(&mut self, s: &mut S) {
        self.expr.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for Call {
    fn subst(&mut self, s: &mut S) {
        self.callee.subst(s);

        for arg in &mut self.args {
            arg.expr.subst(s);
        }
    }
}

impl<S: SubstTy> Subst<S> for Bin {
    fn subst(&mut self, s: &mut S) {
        self.lhs.subst(s);
        self.rhs.subst(s);
    }
}

impl<S: SubstTy> Subst<S> for Name {
    fn subst(&mut self, s: &mut S) {
        for arg in &mut self.args {
            *arg = s.subst_ty(*arg, self.span);
        }
    }
}

impl<S: SubstTy> Subst<S> for Lit {
    fn subst(&mut self, _s: &mut S) {}
}
