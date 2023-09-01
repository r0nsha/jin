use std::collections::HashMap;

use crate::{
    hir::{Bin, Block, Call, Expr, Fn, FnSig, Hir, If, Item, ItemKind, Name, Return},
    passes::typeck::{
        error::InferError,
        infcx::{InferCtxt, InferCtxtInner},
        normalize::NormalizeTy,
    },
    span::{Span, Spanned},
    ty::{fold::TyFolder, InferTy, Ty, TyKind, Typed},
};

impl<'db> InferCtxt<'db> {
    pub fn subst(&mut self, hir: &mut Hir) {
        let mut cx = SubstCtxt { infcx: &mut self.inner.borrow_mut(), errs: HashMap::new() };

        for def in self.db.defs.iter_mut() {
            def.ty = cx.subst_ty(def.ty, def.span);
        }

        for item in &mut hir.items {
            item.subst(&mut cx);
        }

        let diagnostics: Vec<_> =
            cx.errs.into_values().map(|e| e.into_diagnostic(self.db)).collect();
        self.db.diagnostics.extend(diagnostics);
    }
}

struct SubstCtxt<'db> {
    infcx: &'db mut InferCtxtInner,
    errs: HashMap<Span, InferError>,
}

impl SubstCtxt<'_> {
    fn subst_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut s = SubstTy { cx: self, has_unbound_vars: false };
        let ty = s.fold(ty);

        if s.has_unbound_vars {
            s.cx.errs.insert(span, InferError::CannotInfer { ty: ty.normalize(s.cx.infcx), span });
        }

        ty
    }
}

struct SubstTy<'db, 'a> {
    cx: &'a mut SubstCtxt<'db>,
    has_unbound_vars: bool,
}

impl TyFolder for SubstTy<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Infer(InferTy::TyVar(var)) => {
                let root = self.cx.infcx.ty_unification_table.find(*var);

                if let Some(ty) = self.cx.infcx.ty_unification_table.probe_value(root) {
                    self.fold(ty)
                } else {
                    self.has_unbound_vars = true;
                    TyKind::Infer(InferTy::TyVar(*var)).into()
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.cx.infcx.int_unification_table.find(*var);

                self.cx
                    .infcx
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
                    .into()
            }
            _ => self.super_fold(ty),
        }
    }
}

trait Subst<'db> {
    fn subst(&mut self, cx: &mut SubstCtxt<'db>);
}

impl Subst<'_> for Expr {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        match self {
            Self::Item(inner) => inner.subst(cx),
            Self::If(inner) => inner.subst(cx),
            Self::Block(inner) => inner.subst(cx),
            Self::Return(inner) => inner.subst(cx),
            Self::Call(inner) => inner.subst(cx),
            Self::Bin(inner) => inner.subst(cx),
            Self::Name(inner) => inner.subst(cx),
            Self::Lit(..) => (),
        }

        self.set_ty(cx.subst_ty(self.ty(), self.span()));
    }
}

impl Subst<'_> for Item {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.subst(cx),
        }

        self.ty = cx.subst_ty(self.ty, self.span());
    }
}

impl Subst<'_> for Fn {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.sig.subst(cx);
        self.body.subst(cx);
        self.ty = cx.subst_ty(self.ty, self.span);
    }
}

impl Subst<'_> for FnSig {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        for param in &mut self.params {
            param.ty = cx.subst_ty(param.ty, param.span);
        }
    }
}

impl Subst<'_> for If {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.cond.subst(cx);
        self.then.subst(cx);

        if let Some(o) = &mut self.otherwise {
            o.subst(cx);
        }
    }
}

impl Subst<'_> for Block {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        for stmt in &mut self.exprs {
            stmt.subst(cx);
        }
    }
}

impl Subst<'_> for Return {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.expr.subst(cx);
    }
}

impl Subst<'_> for Call {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.callee.subst(cx);

        for arg in &mut self.args {
            arg.expr.subst(cx);
        }
    }
}

impl Subst<'_> for Bin {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.lhs.subst(cx);
        self.rhs.subst(cx);
    }
}

impl Subst<'_> for Name {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        for arg in &mut self.args {
            *arg = cx.subst_ty(*arg, self.span);
        }
    }
}
