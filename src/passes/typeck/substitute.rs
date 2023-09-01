use std::collections::HashMap;

use crate::{
    hir::{Bin, Block, Call, Expr, Fn, FnSig, Hir, If, Item, ItemKind, Return},
    passes::typeck::{
        error::InferError,
        infcx::{InferCtxt, InferCtxtInner},
        normalize::NormalizeTy,
    },
    span::{Span, Spanned},
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind, Typed},
};

impl<'db> InferCtxt<'db> {
    pub fn substitute_all(&mut self, hir: &mut Hir) {
        let mut cx = SubstCtxt { infcx: &mut self.inner.borrow_mut(), errs: HashMap::new() };

        for def in self.db.defs.iter_mut() {
            def.ty = cx.substitute_ty(def.ty, def.span);
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
    fn substitute_ty(&mut self, ty: Ty, span: Span) -> Ty {
        let mut s = SubstTy { cx: self, has_unbound_vars: false };
        let ty = Ty::new(s.subst(&ty));

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

impl SubstTy<'_, '_> {
    fn subst(&mut self, ty: &TyKind) -> TyKind {
        match ty {
            TyKind::Fn(fun) => TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| FnTyParam { name: param.name, ty: self.subst(&param.ty).into() })
                    .collect(),
                ret: self.subst(&fun.ret).into(),
            }),
            TyKind::Infer(InferTy::TyVar(var)) => {
                let root = self.cx.infcx.ty_unification_table.find(*var);

                if let Some(ty) = self.cx.infcx.ty_unification_table.probe_value(root) {
                    self.subst(&ty)
                } else {
                    self.has_unbound_vars = true;
                    TyKind::Infer(InferTy::TyVar(*var))
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.cx.infcx.int_unification_table.find(*var);

                // TODO: swap for the default int in monorphization pass?
                self.cx
                    .infcx
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
            }
            _ => ty.clone(),
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
            Self::Name(..) | Self::Lit(..) => (),
        }

        self.set_ty(cx.substitute_ty(self.ty(), self.span()));
    }
}

impl Subst<'_> for Item {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.subst(cx),
        }

        self.ty = cx.substitute_ty(self.ty, self.span());
    }
}

impl Subst<'_> for Fn {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.sig.subst(cx);
        self.body.subst(cx);
        self.ty = cx.substitute_ty(self.ty, self.span);
    }
}

impl Subst<'_> for FnSig {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        for param in &mut self.params {
            param.ty = cx.substitute_ty(param.ty, param.span);
        }
    }
}

impl Subst<'_> for If {
    fn subst(&mut self, cx: &mut SubstCtxt<'_>) {
        self.cond.subst(cx);
        self.then.subst(cx);
        self.otherwise.subst(cx);
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

impl<'db, T: Subst<'db>> Subst<'db> for Vec<T> {
    fn subst(&mut self, cx: &mut SubstCtxt<'db>) {
        for item in self {
            item.subst(cx);
        }
    }
}

impl<'db, T: Subst<'db>> Subst<'db> for Option<T> {
    fn subst(&mut self, cx: &mut SubstCtxt<'db>) {
        if let Some(item) = self {
            item.subst(cx);
        }
    }
}

impl<'db, T: Subst<'db>> Subst<'db> for Box<T> {
    fn subst(&mut self, cx: &mut SubstCtxt<'db>) {
        self.as_mut().subst(cx);
    }
}
