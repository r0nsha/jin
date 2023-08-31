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
            item.substitute(&mut cx);
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

trait Substitute<'db> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        match self {
            Self::Item(inner) => inner.substitute(cx),
            Self::If(inner) => inner.substitute(cx),
            Self::Block(inner) => inner.substitute(cx),
            Self::Return(inner) => inner.substitute(cx),
            Self::Call(inner) => inner.substitute(cx),
            Self::Bin(inner) => inner.substitute(cx),
            Self::Name(..) | Self::Lit(..) => (),
        }

        self.set_ty(cx.substitute_ty(self.ty(), self.span()));
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.substitute(cx),
        }

        self.ty = cx.substitute_ty(self.ty, self.span());
    }
}

impl Substitute<'_> for Fn {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        self.sig.substitute(cx);
        self.body.substitute(cx);
        self.ty = cx.substitute_ty(self.ty, self.span);
    }
}

impl Substitute<'_> for FnSig {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        for param in &mut self.params {
            param.ty = cx.substitute_ty(param.ty, param.span);
        }
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        self.cond.substitute(cx);
        self.then.substitute(cx);
        self.otherwise.substitute(cx);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        for stmt in &mut self.exprs {
            stmt.substitute(cx);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        self.expr.substitute(cx);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        self.callee.substitute(cx);

        for arg in &mut self.args {
            arg.expr.substitute(cx);
        }
    }
}

impl Substitute<'_> for Bin {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>) {
        self.lhs.substitute(cx);
        self.rhs.substitute(cx);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>) {
        for item in self {
            item.substitute(cx);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>) {
        if let Some(item) = self {
            item.substitute(cx);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>) {
        self.as_mut().substitute(cx);
    }
}
