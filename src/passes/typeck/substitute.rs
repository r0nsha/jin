use std::collections::HashSet;

use crate::{
    db::Db,
    hir::{Bin, Block, Call, Expr, Fn, FnSig, Hir, If, Item, ItemKind, Return},
    passes::typeck::{
        infcx::{InferCtxt, InferCtxtInner},
        InferResult,
    },
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind, TyVar, Typed},
};

impl<'db> InferCtxt<'db> {
    pub fn substitute_all(&mut self, hir: &mut Hir) -> HashSet<TyVar> {
        let mut unbound_vars = HashSet::new();

        let mut cx = SubstCtxt { db: self.db, infcx: &mut self.inner.borrow_mut() };

        let mut def_tys: Vec<_> = cx.db.defs.iter().map(|d| d.ty).collect();

        for ty in &mut def_tys {
            *ty = cx.substitute_ty(*ty, &mut unbound_vars);
        }

        for (def, new_ty) in cx.db.defs.iter_mut().zip(def_tys) {
            def.ty = new_ty;
        }

        for item in &mut hir.items {
            item.substitute(&mut cx, &mut unbound_vars);
        }

        unbound_vars
    }
}

struct SubstCtxt<'db> {
    db: &'db mut Db,
    infcx: &'db mut InferCtxtInner,
}

impl SubstCtxt<'_> {
    fn substitute_ty(&mut self, ty: Ty, unbound_vars: &mut HashSet<TyVar>) -> Ty {
        match self.substitute_tykind(&ty, unbound_vars) {
            Ok(ty) => ty.into(),
            Err(err) => {
                self.db.diagnostics.add(err.into_diagnostic(self.db));
                ty
            }
        }
    }

    fn substitute_tykind(
        &mut self,
        ty: &TyKind,
        unbound_vars: &mut HashSet<TyVar>,
    ) -> InferResult<TyKind> {
        match ty {
            TyKind::Fn(fun) => Ok(TyKind::Fn(FnTy {
                params: fun
                    .params
                    .iter()
                    .map(|param| {
                        Ok(FnTyParam {
                            name: param.name,
                            ty: self.substitute_ty(param.ty, unbound_vars),
                        })
                    })
                    .try_collect()?,
                ret: self.substitute_ty(fun.ret, unbound_vars),
            })),
            TyKind::Infer(InferTy::TyVar(var)) => {
                let root = self.infcx.ty_unification_table.find(*var);

                if let Some(ty) = self.infcx.ty_unification_table.probe_value(root) {
                    self.substitute_tykind(&ty, unbound_vars)
                } else {
                    unbound_vars.insert(root);
                    Ok(TyKind::Infer(InferTy::TyVar(root)))
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                let root = self.infcx.int_unification_table.find(*var);

                Ok(self
                    .infcx
                    .int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TyKind::DEFAULT_INT, Into::into))
            }
            _ => Ok(ty.clone()),
        }
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>, unbound_vars: &mut HashSet<TyVar>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        match self {
            Self::Item(inner) => inner.substitute(cx, unbound_vars),
            Self::If(inner) => inner.substitute(cx, unbound_vars),
            Self::Block(inner) => inner.substitute(cx, unbound_vars),
            Self::Return(inner) => inner.substitute(cx, unbound_vars),
            Self::Call(inner) => inner.substitute(cx, unbound_vars),
            Self::Bin(inner) => inner.substitute(cx, unbound_vars),
            Self::Name(..) | Self::Lit(..) => (),
        }

        self.set_ty(cx.substitute_ty(self.ty(), unbound_vars));
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.substitute(cx, unbound_vars),
        }

        self.ty = cx.substitute_ty(self.ty, unbound_vars);
    }
}

impl Substitute<'_> for Fn {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.sig.substitute(cx, unbound_vars);
        self.body.substitute(cx, unbound_vars);
        self.ty = cx.substitute_ty(self.ty, unbound_vars);
    }
}

impl Substitute<'_> for FnSig {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        for param in &mut self.params {
            param.ty = cx.substitute_ty(param.ty, unbound_vars);
        }
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.cond.substitute(cx, unbound_vars);
        self.then.substitute(cx, unbound_vars);
        self.otherwise.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        for stmt in &mut self.exprs {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.expr.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.callee.substitute(cx, unbound_vars);

        for arg in &mut self.args {
            arg.expr.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Bin {
    fn substitute(&mut self, cx: &mut SubstCtxt<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.lhs.substitute(cx, unbound_vars);
        self.rhs.substitute(cx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>, unbound_vars: &mut HashSet<TyVar>) {
        for item in self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>, unbound_vars: &mut HashSet<TyVar>) {
        if let Some(item) = self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, cx: &mut SubstCtxt<'db>, unbound_vars: &mut HashSet<TyVar>) {
        self.as_mut().substitute(cx, unbound_vars);
    }
}
