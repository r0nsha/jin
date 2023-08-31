use std::collections::HashSet;

use crate::{
    hir::{Bin, Block, Call, Expr, Fn, FnSig, Hir, If, Item, ItemKind, Return},
    passes::typeck::infcx::{InferCtxt, InferCtxtInner},
    ty::{FnTy, FnTyParam, InferTy, Ty, TyKind, TyVar, Typed},
};

impl<'db> InferCtxt<'db> {
    pub fn substitute_all(&mut self, hir: &mut Hir) -> HashSet<TyVar> {
        let mut unbound_vars = HashSet::new();

        let mut infcx = self.inner.borrow_mut();

        for def in self.db.defs.iter_mut() {
            def.ty = substitute_ty(&mut infcx, def.ty, &mut unbound_vars);
        }

        for item in &mut hir.items {
            item.substitute(&mut infcx, &mut unbound_vars);
        }

        unbound_vars
    }
}

fn substitute_ty(infcx: &mut InferCtxtInner, ty: Ty, unbound_vars: &mut HashSet<TyVar>) -> Ty {
    Ty::new(substitute_tykind(infcx, &ty, unbound_vars))
}

fn substitute_tykind(
    infcx: &mut InferCtxtInner,
    ty: &TyKind,
    unbound_vars: &mut HashSet<TyVar>,
) -> TyKind {
    match ty {
        TyKind::Fn(fun) => TyKind::Fn(FnTy {
            params: fun
                .params
                .iter()
                .map(|param| FnTyParam {
                    name: param.name,
                    ty: substitute_ty(infcx, param.ty, unbound_vars),
                })
                .collect(),
            ret: substitute_ty(infcx, fun.ret, unbound_vars),
        }),
        TyKind::Infer(InferTy::TyVar(var)) => {
            let root = infcx.ty_unification_table.find(*var);

            if let Some(ty) = infcx.ty_unification_table.probe_value(root) {
                substitute_tykind(infcx, &ty, unbound_vars)
            } else {
                unbound_vars.insert(root);
                TyKind::Infer(InferTy::TyVar(root))
            }
        }
        TyKind::Infer(InferTy::IntVar(var)) => {
            let root = infcx.int_unification_table.find(*var);

            infcx
                .int_unification_table
                .probe_value(root)
                .map_or_else(|| TyKind::DEFAULT_INT, Into::into)
        }
        _ => ty.clone(),
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        match self {
            Self::Item(inner) => inner.substitute(infcx, unbound_vars),
            Self::If(inner) => inner.substitute(infcx, unbound_vars),
            Self::Block(inner) => inner.substitute(infcx, unbound_vars),
            Self::Return(inner) => inner.substitute(infcx, unbound_vars),
            Self::Call(inner) => inner.substitute(infcx, unbound_vars),
            Self::Bin(inner) => inner.substitute(infcx, unbound_vars),
            Self::Name(..) | Self::Lit(..) => (),
        }

        self.set_ty(substitute_ty(infcx, self.ty(), unbound_vars));
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.substitute(infcx, unbound_vars),
        }

        self.ty = substitute_ty(infcx, self.ty, unbound_vars);
    }
}

impl Substitute<'_> for Fn {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.sig.substitute(infcx, unbound_vars);
        self.body.substitute(infcx, unbound_vars);
        self.ty = substitute_ty(infcx, self.ty, unbound_vars);
    }
}

impl Substitute<'_> for FnSig {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        for param in &mut self.params {
            param.ty = substitute_ty(infcx, param.ty, unbound_vars);
        }
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.cond.substitute(infcx, unbound_vars);
        self.then.substitute(infcx, unbound_vars);
        self.otherwise.substitute(infcx, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        for stmt in &mut self.exprs {
            stmt.substitute(infcx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.expr.substitute(infcx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.callee.substitute(infcx, unbound_vars);

        for arg in &mut self.args {
            arg.expr.substitute(infcx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Bin {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.lhs.substitute(infcx, unbound_vars);
        self.rhs.substitute(infcx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        for item in self {
            item.substitute(infcx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        if let Some(item) = self {
            item.substitute(infcx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TyVar>) {
        self.as_mut().substitute(infcx, unbound_vars);
    }
}
