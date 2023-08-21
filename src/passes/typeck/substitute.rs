use std::collections::HashSet;

use super::TypeCx;
use crate::{
    db::TyId,
    hir::{Binary, Block, Call, CallArg, Expr, Function, If, Item, ItemKind, Module, Return},
    ty::{FunctionParamTy, FunctionTy, InferTy, Ty, TyVar, Typed},
};

// Substitute
impl<'db> TypeCx<'db> {
    pub fn substitution(&mut self, modules: &mut [Module]) -> HashSet<TyVar> {
        let mut unbound_vars = HashSet::new();

        for i in 0..self.db.symbols.len() {
            let ty = self.db.symbols[i.into()].ty;
            self.substitute_tyid(ty, &mut unbound_vars);
        }

        for module in modules {
            for item in &mut module.items {
                item.substitute(self, &mut unbound_vars);
            }
        }

        unbound_vars
    }

    fn substitute_tyid(&mut self, id: TyId, unbound_vars: &mut HashSet<TyVar>) {
        let ty = self.db[id].clone();

        let new_ty = self.substitute_ty(&ty, unbound_vars);
        self.db[id] = new_ty;
    }

    fn substitute_ty(&mut self, ty: &Ty, unbound_vars: &mut HashSet<TyVar>) -> Ty {
        match ty {
            Ty::Function(fun) => Ty::Function(FunctionTy {
                ret: Box::new(self.substitute_ty(&fun.ret, unbound_vars)),
                params: fun
                    .params
                    .iter()
                    .map(|param| FunctionParamTy {
                        name: param.name,
                        ty: self.substitute_ty(&param.ty, unbound_vars),
                    })
                    .collect(),
                span: fun.span,
            }),
            Ty::Infer(InferTy::TyVar(var), span) => {
                let root = self.ty_unification_table.find(*var);

                if let Some(ty) = self.ty_unification_table.probe_value(root) {
                    self.substitute_ty(&ty, unbound_vars)
                } else {
                    unbound_vars.insert(root);
                    Ty::Infer(InferTy::TyVar(root), *span)
                }
            }
            Ty::Infer(InferTy::IntVar(var), span) => {
                let root = self.int_unification_table.find(*var);

                self.int_unification_table
                    .probe_value(root)
                    .map_or_else(|| Ty::default_int(*span), Into::into)
            }
            _ => ty.clone(),
        }
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, cx: &mut TypeCx<'db>, unbound_vars: &mut HashSet<TyVar>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        match self {
            Self::Item(inner) => inner.substitute(cx, unbound_vars),
            Self::If(inner) => inner.substitute(cx, unbound_vars),
            Self::Block(inner) => inner.substitute(cx, unbound_vars),
            Self::Return(inner) => inner.substitute(cx, unbound_vars),
            Self::Call(inner) => inner.substitute(cx, unbound_vars),
            Self::Binary(inner) => inner.substitute(cx, unbound_vars),
            Self::Name(_) | Self::Lit(_) => (),
        }

        cx.substitute_tyid(self.ty(), unbound_vars);
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        match &mut self.kind {
            ItemKind::Function(fun) => fun.substitute(cx, unbound_vars),
        }

        cx.substitute_tyid(self.ty, unbound_vars);
        cx.substitute_tyid(cx.db[self.id.expect("to be resolved")].ty, unbound_vars);
    }
}

impl Substitute<'_> for Function {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        for param in &self.params {
            cx.substitute_tyid(param.ty, unbound_vars);
        }

        self.body.substitute(cx, unbound_vars);
        cx.substitute_tyid(self.ty, unbound_vars);
        cx.substitute_tyid(cx.db[self.id.expect("to be resolved")].ty, unbound_vars);
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.cond.substitute(cx, unbound_vars);
        self.then.substitute(cx, unbound_vars);
        self.otherwise.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        for stmt in &mut self.exprs {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.expr.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.callee.substitute(cx, unbound_vars);

        for arg in &mut self.args {
            arg.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for CallArg {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        match self {
            Self::Positional(expr) | Self::Named(_, expr) => {
                expr.substitute(cx, unbound_vars);
            }
        }
    }
}

impl Substitute<'_> for Binary {
    fn substitute(&mut self, cx: &mut TypeCx<'_>, unbound_vars: &mut HashSet<TyVar>) {
        self.lhs.substitute(cx, unbound_vars);
        self.rhs.substitute(cx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, cx: &mut TypeCx<'db>, unbound_vars: &mut HashSet<TyVar>) {
        for item in self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, cx: &mut TypeCx<'db>, unbound_vars: &mut HashSet<TyVar>) {
        if let Some(item) = self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, cx: &mut TypeCx<'db>, unbound_vars: &mut HashSet<TyVar>) {
        self.as_mut().substitute(cx, unbound_vars);
    }
}
