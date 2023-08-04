use std::collections::HashSet;

use crate::{
    db::TypeId,
    hir::*,
    ty::{Type, TypeKind, TypeVar},
};

use super::{constraint::Constraints, InferCx};

// Substitute
impl<'a> InferCx<'a> {
    pub(crate) fn substitution(
        &mut self,
        modules: &mut [Module],
        constraints: &Constraints,
    ) -> HashSet<TypeVar> {
        let mut unbound_vars = HashSet::new();

        for module in modules {
            for binding in &mut module.bindings {
                binding.substitute(self, &mut unbound_vars);
            }
        }

        unbound_vars
    }

    fn substitute_type_id(&mut self, id: TypeId, unbound_vars: &mut HashSet<TypeVar>) {
        let ty = id.get(&self.db);

        let new_ty = self.substitute_ty(ty, unbound_vars);
        *id.get_mut(&mut self.db) = new_ty;
    }

    fn substitute_ty(&mut self, ty: &Type, unbound_vars: &mut HashSet<TypeVar>) -> Type {
        match ty.kind {
            TypeKind::Fun(fun) => {
                let ret = self.substitute_ty(&fun.ret, unbound_vars);
                Type::fun(ret, ty.span)
            }
            TypeKind::Var(v) => {
                let root = self.typecx.unification_table.find(v);

                match self.typecx.unification_table.probe_value(root) {
                    Some(ty) => self.substitute_ty(&ty, unbound_vars),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        Type::var(root, ty.span)
                    }
                }
            }
            _ => ty.clone(),
        }
    }
}

trait Substitute<'a> {
    fn substitute(&mut self, cx: &mut InferCx<'a>, unbound_vars: &mut HashSet<TypeVar>);
}

impl Substitute<'_> for Hir {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        match self {
            Hir::Fun(x) => x.substitute(cx, unbound_vars),
            Hir::Ret(x) => x.substitute(cx, unbound_vars),
            Hir::Lit(x) => x.substitute(cx, unbound_vars),
        }
    }
}

impl Substitute<'_> for Binding {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        cx.substitute_type_id(self.id.get(&cx.db).ty, unbound_vars);
        self.expr.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Fun {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.body.substitute(cx, unbound_vars);
        cx.substitute_type_id(self.id.get(&cx.db).ty, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        for stmt in &mut self.statements {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Ret {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.expr.substitute(cx);
    }
}

impl Substitute<'_> for Lit {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        let (unbound_ty, ty) = self.substitute(hir.ty().get(&self.db));
        hir.set_ty(ty);
        unbound_ty
    }
}
