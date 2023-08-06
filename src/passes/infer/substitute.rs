use std::collections::HashSet;

use crate::{
    db::TyId,
    hir::*,
    ty::{Ty, TypeKind, TypeVar},
};

use super::InferCx;

// Substitute
impl<'db> InferCx<'db> {
    pub(crate) fn substitution(&mut self, modules: &mut [Module]) -> HashSet<TypeVar> {
        let mut unbound_vars = HashSet::new();

        for module in modules {
            for binding in &mut module.bindings {
                binding.substitute(self, &mut unbound_vars);
            }
        }

        unbound_vars
    }

    fn substitute_type_id(&mut self, id: TyId, unbound_vars: &mut HashSet<TypeVar>) {
        let ty = id.get(&self.db).clone();

        let new_ty = self.substitute_ty(&ty, unbound_vars);
        *id.get_mut(&mut self.db) = new_ty;
    }

    fn substitute_ty(&mut self, ty: &Ty, unbound_vars: &mut HashSet<TypeVar>) -> Ty {
        match &ty.kind {
            TypeKind::Fun(fun) => {
                let ret = self.substitute_ty(&fun.ret, unbound_vars);
                Ty::fun(ret, ty.span)
            }
            TypeKind::Var(v) => {
                let root = self.typecx.unification_table.find(*v);

                match self.typecx.unification_table.probe_value(root) {
                    Some(ty) => self.substitute_ty(&ty, unbound_vars),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        Ty::var(root, ty.span)
                    }
                }
            }
            _ => ty.clone(),
        }
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, cx: &mut InferCx<'db>, unbound_vars: &mut HashSet<TypeVar>);
}

impl Substitute<'_> for Hir {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        match self {
            Hir::Fun(x) => x.substitute(cx, unbound_vars),
            Hir::Block(x) => x.substitute(cx, unbound_vars),
            Hir::Ret(x) => x.substitute(cx, unbound_vars),
            Hir::Lit(_) => (),
        }

        cx.substitute_type_id(self.ty(), unbound_vars);
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
        for stmt in &mut self.exprs {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Ret {
    fn substitute(&mut self, cx: &mut InferCx<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.expr.substitute(cx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, cx: &mut InferCx<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        for item in self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, cx: &mut InferCx<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        if let Some(item) = self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, cx: &mut InferCx<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        self.as_mut().substitute(cx, unbound_vars);
    }
}
