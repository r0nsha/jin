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
        let mut unbound = HashSet::new();

        for module in modules {
            for binding in &mut module.bindings {
                let (unbound_ty, _binding_ty) =
                    self.substitute_ty(binding.id.get(&self.db).ty.get(&self.db));
                unbound.extend(unbound_ty);

                unbound.extend(binding.substitute(self));
            }
        }

        unbound
    }

    fn substitute_type_id(&mut self, id: TypeId) -> HashSet<TypeVar> {
        let ty = id.get(&self.db);

        let (unbound, new_ty) = self.substitute_ty(ty);
        *id.get_mut(&mut self.db) = new_ty;

        unbound
    }

    fn substitute_ty(&mut self, ty: &Type) -> (HashSet<TypeVar>, Type) {
        match ty.kind {
            TypeKind::Fun(fun) => {
                let (ret_unbound, ret) = self.substitute_ty(&fun.ret);
                (ret_unbound, Type::fun(ret, ty.span))
            }
            TypeKind::Var(v) => {
                let root = self.typecx.unification_table.find(v);

                match self.typecx.unification_table.probe_value(root) {
                    Some(ty) => self.substitute_ty(&ty),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        (unbound, Type::var(root, ty.span))
                    }
                }
            }
            _ => (HashSet::new(), ty.clone()),
        }
    }
}

trait Substitute<'a> {
    fn substitute(&mut self, cx: &mut InferCx<'a>) -> HashSet<TypeVar>;
}

impl Substitute<'_> for Hir {
    fn substitute(&mut self, cx: &mut InferCx<'_>) -> HashSet<TypeVar> {
        match self {
            Hir::Fun(x) => x.substitute(),
            Hir::Ret(x) => x.substitute(),
            Hir::Lit(x) => x.substitute(),
            // {
            // let (unbound_ty, ty) = self.substitute(hir.ty().get(&self.db));
            // hir.set_ty(ty);
            // unbound_ty
            // }
        }
    }
}

impl Substitute<'_> for Binding {
    fn substitute(&mut self, cx: &mut InferCx<'_>) -> HashSet<TypeVar> {
        let unbound = cx.substitute_type_id(self.id.get(&cx.db).ty);
        unbound
    }
}

impl Substitute<'_> for Fun {
    fn substitute(&mut self, cx: &mut InferCx<'_>) -> HashSet<TypeVar> {
        let unbound_body = self.body.substitute(cx);

        let (unbound_ty, ty) = self.substitute_ty(fun.ty_cloned());
        fun.set_ty(ty);

        unbound_body.union(&unbound_ty).copied().collect()
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut InferCx<'_>) -> HashSet<TypeVar> {
        let unbound_body = self.body.substitute(cx);
    }
}
