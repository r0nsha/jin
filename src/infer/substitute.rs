use std::collections::HashSet;

use crate::{
    hir::{Hir, Module},
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
                    self.substitute(binding.id.get(&self.db).ty.get(&self.db));
                unbound.extend(unbound_ty);

                let unbound_binding = self.substitute_binding(binding);
                unbound.extend(unbound_binding);
            }
        }

        unbound
    }

    fn substitute(&mut self, ty: &Type) -> (HashSet<TypeVar>, Type) {
        match ty.kind {
            TypeKind::Fun(fun) => {
                let (ret_unbound, ret) = self.substitute(&fun.ret);
                (ret_unbound, Type::fun(ret, ty.span))
            }
            TypeKind::Var(v) => {
                let root = self.typecx.unification_table.find(v);

                match self.typecx.unification_table.probe_value(root) {
                    Some(ty) => self.substitute(&ty),
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

    fn substitute_hir(&mut self, hir: &mut Hir) -> HashSet<TypeVar> {
        match hir {
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

    fn substitute_binding(&mut self, binding: &mut Binding) -> HashSet<TypeVar> {
        match &mut binding.kind {
            BindingKind::Fun { name: _, fun } => self.substitute_fun(fun),
        }
    }

    fn substitute_fun(&mut self, fun: &mut Fun) -> HashSet<TypeVar> {
        // let (mut unbound, ty) = self.substitute(arg.1);
        // let arg = TypedVar(arg.0, ty);

        let unbound_body = self.substitute_hir(&mut fun.body);
        // unbound.extend(unbound_body);

        let (unbound_ty, ty) = self.substitute(fun.ty_cloned());
        fun.set_ty(ty);

        unbound_body.union(&unbound_ty).copied().collect()
    }
}
