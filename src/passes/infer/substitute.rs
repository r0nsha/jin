use std::collections::HashSet;

use crate::{
    db::TyId,
    hir::*,
    ty::{Ty, TyKind, TyVar},
};

use super::InferCx;

// Substitute
impl<'db> InferCx<'db> {
    pub(crate) fn substitution(
        &mut self,
        modules: &mut [Module],
    ) -> HashSet<TyVar> {
        let mut unbound_vars = HashSet::new();

        for module in modules {
            for def in &mut module.definitions {
                def.substitute(self, &mut unbound_vars);
            }
        }

        unbound_vars
    }

    fn substitute_type_id(
        &mut self,
        id: TyId,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        let ty = id.get(self.db).clone();

        let new_ty = self.substitute_ty(&ty, unbound_vars);
        *id.get_mut(self.db) = new_ty;
    }

    fn substitute_ty(
        &mut self,
        ty: &Ty,
        unbound_vars: &mut HashSet<TyVar>,
    ) -> Ty {
        match &ty.kind {
            TyKind::Function(fun) => {
                let ret = self.substitute_ty(&fun.ret, unbound_vars);
                Ty::fun(ret, ty.span)
            }
            TyKind::Var(v) => {
                let root = self.tcx.unification_table.find(*v);

                match self.tcx.unification_table.probe_value(root) {
                    Some(ty) => self.substitute_ty(&ty, unbound_vars),
                    None => {
                        unbound_vars.insert(root);
                        Ty::var(root, ty.span)
                    }
                }
            }
            _ => ty.clone(),
        }
    }
}

trait Substitute<'db> {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'db>,
        unbound_vars: &mut HashSet<TyVar>,
    );
}

impl Substitute<'_> for Node {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        match self {
            Node::Function(x) => x.substitute(cx, unbound_vars),
            Node::Block(x) => x.substitute(cx, unbound_vars),
            Node::Return(x) => x.substitute(cx, unbound_vars),
            Node::Call(x) => x.substitute(cx, unbound_vars),
            Node::Name(_) | Node::Lit(_) => (),
        }

        cx.substitute_type_id(self.ty(), unbound_vars);
    }
}

impl Substitute<'_> for Definition {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        match &mut self.kind {
            DefinitionKind::Function(fun) => fun.substitute(cx, unbound_vars),
        }

        cx.substitute_type_id(self.ty, unbound_vars);
        cx.substitute_type_id(
            self.id.expect("to be resolved").get(cx.db).ty,
            unbound_vars,
        );
    }
}

impl Substitute<'_> for Function {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        self.body.substitute(cx, unbound_vars);
        cx.substitute_type_id(
            self.id.expect("to be resolved").get(cx.db).ty,
            unbound_vars,
        );
    }
}

impl Substitute<'_> for Block {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        for stmt in &mut self.exprs {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        self.expr.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'_>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        self.callee.substitute(cx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'db>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        for item in self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'db>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        if let Some(item) = self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(
        &mut self,
        cx: &mut InferCx<'db>,
        unbound_vars: &mut HashSet<TyVar>,
    ) {
        self.as_mut().substitute(cx, unbound_vars);
    }
}
