mod constraint;
mod error;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use crate::{db::Database, hir::*, ty::*};

use self::{
    constraint::{Constraint, Constraints},
    type_env::{FunScope, TypeEnv},
    typecx::TypeCx,
};

pub(crate) fn infer(db: &mut Database, modules: &mut [Module]) {
    let mut cx = InferCx::new(db);

    cx.infer_all(modules);

    if let Err(e) = cx.unification() {
        db.diagnostics.add(e);
        return;
    }

    cx.substitution(modules);
}

pub(super) struct InferCx<'db> {
    pub(super) db: &'db mut Database,
    pub(super) typecx: TypeCx,
    pub(super) constraints: Constraints,
}

impl<'db> InferCx<'db> {
    fn new(db: &'db mut Database) -> Self {
        Self {
            db,
            typecx: TypeCx::new(),
            constraints: Constraints::new(),
        }
    }
}

impl<'db> InferCx<'db> {
    fn infer_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            self.infer_module(module);
        }
    }

    fn infer_module(&mut self, module: &mut Module) {
        let mut env = TypeEnv::new(module.id);

        for def in &mut module.definitions {
            def.infer(self, &mut env);
        }
    }
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCx<'db>, env: &mut TypeEnv);
}

impl Infer<'_> for Hir {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        match self {
            Hir::Function(x) => x.infer(cx, env),
            Hir::Block(x) => x.infer(cx, env),
            Hir::Return(x) => x.infer(cx, env),
            Hir::Lit(x) => x.infer(cx, env),
        }
    }
}

impl Infer<'_> for Definition {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Ty::alloc(&mut cx.db, Ty::unit(self.span));

        match &mut self.kind {
            DefinitionKind::Function(fun) => fun.infer(cx, env),
        }

        self.id.get_mut(&mut cx.db).ty = self.kind.ty();
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.typecx.fresh_type_var(self.span);
        let fun_ty = Ty::fun(ret_ty.clone(), self.span);

        let ret_ty = Ty::alloc(&mut cx.db, ret_ty);

        let ty = Ty::alloc(&mut cx.db, fun_ty);

        self.ty = ty;
        self.id.get_mut(&mut cx.db).ty = ty;

        env.fun_scopes.push(FunScope {
            id: self.id,
            ret_ty,
        });

        self.body.infer(cx, env);
        cx.constraints.push(Constraint::Eq {
            expected: ret_ty,
            actual: self.body.ty,
        });

        env.fun_scopes.pop();
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        let mut constraints = Constraints::new();

        for expr in &mut self.exprs {
            expr.infer(cx, env);
        }

        self.ty = self.exprs.last().map_or_else(
            || Ty::alloc(&mut cx.db, Ty::unit(self.span)),
            |expr| expr.ty(),
        );
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Ty::alloc(&mut cx.db, Ty::never(self.span));

        let fun_scope = env.fun_scopes.current().unwrap();
        let ret_ty = fun_scope.ret_ty;

        if let Some(value) = self.expr.as_mut() {
            value.infer(cx, env);
            cx.constraints.push(Constraint::Eq {
                expected: ret_ty,
                actual: value.ty(),
            });
        } else {
            cx.constraints.push(Constraint::Eq {
                expected: ret_ty,
                actual: Ty::alloc(&mut cx.db, Ty::unit(self.span)),
            });
        }
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => {
                // TODO: use a polymorphic int
                Ty::alloc(&mut cx.db, Ty::int(self.span))
            }
            LitKind::Unit => Ty::alloc(&mut cx.db, Ty::unit(self.span)),
        };
    }
}
