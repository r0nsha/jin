mod constraint;
mod error;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use crate::{db::Database, diagnostics::Diagnostic, hir::*, ty::*};

use self::{
    constraint::{Constraint, Constraints},
    type_env::{FunScope, TypeEnv},
    typecx::TypeCx,
};

pub(crate) fn infer(db: &mut Database, modules: &mut [Module]) -> Result<(), Diagnostic> {
    let mut cx = InferCx::new(db);

    cx.infer_all(modules);
    cx.unification()?;
    cx.substitution(modules);

    Ok(())
}

pub(super) struct InferCx<'a> {
    pub(super) db: &'a mut Database,
    pub(super) typecx: TypeCx,
    pub(super) constraints: Constraints,
}

impl<'a> InferCx<'a> {
    fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            typecx: TypeCx::new(),
            constraints: Constraints::new(),
        }
    }
}

impl<'a> InferCx<'a> {
    fn infer_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            self.infer_module(module);
        }
    }

    fn infer_module(&mut self, module: &mut Module) {
        let mut env = TypeEnv::new(module.id);

        for binding in &mut module.bindings {
            binding.infer(self, &mut env);
        }
    }
}

trait Infer<'a> {
    fn infer(&mut self, cx: &mut InferCx<'a>, env: &mut TypeEnv);
}

impl Infer<'_> for Hir {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        match self {
            Hir::Fun(x) => x.infer(cx, env),
            Hir::Block(x) => x.infer(cx, env),
            Hir::Ret(x) => x.infer(cx, env),
            Hir::Lit(x) => x.infer(cx, env),
        }
    }
}

impl Infer<'_> for Binding {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Type::alloc(&mut cx.db, Type::unit(self.span));
        self.expr.infer(cx, env);
        self.id.get_mut(&mut cx.db).ty = self.expr.ty();
    }
}

impl Infer<'_> for Fun {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.typecx.fresh_type_var(self.span);
        let fun_ty = Type::fun(ret_ty.clone(), self.span);

        let ret_ty = Type::alloc(&mut cx.db, ret_ty);

        let ty = Type::alloc(&mut cx.db, fun_ty);

        self.ty = ty;
        self.id.get_mut(&mut cx.db).ty = ty;

        env.fun_scopes.push(FunScope {
            id: self.id,
            ret_ty,
        });

        self.body.infer(cx, env);
        cx.constraints.push(Constraint::Eq {
            expected: ret_ty,
            actual: self.body.ty(),
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
            || Type::alloc(&mut cx.db, Type::unit(self.span)),
            |expr| expr.ty(),
        );
    }
}

impl Infer<'_> for Ret {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Type::alloc(&mut cx.db, Type::never(self.span));

        if let Some(fun_scope) = env.fun_scopes.current() {
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
                    actual: Type::alloc(&mut cx.db, Type::unit(self.span)),
                });
            }
        } else {
            // Err(CheckError::MisplacedReturn { span: ret.span });
            todo!()
        }
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => {
                // TODO: use a polymorphic int
                Type::alloc(&mut cx.db, Type::int(self.span))
            }
            LitKind::Unit => Type::alloc(&mut cx.db, Type::unit(self.span)),
        };
    }
}
