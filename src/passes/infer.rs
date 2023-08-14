mod constraint;
mod normalize;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use crate::db::{DefinitionId, TyId};
use crate::{
    db::Database,
    hir::{
        Block, Call, Definition, DefinitionKind, Function, Hir, Lit, LitKind,
        Module, Name, Node, Return,
    },
    ty::Ty,
};

use self::{
    constraint::{Constraint, Constraints},
    type_env::{FunScope, TypeEnv},
    typecx::TypeCx,
};

pub fn infer(db: &mut Database, hir: &mut Hir) {
    let mut cx = InferCx::new(db);

    cx.infer_all(&mut hir.modules);

    if let Err(e) = cx.unification() {
        db.diagnostics.add(e.into_diagnostic(db));
        return;
    }

    cx.substitution(&mut hir.modules);
}

pub(super) struct InferCx<'db> {
    pub(super) db: &'db mut Database,
    pub(super) tcx: TypeCx,
    pub(super) constraints: Constraints,
}

impl<'db> InferCx<'db> {
    fn new(db: &'db mut Database) -> Self {
        Self { db, tcx: TypeCx::new(), constraints: Constraints::new() }
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

    fn infer_definition(&mut self, id: DefinitionId) -> TyId {
        let sym = &self.db[id];

        if sym.ty.is_null() {
            let ty = Ty::alloc(self.db, self.tcx.fresh_type_var(sym.span));
            self.db[id].ty = ty;
            ty
        } else {
            sym.ty
        }
    }
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCx<'db>, env: &mut TypeEnv);
}

impl Infer<'_> for Node {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        match self {
            Self::Function(x) => x.infer(cx, env),
            Self::Block(x) => x.infer(cx, env),
            Self::Return(x) => x.infer(cx, env),
            Self::Call(x) => x.infer(cx, env),
            Self::Name(x) => x.infer(cx, env),
            Self::Lit(x) => x.infer(cx, env),
        }
    }
}

impl Infer<'_> for Definition {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Ty::alloc(cx.db, Ty::unit(self.span));

        match &mut self.kind {
            DefinitionKind::Function(fun) => fun.infer(cx, env),
        }

        let sym_ty = cx.infer_definition(self.id.expect("to be resolved"));

        cx.constraints
            .push(Constraint::Eq { expected: self.kind.ty(), actual: sym_ty });
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.tcx.fresh_type_var(self.span);
        let fun_ty = Ty::fun(ret_ty.clone(), self.span);

        let ret_ty = Ty::alloc(cx.db, ret_ty);
        let fun_ty = Ty::alloc(cx.db, fun_ty);

        self.ty = fun_ty;

        let id = self.id.expect("to be resolved");
        env.fun_scopes.push(FunScope { id, ret_ty });

        self.body.infer(cx, env);
        cx.constraints
            .push(Constraint::Eq { expected: ret_ty, actual: self.body.ty });

        env.fun_scopes.pop();
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        for expr in &mut self.exprs {
            expr.infer(cx, env);
        }

        self.ty = self
            .exprs
            .last()
            .map_or_else(|| Ty::alloc(cx.db, Ty::unit(self.span)), Node::ty);
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = Ty::alloc(cx.db, Ty::never(self.span));

        let fun_scope = env.fun_scopes.current().unwrap();
        let ret_ty = fun_scope.ret_ty;

        if let Some(value) = self.expr.as_mut() {
            value.infer(cx, env);
            cx.constraints
                .push(Constraint::Eq { expected: ret_ty, actual: value.ty() });
        } else {
            cx.constraints.push(Constraint::Eq {
                expected: ret_ty,
                actual: Ty::alloc(cx.db, Ty::unit(self.span)),
            });
        }
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.callee.infer(cx, env);

        let result_ty = cx.tcx.fresh_type_var(self.span);
        let expected_ty =
            Ty::alloc(cx.db, Ty::fun(result_ty.clone(), self.span));

        cx.constraints.push(Constraint::Eq {
            expected: expected_ty,
            actual: self.callee.ty(),
        });

        self.ty = Ty::alloc(cx.db, result_ty);
    }
}

impl Infer<'_> for Name {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = cx.infer_definition(self.id.expect("to be resolved"));
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => {
                // TODO: use a polymorphic int
                Ty::alloc(cx.db, Ty::int(self.span))
            }
            LitKind::Unit => Ty::alloc(cx.db, Ty::unit(self.span)),
        };
    }
}
