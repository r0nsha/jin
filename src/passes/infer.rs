mod constraint;
mod normalize;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use crate::db::{DefinitionId, TyId};
use crate::passes::infer::typecx::TypeCx;
use crate::ty::FunctionTy;
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
    type_env::{CallFrame, TypeEnv},
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
            let ty = self.db.alloc_ty(self.tcx.fresh_ty_var(sym.span));
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
        self.ty = cx.db.alloc_ty(Ty::Unit(self.span));

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
        let ret_ty = cx.tcx.fresh_ty_var(self.span);
        let fun_ty = Ty::Function(FunctionTy {
            ret: Box::new(ret_ty.clone()),
            span: self.span,
        });

        let ret_ty = cx.db.alloc_ty(ret_ty);
        let fun_ty = cx.db.alloc_ty(fun_ty);

        self.ty = fun_ty;

        let id = self.id.expect("to be resolved");
        env.call_stack.push(CallFrame { id, ret_ty });

        self.body.infer(cx, env);
        cx.constraints
            .push(Constraint::Eq { expected: ret_ty, actual: self.body.ty });

        env.call_stack.pop();
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
            .map_or_else(|| cx.db.alloc_ty(Ty::Unit(self.span)), Node::ty);
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Ty::Never(self.span));

        let call_frame = env.call_stack.current().unwrap();
        let ret_ty = call_frame.ret_ty;

        if let Some(value) = self.expr.as_mut() {
            value.infer(cx, env);
            cx.constraints
                .push(Constraint::Eq { expected: ret_ty, actual: value.ty() });
        } else {
            cx.constraints.push(Constraint::Eq {
                expected: ret_ty,
                actual: cx.db.alloc_ty(Ty::Unit(self.span)),
            });
        }
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.callee.infer(cx, env);

        let result_ty = cx.tcx.fresh_ty_var(self.span);
        let expected_ty = cx.db.alloc_ty(Ty::Function(FunctionTy {
            ret: Box::new(result_ty.clone()),
            span: self.span,
        }));

        cx.constraints.push(Constraint::Eq {
            expected: expected_ty,
            actual: self.callee.ty(),
        });

        self.ty = cx.db.alloc_ty(result_ty);
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
            LitKind::Int(_) => cx.db.alloc_ty(cx.tcx.fresh_int_var(self.span)),
            LitKind::Unit => cx.db.alloc_ty(Ty::Unit(self.span)),
        };
    }
}
