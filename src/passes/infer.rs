mod constraint;
mod normalize;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use self::{
    constraint::{Constraint, Constraints},
    type_env::{CallFrame, TypeEnv},
};
use crate::{
    ast::BinaryOp,
    db::{Database, DefinitionId, TyId},
    hir::{
        Binary, Block, Call, Definition, DefinitionKind, Expr, Function, Hir, If, Lit, LitKind,
        Module, Name, Return,
    },
    passes::infer::typecx::TypeCx,
    ty::{FunctionTy, Ty},
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

    fn get_definition_info_ty(&mut self, id: DefinitionId) -> TyId {
        let def = &self.db[id];

        if def.ty.is_null() {
            let ty = self.db.alloc_ty(self.tcx.fresh_ty_var(def.span));
            self.db[id].ty = ty;
            ty
        } else {
            def.ty
        }
    }
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCx<'db>, env: &mut TypeEnv);
}

impl Infer<'_> for Expr {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        match self {
            Self::Function(inner) => inner.infer(cx, env),
            Self::If(inner) => inner.infer(cx, env),
            Self::Block(inner) => inner.infer(cx, env),
            Self::Return(inner) => inner.infer(cx, env),
            Self::Call(inner) => inner.infer(cx, env),
            Self::Binary(inner) => inner.infer(cx, env),
            Self::Name(inner) => inner.infer(cx, env),
            Self::Lit(inner) => inner.infer(cx, env),
        }
    }
}

impl Infer<'_> for Definition {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Ty::Unit(self.span));

        match &mut self.kind {
            DefinitionKind::Function(fun) => fun.infer(cx, env),
        }

        let def_ty = cx.get_definition_info_ty(self.id.expect("to be resolved"));

        cx.constraints.push(Constraint::Eq { expected: self.kind.ty(), actual: def_ty });
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.tcx.fresh_ty_var(self.span);
        let fun_ty = Ty::Function(FunctionTy { ret: Box::new(ret_ty.clone()), span: self.span });

        let ret_ty = cx.db.alloc_ty(ret_ty);

        self.ty = cx.db.alloc_ty(fun_ty);

        let id = self.id.expect("to be resolved");
        env.call_stack.push(CallFrame { id, ret_ty });

        self.body.infer(cx, env);
        cx.constraints.push(Constraint::Eq { expected: ret_ty, actual: self.body.ty });

        env.call_stack.pop();
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        todo!()
        // self.ty = self.exprs.last().map_or_else(|| cx.db.alloc_ty(Ty::Unit(self.span)), Expr::ty);
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        for expr in &mut self.exprs {
            expr.infer(cx, env);
        }

        self.ty = self.exprs.last().map_or_else(|| cx.db.alloc_ty(Ty::Unit(self.span)), Expr::ty);
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Ty::Never(self.span));

        let call_frame = env.call_stack.current().unwrap();
        let ret_ty = call_frame.ret_ty;

        self.expr.infer(cx, env);
        cx.constraints.push(Constraint::Eq { expected: ret_ty, actual: self.expr.ty() });
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

        cx.constraints.push(Constraint::Eq { expected: expected_ty, actual: self.callee.ty() });

        self.ty = cx.db.alloc_ty(result_ty);
    }
}

impl Infer<'_> for Binary {
    fn infer(&mut self, cx: &mut InferCx<'_>, env: &mut TypeEnv) {
        self.lhs.infer(cx, env);
        self.rhs.infer(cx, env);

        cx.constraints.push(Constraint::Eq { expected: self.lhs.ty(), actual: self.rhs.ty() });

        self.ty = match self.op {
            BinaryOp::Cmp(_) => cx.db.alloc_ty(Ty::Bool(self.span)),
            _ => self.lhs.ty(),
        };
    }
}

impl Infer<'_> for Name {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = cx.get_definition_info_ty(self.id.expect("to be resolved"));
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => cx.db.alloc_ty(cx.tcx.fresh_int_var(self.span)),
            LitKind::Bool(_) => cx.db.alloc_ty(Ty::Bool(self.span)),
            LitKind::Unit => cx.db.alloc_ty(Ty::Unit(self.span)),
        };
    }
}
