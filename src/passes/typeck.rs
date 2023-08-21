mod constraint;
mod normalize;
mod substitute;
mod type_env;
mod unify;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::{
    ast::BinaryOp,
    db::{Database, SymbolId, TyId},
    hir::{
        Binary, Block, Call, CallArg, Expr, Function, Hir, If, Item, ItemKind, Lit, LitKind,
        Module, Name, Return,
    },
    passes::typeck::{
        constraint::{Constraint, Constraints},
        type_env::{CallFrame, TypeEnv},
    },
    span::{Span, Spanned},
    ty::{FunctionParamTy, FunctionTy, InferTy, IntVar, IntVarValue, Ty, TyVar, Typed},
};

pub fn typeck(db: &mut Database, hir: &mut Hir) {
    let mut cx = TypeCx::new(db);

    cx.infer_all(&mut hir.modules);

    if let Err(e) = cx.unification() {
        db.diagnostics.add(e.into_diagnostic(db));
        return;
    }

    cx.substitution(&mut hir.modules);
}

pub struct TypeCx<'db> {
    pub db: &'db mut Database,
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
    pub constraints: Constraints,
}

impl<'db> TypeCx<'db> {
    fn new(db: &'db mut Database) -> Self {
        Self {
            db,
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
            constraints: Constraints::new(),
        }
    }
}

impl<'db> TypeCx<'db> {
    fn infer_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            self.infer_module(module);
        }
    }

    fn infer_module(&mut self, module: &mut Module) {
        let mut env = TypeEnv::new(module.id);

        for item in &mut module.items {
            item.infer(self, &mut env);
        }
    }

    fn lookup(&mut self, id: SymbolId) -> TyId {
        let symbol = &self.db[id];

        if symbol.ty.is_null() {
            let ty = self.alloc_ty_var(symbol.span);
            self.db[id].ty = ty;
            ty
        } else {
            symbol.ty
        }
    }

    #[inline]
    pub fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::Infer(InferTy::TyVar(self.ty_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_ty_var(&mut self, span: Span) -> TyId {
        let ftv = self.fresh_ty_var(span);
        self.db.alloc_ty(ftv)
    }

    #[inline]
    pub fn fresh_int_var(&mut self, span: Span) -> Ty {
        Ty::Infer(InferTy::IntVar(self.int_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_int_var(&mut self, span: Span) -> TyId {
        let ftv = self.fresh_int_var(span);
        self.db.alloc_ty(ftv)
    }
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut TypeCx<'db>, env: &mut TypeEnv);
}

impl Infer<'_> for Expr {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        match self {
            Self::Item(inner) => inner.infer(cx, env),
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

impl Infer<'_> for Item {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Ty::Unit(self.span));

        match &mut self.kind {
            ItemKind::Function(fun) => fun.infer(cx, env),
        }

        let symbol_ty = cx.lookup(self.id.expect("to be resolved"));

        cx.constraints.push(Constraint::Eq { expected: self.kind.ty(), actual: symbol_ty });
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.alloc_ty_var(self.span);

        for param in &mut self.params {
            param.ty = cx.alloc_ty_var(param.span);
        }

        let fun_ty = Ty::Function(FunctionTy {
            ret: Box::new(cx.db[ret_ty].clone()),
            params: self
                .params
                .iter()
                .map(|param| FunctionParamTy {
                    name: Some(param.name.name()),
                    ty: cx.db[param.ty].clone(),
                })
                .collect(),
            span: self.span,
        });

        self.ty = cx.db.alloc_ty(fun_ty);

        let id = self.id.expect("to be resolved");
        env.call_stack.push(CallFrame { id, ret_ty });

        self.body.infer(cx, env);
        cx.constraints.push(Constraint::Eq { expected: ret_ty, actual: self.body.ty });

        env.call_stack.pop();
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.cond.infer(cx, env);

        cx.constraints.push(Constraint::Eq {
            expected: cx.db.alloc_ty(Ty::Bool(self.cond.span())),
            actual: self.cond.ty(),
        });

        self.then.infer(cx, env);

        let other_ty = if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, env);
            otherwise.ty()
        } else {
            cx.db.alloc_ty(Ty::Unit(self.span))
        };

        cx.constraints.push(Constraint::Eq { expected: self.then.ty(), actual: other_ty });

        self.ty = self.then.ty();
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        for expr in &mut self.exprs {
            expr.infer(cx, env);
        }

        self.ty = self.exprs.last().map_or_else(|| cx.db.alloc_ty(Ty::Unit(self.span)), Expr::ty);
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Ty::Never(self.span));

        let call_frame = env.call_stack.current().unwrap();
        let ret_ty = call_frame.ret_ty;

        self.expr.infer(cx, env);
        cx.constraints.push(Constraint::Eq { expected: ret_ty, actual: self.expr.ty() });
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.callee.infer(cx, env);

        let result_ty = cx.fresh_ty_var(self.span);

        for arg in &mut self.args {
            match arg {
                CallArg::Positional(expr) | CallArg::Named(_, expr) => expr.infer(cx, env),
            }
        }

        let expected_ty = cx.db.alloc_ty(Ty::Function(FunctionTy {
            ret: Box::new(result_ty.clone()),
            params: self
                .args
                .iter()
                .map(|arg| match arg {
                    CallArg::Positional(expr) => {
                        FunctionParamTy { name: None, ty: cx.db[expr.ty()].clone() }
                    }
                    CallArg::Named(name, expr) => {
                        FunctionParamTy { name: Some(name.name()), ty: cx.db[expr.ty()].clone() }
                    }
                })
                .collect(),
            span: self.span,
        }));

        cx.constraints.push(Constraint::Eq { expected: expected_ty, actual: self.callee.ty() });

        self.ty = cx.db.alloc_ty(result_ty);
    }
}

impl Infer<'_> for Binary {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.lhs.infer(cx, env);
        self.rhs.infer(cx, env);

        cx.constraints.push(Constraint::Eq { expected: self.lhs.ty(), actual: self.rhs.ty() });

        match self.op {
            BinaryOp::Cmp(_) => (),
            BinaryOp::And | BinaryOp::Or => {
                let expected = cx.db.alloc_ty(Ty::Bool(self.span));
                cx.constraints.push(Constraint::Eq { expected, actual: self.lhs.ty() });
                cx.constraints.push(Constraint::Eq { expected, actual: self.rhs.ty() });
            }
            _ => {
                let expected = cx.alloc_int_var(self.span);
                cx.constraints.push(Constraint::Eq { expected, actual: self.lhs.ty() });
                cx.constraints.push(Constraint::Eq { expected, actual: self.rhs.ty() });
            }
        }

        self.ty = match self.op {
            BinaryOp::Cmp(_) => cx.db.alloc_ty(Ty::Bool(self.span)),
            _ => self.lhs.ty(),
        };
    }
}

impl Infer<'_> for Name {
    fn infer(&mut self, cx: &mut TypeCx<'_>, _env: &mut TypeEnv) {
        self.ty = cx.lookup(self.id.expect("to be resolved"));
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut TypeCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => cx.alloc_int_var(self.span),
            LitKind::Bool(_) => cx.db.alloc_ty(Ty::Bool(self.span)),
            LitKind::Unit => cx.db.alloc_ty(Ty::Unit(self.span)),
        };
    }
}

impl UnifyKey for TyVar {
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

impl EqUnifyValue for Ty {}

impl UnifyKey for IntVar {
    type Value = Option<IntVarValue>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "IntTy"
    }
}

impl EqUnifyValue for IntVarValue {}
