mod constraint;
mod normalize;
mod substitute;
mod type_env;
mod unify;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::{
    ast::BinaryOp,
    db::{Database, SymbolId, TypeId},
    passes::typeck::{
        constraint::{Constraint, Constraints},
        type_env::{CallFrame, TypeEnv},
    },
    span::{Span, Spanned},
    tast::{
        Binary, Block, Call, CallArg, Expr, Function, If, Item, ItemKind, Lit, LitKind, Name,
        Return, TypedAst,
    },
    ty::{FunctionType, FunctionTypeParam, InferType, IntVar, IntVarValue, Type, TypeVar, Typed},
};

pub fn typeck(db: &mut Database, tast: &mut TypedAst) {
    let mut cx = TypeCx::new(db);

    cx.fill_symbol_tys();
    cx.infer_all(tast);

    if let Err(e) = cx.unification() {
        db.diagnostics.add(e.into_diagnostic(db));
        return;
    }

    cx.substitution(tast);
}

pub struct TypeCx<'db> {
    pub db: &'db mut Database,
    pub ty_unification_table: InPlaceUnificationTable<TypeVar>,
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
    fn fill_symbol_tys(&mut self) {
        // TODO: find a less unsightly code pattern for mutating all symbols...
        for i in 0..self.db.symbols.len() {
            let id = i.into();
            self.db.symbols[id].ty = self.alloc_ty_var(self.db.symbols[id].span);
        }
    }

    fn infer_all(&mut self, tast: &mut TypedAst) {
        let mut env = TypeEnv::new();

        for item in &mut tast.items {
            item.infer(self, &mut env);
        }
    }

    fn lookup(&mut self, id: SymbolId) -> TypeId {
        let sym = &self.db[id];
        assert!(!sym.ty.is_null(), "symbol `{}` wasn't assigned a TypeId", sym.qname);
        sym.ty
    }

    #[inline]
    pub fn fresh_ty_var(&mut self, span: Span) -> Type {
        Type::Infer(InferType::TypeVar(self.ty_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_ty_var(&mut self, span: Span) -> TypeId {
        let ftv = self.fresh_ty_var(span);
        self.db.alloc_ty(ftv)
    }

    #[inline]
    pub fn fresh_int_var(&mut self, span: Span) -> Type {
        Type::Infer(InferType::IntVar(self.int_unification_table.new_key(None)), span)
    }

    #[inline]
    pub fn alloc_int_var(&mut self, span: Span) -> TypeId {
        let ftv = self.fresh_int_var(span);
        self.db.alloc_ty(ftv)
    }

    #[inline]
    pub fn add_eq_constraint(&mut self, expected: TypeId, actual: TypeId) {
        self.constraints.push(Constraint::Eq { expected, actual });
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
        match &mut self.kind {
            ItemKind::Function(fun) => fun.infer(cx, env),
        }

        self.ty = cx.db.alloc_ty(Type::Unit(self.span()));
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        let ret_ty = cx.alloc_ty_var(self.span);

        for param in &mut self.sig.params {
            param.ty = cx.alloc_ty_var(param.span);
            cx.db[param.id].ty = param.ty;
        }

        let fun_ty = Type::Function(FunctionType {
            ret: Box::new(cx.db[ret_ty].clone()),
            params: self
                .sig
                .params
                .iter()
                .map(|param| FunctionTypeParam {
                    name: Some(cx.db[param.id].name),
                    ty: cx.db[param.ty].clone(),
                })
                .collect(),
            span: self.span,
        });

        self.ty = cx.db.alloc_ty(fun_ty);

        let sym_ty = cx.lookup(self.id);
        cx.add_eq_constraint(sym_ty, self.ty);

        env.call_stack.push(CallFrame { id: self.id, ret_ty });

        self.body.infer(cx, env);
        cx.add_eq_constraint(ret_ty, self.body.ty);

        env.call_stack.pop();
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.cond.infer(cx, env);

        let expected = cx.db.alloc_ty(Type::Bool(self.cond.span()));
        cx.add_eq_constraint(expected, self.cond.ty());

        self.then.infer(cx, env);

        let other_ty = if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, env);
            otherwise.ty()
        } else {
            cx.db.alloc_ty(Type::Unit(self.span))
        };

        cx.add_eq_constraint(self.then.ty(), other_ty);

        self.ty = self.then.ty();
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        for expr in &mut self.exprs {
            expr.infer(cx, env);
        }

        self.ty = self.exprs.last().map_or_else(|| cx.db.alloc_ty(Type::Unit(self.span)), Expr::ty);
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.ty = cx.db.alloc_ty(Type::Never(self.span));

        let call_frame = env.call_stack.current().expect("to be inside a call frame");
        let ret_ty = call_frame.ret_ty;

        self.expr.infer(cx, env);
        cx.add_eq_constraint(ret_ty, self.expr.ty());
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.callee.infer(cx, env);

        for arg in &mut self.args {
            match arg {
                CallArg::Positional(expr) | CallArg::Named(_, expr) => expr.infer(cx, env),
            }
        }

        let result_ty = cx.fresh_ty_var(self.span);

        let expected_ty = cx.db.alloc_ty(Type::Function(FunctionType {
            ret: Box::new(result_ty.clone()),
            params: self
                .args
                .iter()
                .map(|arg| match arg {
                    CallArg::Positional(expr) => {
                        FunctionTypeParam { name: None, ty: cx.db[expr.ty()].clone() }
                    }
                    CallArg::Named(name, expr) => {
                        FunctionTypeParam { name: Some(name.name()), ty: cx.db[expr.ty()].clone() }
                    }
                })
                .collect(),
            span: self.span,
        }));

        cx.add_eq_constraint(expected_ty, self.callee.ty());

        self.ty = cx.db.alloc_ty(result_ty);
    }
}

impl Infer<'_> for Binary {
    fn infer(&mut self, cx: &mut TypeCx<'_>, env: &mut TypeEnv) {
        self.lhs.infer(cx, env);
        self.rhs.infer(cx, env);

        cx.add_eq_constraint(self.lhs.ty(), self.rhs.ty());

        match self.op {
            BinaryOp::Cmp(_) => (),
            BinaryOp::And | BinaryOp::Or => {
                let expected = cx.db.alloc_ty(Type::Bool(self.span));
                cx.add_eq_constraint(expected, self.lhs.ty());
                cx.add_eq_constraint(expected, self.rhs.ty());
            }
            _ => {
                let expected = cx.alloc_int_var(self.span);
                cx.add_eq_constraint(expected, self.lhs.ty());
                cx.add_eq_constraint(expected, self.rhs.ty());
            }
        }

        self.ty = match self.op {
            BinaryOp::Cmp(_) => cx.db.alloc_ty(Type::Bool(self.span)),
            _ => self.lhs.ty(),
        };
    }
}

impl Infer<'_> for Name {
    fn infer(&mut self, cx: &mut TypeCx<'_>, _env: &mut TypeEnv) {
        self.ty = cx.lookup(self.id);
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut TypeCx<'_>, _env: &mut TypeEnv) {
        self.ty = match &self.kind {
            LitKind::Int(_) => cx.alloc_int_var(self.span),
            LitKind::Bool(_) => cx.db.alloc_ty(Type::Bool(self.span)),
            LitKind::Unit => cx.db.alloc_ty(Type::Unit(self.span)),
        };
    }
}

impl UnifyKey for TypeVar {
    type Value = Option<Type>;

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

impl EqUnifyValue for Type {}

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
