mod constraint;
mod infcx;
mod normalize;
mod substitute;
mod type_env;
mod unify;

use crate::{
    ast::BinaryOp,
    db::Db,
    diagnostics::Diagnostic,
    passes::typeck::{
        infcx::InferCtxt,
        type_env::{CallFrame, TypeEnv},
        unify::InferError,
    },
    span::Spanned,
    tast::{
        Binary, Block, Call, CallArg, Expr, Function, If, Item, ItemKind, Lit, LitKind, Name,
        Return, TypedAst,
    },
    ty::{FunctionType, FunctionTypeParam, Type, TypeKind, Typed},
};

pub type InferResult<T> = Result<T, InferError>;

pub fn typeck(db: &mut Db, tast: &mut TypedAst) -> Result<(), Diagnostic> {
    let mut cx = InferCtxt::new(db);

    fill_symbol_tys(&mut cx);
    infer_all(&mut cx, tast).map_err(|err| err.into_diagnostic(cx.db))?;

    cx.substitution(tast);

    Ok(())
}

fn fill_symbol_tys(infcx: &mut InferCtxt) {
    // TODO: find a less unsightly code pattern for mutating all symbols...
    for i in 0..infcx.db.symbols.len() {
        let id = i.into();
        infcx.db.symbols[id].ty = infcx.fresh_ty_var(infcx.db.symbols[id].span);
    }
}

fn infer_all(infcx: &mut InferCtxt, tast: &mut TypedAst) -> InferResult<()> {
    let mut env = TypeEnv::new();

    for item in &mut tast.items {
        item.infer(infcx, &mut env)?;
    }

    Ok(())
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCtxt<'db>, env: &mut TypeEnv) -> InferResult<()>;
}

impl Infer<'_> for Expr {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
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
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        match &mut self.kind {
            ItemKind::Function(fun) => fun.infer(cx, env)?,
        }

        self.ty = Type::new(TypeKind::Unit(self.span()));

        Ok(())
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        let ret_ty = cx.fresh_ty_var(self.span);

        for param in &mut self.sig.params {
            param.ty = cx.fresh_ty_var(param.span);
            cx.db[param.id].ty = param.ty;
        }

        let fun_ty = TypeKind::Function(FunctionType {
            ret: ret_ty,
            params: self
                .sig
                .params
                .iter()
                .map(|param| FunctionTypeParam { name: Some(cx.db[param.id].name), ty: param.ty })
                .collect(),
            span: self.span,
        });

        let sym_ty = cx.lookup(self.id);
        cx.at(self.span).eq(sym_ty, Type::new(fun_ty))?;
        self.ty = sym_ty;

        env.call_stack.push(CallFrame { id: self.id, ret_ty });

        self.body.infer(cx, env)?;
        cx.at(self.body.span).eq(ret_ty, self.body.ty)?;

        env.call_stack.pop();

        Ok(())
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.cond.infer(cx, env)?;

        cx.at(self.cond.span()).eq(Type::new(TypeKind::Bool(self.cond.span())), self.cond.ty())?;

        self.then.infer(cx, env)?;

        if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, env)?;
            cx.at(otherwise.span()).eq(self.then.ty(), otherwise.ty())?;
        } else {
            cx.at(self.then.span()).eq(self.then.ty(), Type::new(TypeKind::Unit(self.span)))?;
        }

        self.ty = self.then.ty();

        Ok(())
    }
}

impl Infer<'_> for Block {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        for expr in &mut self.exprs {
            expr.infer(cx, env)?;
        }

        self.ty = self.exprs.last().map_or_else(|| Type::new(TypeKind::Unit(self.span)), Expr::ty);

        Ok(())
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.ty = Type::new(TypeKind::Never(self.span));

        let call_frame = env.call_stack.current().expect("to be inside a call frame");
        let ret_ty = call_frame.ret_ty;

        self.expr.infer(cx, env)?;
        cx.at(self.expr.span()).eq(ret_ty, self.expr.ty())?;

        Ok(())
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.callee.infer(cx, env)?;

        for arg in &mut self.args {
            match arg {
                CallArg::Positional(expr) | CallArg::Named(_, expr) => expr.infer(cx, env)?,
            }
        }

        let result_ty = cx.fresh_ty_var(self.span);

        let expected_ty = Type::new(TypeKind::Function(FunctionType {
            ret: result_ty,
            params: self
                .args
                .iter()
                .map(|arg| match arg {
                    CallArg::Positional(expr) => FunctionTypeParam { name: None, ty: expr.ty() },
                    CallArg::Named(name, expr) => {
                        FunctionTypeParam { name: Some(name.name()), ty: expr.ty() }
                    }
                })
                .collect(),
            span: self.span,
        }));

        cx.at(self.callee.span()).eq(expected_ty, self.callee.ty())?;

        self.ty = result_ty;

        Ok(())
    }
}

impl Infer<'_> for Binary {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.lhs.infer(cx, env)?;
        self.rhs.infer(cx, env)?;

        cx.at(self.rhs.span()).eq(self.lhs.ty(), self.rhs.ty())?;

        match self.op {
            BinaryOp::Cmp(_) => (),
            BinaryOp::And | BinaryOp::Or => {
                let expected = Type::new(TypeKind::Bool(self.span));
                cx.at(self.lhs.span()).eq(expected, self.lhs.ty())?;
                cx.at(self.rhs.span()).eq(expected, self.rhs.ty())?;
            }
            _ => {
                // TODO: type check arithmetic operations
            }
        }

        self.ty = match self.op {
            BinaryOp::Cmp(_) => Type::new(TypeKind::Bool(self.span)),
            _ => self.lhs.ty(),
        };

        Ok(())
    }
}

impl Infer<'_> for Name {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _env: &mut TypeEnv) -> InferResult<()> {
        self.ty = cx.lookup(self.id);
        Ok(())
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _env: &mut TypeEnv) -> InferResult<()> {
        self.ty = match &self.kind {
            LitKind::Int(_) => cx.fresh_int_var(self.span),
            LitKind::Bool(_) => Type::new(TypeKind::Bool(self.span)),
            LitKind::Unit => Type::new(TypeKind::Unit(self.span)),
        };

        Ok(())
    }
}
