mod infcx;
mod normalize;
mod substitute;
mod type_env;
mod unify;

use crate::{
    ast::BinOp,
    db::Db,
    diagnostics::Diagnostic,
    passes::typeck::{
        infcx::InferCtxt,
        type_env::{CallFrame, TypeEnv},
        unify::{Cause, InferError},
    },
    span::Spanned,
    tast::{
        Bin, Block, Call, CallArg, Expr, Function, If, Item, ItemKind, Lit, LitKind, Name, Return,
        TypedAst,
    },
    ty::{tyctxt::TyCtxt, FunctionType, FunctionTypeParam, Type, TypeKind, Typed},
};

pub type InferResult<T> = Result<T, InferError>;

pub fn typeck(db: &mut Db, tcx: &TyCtxt, tast: &mut TypedAst) -> Result<(), Diagnostic> {
    let mut cx = InferCtxt::new(db, tcx);

    fill_symbol_tys(&mut cx);
    infer_all(&mut cx, tast).map_err(|err| err.into_diagnostic(cx.db))?;

    cx.substitution(tast);

    Ok(())
}

fn fill_symbol_tys(infcx: &mut InferCtxt) {
    // TODO: find a less unsightly code pattern for mutating all symbols...
    for i in 0..infcx.db.symbols.len() {
        let id = i.into();
        infcx.db.symbols[id].ty = infcx.fresh_ty_var();
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
            Self::Bin(inner) => inner.infer(cx, env),
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

        self.ty = cx.tcx.types.unit;

        Ok(())
    }
}

impl Infer<'_> for Function {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        let ret_ty = cx.fresh_ty_var();

        for param in &mut self.sig.params {
            param.ty = cx.fresh_ty_var();
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
        });

        let sym_ty = cx.lookup(self.id);
        cx.at(Cause::obvious(self.span)).eq(sym_ty, Type::new(fun_ty))?;
        self.ty = sym_ty;

        env.call_stack.push(CallFrame { id: self.id, ret_ty });

        self.body.infer(cx, env)?;
        cx.at(Cause::return_ty(self.body.span, cx.db[self.id].span)).eq(ret_ty, self.body.ty)?;

        env.call_stack.pop();

        Ok(())
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.cond.infer(cx, env)?;

        let bool_ty = cx.tcx.types.bool;
        cx.at(Cause::obvious(self.cond.span())).eq(bool_ty, self.cond.ty())?;

        self.then.infer(cx, env)?;

        if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, env)?;
            cx.at(Cause::exprs(self.span, self.then.span(), otherwise.span()))
                .eq(self.then.ty(), otherwise.ty())?;
        } else {
            let unit_ty = cx.tcx.types.unit;
            cx.at(Cause::obvious(self.then.span())).eq(unit_ty, self.then.ty())?;
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

        self.ty = self.exprs.last().map_or_else(|| cx.tcx.types.unit, Expr::ty);

        Ok(())
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.ty = cx.tcx.types.never;

        let CallFrame { id, ret_ty } =
            env.call_stack.current().expect("to be inside a call frame").clone();

        self.expr.infer(cx, env)?;
        cx.at(Cause::return_ty(self.expr.span(), cx.db[id].span)).eq(ret_ty, self.expr.ty())?;

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

        let result_ty = cx.fresh_ty_var();

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
        }));

        cx.at(Cause::obvious(self.callee.span())).eq(expected_ty, self.callee.ty())?;

        self.ty = result_ty;

        Ok(())
    }
}

impl Infer<'_> for Bin {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.lhs.infer(cx, env)?;
        self.rhs.infer(cx, env)?;

        cx.at(Cause::exprs(self.span, self.lhs.span(), self.rhs.span()))
            .eq(self.lhs.ty(), self.rhs.ty())?;

        match self.op {
            BinOp::Cmp(_) => (),
            BinOp::And | BinOp::Or => {
                let bool_ty = cx.tcx.types.bool;
                cx.at(Cause::obvious(self.lhs.span())).eq(bool_ty, self.lhs.ty())?;
                cx.at(Cause::obvious(self.rhs.span())).eq(bool_ty, self.rhs.ty())?;
            }
            _ => {
                // TODO: type check arithmetic operations
            }
        }

        self.ty = match self.op {
            BinOp::Cmp(_) => cx.tcx.types.bool,
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
            LitKind::Int(_) => cx.fresh_int_var(),
            LitKind::Bool(_) => cx.tcx.types.bool,
            LitKind::Unit => cx.tcx.types.unit,
        };

        Ok(())
    }
}
