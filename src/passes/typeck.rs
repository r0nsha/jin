mod error;
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
        error::InferError,
        infcx::InferCtxt,
        type_env::{CallFrame, TypeEnv},
        unify::Obligation,
    },
    span::Spanned,
    tast::{
        Bin, Block, Call, Expr, Function, FunctionSig, If, Item, ItemKind, Lit, LitKind, Name,
        Return, TypedAst,
    },
    ty::{tcx::TyCtxt, FunctionTy, FunctionTyParam, Ty, TyKind, Typed},
};

pub type InferResult<T> = Result<T, InferError>;

pub fn typeck(db: &mut Db, tcx: &TyCtxt, tast: &mut TypedAst) -> Result<(), Diagnostic> {
    typeck_inner(db, tcx, tast).map_err(|err| err.into_diagnostic(db))
}

fn typeck_inner(db: &mut Db, tcx: &TyCtxt, tast: &mut TypedAst) -> InferResult<()> {
    let mut cx = InferCtxt::new(db, tcx);

    cx.typeck_function_signatures(tast);
    cx.typeck_function_bodies(tast)?;

    cx.substitute_all(tast);

    Ok(())
}

impl InferCtxt<'_> {
    fn typeck_function_signatures(&mut self, tast: &mut TypedAst) {
        for item in &mut tast.items {
            match &mut item.kind {
                ItemKind::Function(fun) => {
                    fun.ty = self.typeck_function_sig(&mut fun.sig);
                    self.db[fun.id].ty = fun.ty;
                }
            }
        }
    }

    fn typeck_function_sig(&mut self, sig: &mut FunctionSig) -> Ty {
        let ret_ty = self.fresh_ty_var();

        for param in &mut sig.params {
            param.ty = self.fresh_ty_var();
            self.db[param.id].ty = param.ty;
        }

        Ty::new(TyKind::Function(FunctionTy {
            ret: ret_ty,
            params: sig
                .params
                .iter()
                .map(|param| FunctionTyParam { name: Some(self.db[param.id].name), ty: param.ty })
                .collect(),
        }))
    }

    fn typeck_function_bodies(&mut self, tast: &mut TypedAst) -> InferResult<()> {
        let mut env = TypeEnv::new();

        for item in &mut tast.items {
            item.infer(self, &mut env)?;
        }

        Ok(())
    }
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
        if cx.db[self.id].scope.level.is_local() {
            self.ty = cx.typeck_function_sig(&mut self.sig);
            cx.db[self.id].ty = self.ty;
        }

        let ret_ty = self.ty.as_function().unwrap().ret;

        env.call_stack.push(CallFrame { id: self.id, ret_ty });

        self.body.infer(cx, env)?;
        cx.at(Obligation::return_ty(self.body.span, cx.db[self.id].span))
            .eq(ret_ty, self.body.ty)?;

        env.call_stack.pop();

        Ok(())
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.cond.infer(cx, env)?;

        cx.at(Obligation::obvious(self.cond.span())).eq(cx.tcx.types.bool, self.cond.ty())?;

        self.then.infer(cx, env)?;

        if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, env)?;
            cx.at(Obligation::exprs(self.span, self.then.span(), otherwise.span()))
                .eq(self.then.ty(), otherwise.ty())?;
        } else {
            cx.at(Obligation::obvious(self.then.span())).eq(cx.tcx.types.unit, self.then.ty())?;
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
        cx.at(Obligation::return_ty(self.expr.span(), cx.db[id].span))
            .eq(ret_ty, self.expr.ty())?;

        Ok(())
    }
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.callee.infer(cx, env)?;

        for arg in &mut self.args {
            arg.expr.infer(cx, env)?;
        }

        self.ty = if let TyKind::Function(fun_ty) = self.callee.ty().as_ref() {
            if self.args.len() != fun_ty.params.len() {
                return Err(InferError::ArgMismatch {
                    expected: fun_ty.params.len(),
                    found: self.args.len(),
                    span: self.span,
                });
            }

            // Generate a mapping from (arg index -> availability)
            let mut idx_availability = (0..self.args.len()).map(|_| true).collect::<Vec<_>>();

            // Resolve named arg indices
            for arg in &mut self.args {
                if let Some(arg_name) = &arg.name {
                    let name = arg_name.name();

                    let idx = fun_ty
                        .params
                        .iter()
                        .enumerate()
                        .find_map(|(i, p)| if p.name == Some(name) { Some(i) } else { None })
                        .ok_or(InferError::NamedParamNotFound { word: *arg_name })?;

                    arg.index = Some(idx);
                    idx_availability[idx] = false;
                }
            }

            // Resolve positional arg indices
            let mut pos_idx = 0;

            for arg in &mut self.args {
                if arg.name.is_none() {
                    pos_idx = if let Some(avail_idx) =
                        idx_availability.iter().enumerate().find_map(|(i, avail)| avail.then(|| i))
                    {
                        idx_availability[avail_idx] = false;
                        avail_idx
                    } else {
                        pos_idx + 1
                    };

                    arg.index = Some(pos_idx);
                }
            }

            // Sort args by index
            self.args.sort_by_key(|arg| arg.index.expect("arg index to be resolved"));

            // Unify all args with their corresponding param type
            for arg in &self.args {
                let idx = arg.index.expect("arg index to be resolved");
                cx.at(Obligation::obvious(arg.expr.span()))
                    .eq(fun_ty.params[idx].ty, arg.expr.ty())?;
            }

            fun_ty.ret
        } else {
            // Unresolved callee type, unify with a generic function type
            let result_ty = cx.fresh_ty_var();

            let expected_ty = Ty::new(TyKind::Function(FunctionTy {
                ret: result_ty,
                params: self
                    .args
                    .iter()
                    .map(|arg| FunctionTyParam {
                        name: arg.name.map(|n| n.name()),
                        ty: arg.expr.ty(),
                    })
                    .collect(),
            }));

            cx.at(Obligation::obvious(self.callee.span())).eq(expected_ty, self.callee.ty())?;

            result_ty
        };

        Ok(())
    }
}

impl Infer<'_> for Bin {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, env: &mut TypeEnv) -> InferResult<()> {
        self.lhs.infer(cx, env)?;
        self.rhs.infer(cx, env)?;

        cx.at(Obligation::exprs(self.span, self.lhs.span(), self.rhs.span()))
            .eq(self.lhs.ty(), self.rhs.ty())?;

        match self.op {
            BinOp::Cmp(..) => (),
            BinOp::And | BinOp::Or => {
                cx.at(Obligation::obvious(self.lhs.span())).eq(cx.tcx.types.bool, self.lhs.ty())?;
                cx.at(Obligation::obvious(self.rhs.span())).eq(cx.tcx.types.bool, self.rhs.ty())?;
            }
            _ => {
                // TODO: type check arithmetic operations
            }
        }

        self.ty = match self.op {
            BinOp::Cmp(..) => cx.tcx.types.bool,
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
            LitKind::Int(..) => cx.fresh_int_var(),
            LitKind::Bool(..) => cx.tcx.types.bool,
            LitKind::Unit => cx.tcx.types.unit,
        };

        Ok(())
    }
}
