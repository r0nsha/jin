mod error;
mod infcx;
mod instantiate;
mod normalize;
mod substitute;
mod unify;

use std::collections::HashMap;

use ustr::UstrMap;

use crate::{
    ast::BinOp,
    db::{Db, SymbolId},
    diagnostics::Diagnostic,
    passes::typeck::{
        error::InferError, infcx::InferCtxt, instantiate::instantiate, normalize::NormalizeTy,
        unify::Obligation,
    },
    span::{Span, Spanned},
    tast::{
        Bin, Block, Call, Expr, Fn, FnSig, If, Item, ItemKind, Lit, LitKind, Name, Return, TypedAst,
    },
    ty::{tcx::TyCtxt, FnTy, FnTyParam, ParamTy, Ty, TyKind, Typed},
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

    fn typeck_function_sig(&mut self, sig: &mut FnSig) -> Ty {
        let ret_ty = self.fresh_ty_var();

        for (index, param) in sig.params.iter_mut().enumerate() {
            // param.ty = self.fresh_ty_var();
            param.ty =
                Ty::new(TyKind::Param(ParamTy { name: ustr::ustr(&format!("T{index}")), index }));
            self.db[param.id].ty = param.ty;
        }

        Ty::new(TyKind::Fn(FnTy {
            params: sig
                .params
                .iter()
                .map(|p| FnTyParam { name: Some(self.db[p.id].name), ty: p.ty })
                .collect(),
            ret: ret_ty,
        }))
    }

    fn typeck_function_bodies(&mut self, tast: &mut TypedAst) -> InferResult<()> {
        for item in &mut tast.items {
            match &mut item.kind {
                ItemKind::Function(fun) => {
                    let mut fx = FnCtxt::from_function(fun);
                    fun.infer(self, &mut fx)?;
                }
            }
        }

        Ok(())
    }
}

struct FnCtxt {
    pub id: SymbolId,
    pub ret_ty: Ty,
    pub param_env: HashMap<usize, Ty>,
}

impl FnCtxt {
    fn from_function(fun: &Fn) -> Self {
        FnCtxt { id: fun.id, ret_ty: fun.ty.as_fn().unwrap().ret, param_env: HashMap::new() }
    }
}

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCtxt<'db>, fx: &mut FnCtxt) -> InferResult<()>;
}

impl Infer<'_> for Expr {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        match self {
            Self::Item(inner) => inner.infer(cx, fx),
            Self::If(inner) => inner.infer(cx, fx),
            Self::Block(inner) => inner.infer(cx, fx),
            Self::Return(inner) => inner.infer(cx, fx),
            Self::Call(inner) => inner.infer(cx, fx),
            Self::Bin(inner) => inner.infer(cx, fx),
            Self::Name(inner) => inner.infer(cx, fx),
            Self::Lit(inner) => inner.infer(cx, fx),
        }
    }
}

impl Infer<'_> for Item {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        match &mut self.kind {
            ItemKind::Function(fun) => fun.infer(cx, fx)?,
        }

        self.ty = cx.tcx.types.unit;

        Ok(())
    }
}

impl Infer<'_> for Fn {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _fx: &mut FnCtxt) -> InferResult<()> {
        if cx.db[self.id].scope.level.is_local() {
            self.ty = cx.typeck_function_sig(&mut self.sig);
            cx.db[self.id].ty = self.ty;
        }

        let mut fx = FnCtxt::from_function(self);

        self.body.infer(cx, &mut fx)?;

        cx.at(Obligation::return_ty(self.body.span, cx.db[self.id].span))
            .eq(fx.ret_ty, self.body.ty)?;

        Ok(())
    }
}

impl Infer<'_> for If {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        self.cond.infer(cx, fx)?;

        cx.at(Obligation::obvious(self.cond.span())).eq(cx.tcx.types.bool, self.cond.ty())?;

        self.then.infer(cx, fx)?;

        if let Some(otherwise) = self.otherwise.as_mut() {
            otherwise.infer(cx, fx)?;
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
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        for expr in &mut self.exprs {
            expr.infer(cx, fx)?;
        }

        self.ty = self.exprs.last().map_or_else(|| cx.tcx.types.unit, Expr::ty);

        Ok(())
    }
}

impl Infer<'_> for Return {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        self.ty = cx.tcx.types.never;

        self.expr.infer(cx, fx)?;
        cx.at(Obligation::return_ty(self.expr.span(), cx.db[fx.id].span))
            .eq(fx.ret_ty, self.expr.ty())?;

        Ok(())
    }
}

#[derive(Debug)]
struct PassedArg {
    is_named: bool,
    span: Span,
}

impl Infer<'_> for Call {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        self.callee.infer(cx, fx)?;

        for arg in &mut self.args {
            arg.expr.infer(cx, fx)?;
        }

        self.ty = if let TyKind::Fn(fun_ty) = self.callee.ty().kind() {
            if self.args.len() != fun_ty.params.len() {
                return Err(InferError::ArgMismatch {
                    expected: fun_ty.params.len(),
                    found: self.args.len(),
                    span: self.span,
                });
            }

            let mut already_passed_args = UstrMap::<PassedArg>::default();

            // Resolve positional arg indices
            for (idx, arg) in self.args.iter_mut().enumerate() {
                if arg.name.is_none() {
                    arg.index = Some(idx);
                    already_passed_args.insert(
                        fun_ty.params[idx].name.expect("to have a name"),
                        PassedArg { is_named: false, span: arg.expr.span() },
                    );
                }
            }

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

                    // Report named arguments that are passed twice
                    if let Some(passed_arg) = already_passed_args.insert(
                        arg_name.name(),
                        PassedArg { is_named: true, span: arg_name.span() },
                    ) {
                        return Err(InferError::MultipleNamedArgs {
                            name: arg_name.name(),
                            prev: passed_arg.span,
                            dup: arg_name.span(),
                            is_named: passed_arg.is_named,
                        });
                    }

                    arg.index = Some(idx);
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
            return Err(InferError::UncallableTy {
                ty: self.callee.ty().normalize(&mut cx.inner.borrow_mut()),
                span: self.callee.span(),
            });
        };

        Ok(())
    }
}

impl Infer<'_> for Bin {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        self.lhs.infer(cx, fx)?;
        self.rhs.infer(cx, fx)?;

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
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _env: &mut FnCtxt) -> InferResult<()> {
        let ty = cx.lookup(self.id).normalize(&mut cx.inner.borrow_mut());

        let args = ty.collect_params().into_iter().map(|_| cx.fresh_ty_var()).collect();
        let instantiated = instantiate(ty, args);

        self.ty = instantiated;

        Ok(())
    }
}

impl Infer<'_> for Lit {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _env: &mut FnCtxt) -> InferResult<()> {
        self.ty = match &self.kind {
            LitKind::Int(..) => cx.fresh_int_var(),
            LitKind::Bool(..) => cx.tcx.types.bool,
            LitKind::Unit => cx.tcx.types.unit,
        };

        Ok(())
    }
}
