mod coerce;
mod error;
mod infcx;
mod instantiate;
mod normalize;
mod subst;
mod unify;

use ustr::UstrMap;

use crate::{
    ast::BinOpKind,
    db::{Db, DefId, DefKind},
    diagnostics::Diagnostic,
    hir::{self, Expr, ExprKind, Fn, FnSig, Hir, ItemKind, LitKind},
    passes::typeck::{
        coerce::CoerceExt, error::InferError, infcx::InferCtxt, instantiate::instantiate,
        normalize::NormalizeTy, unify::Obligation,
    },
    span::{Span, Spanned},
    ty::{FnTy, FnTyParam, Instantiation, ParamTy, Ty, TyKind},
};

pub type InferResult<T> = Result<T, InferError>;

pub fn typeck(db: &mut Db, hir: &mut Hir) -> Result<(), Diagnostic> {
    typeck_inner(db, hir).map_err(|err| err.into_diagnostic(db))?;
    coerce::apply_coercions(db, hir);
    Ok(())
}

fn typeck_inner(db: &mut Db, hir: &mut Hir) -> InferResult<()> {
    let mut cx = InferCtxt::new(db);

    cx.typeck_fn_sigs(hir)?;
    cx.typeck_fn_bodies(hir)?;

    cx.subst(hir);

    Ok(())
}

impl InferCtxt<'_> {
    fn typeck_fn_sigs(&mut self, hir: &mut Hir) -> InferResult<()> {
        for item in &mut hir.items {
            match &mut item.kind {
                ItemKind::Fn(fun) => {
                    fun.ty = self.typeck_fn_sig(&mut fun.sig)?;
                    self.db[fun.id].ty = fun.ty;
                }
            }
        }

        Ok(())
    }

    fn typeck_fn_sig(&mut self, sig: &mut FnSig) -> InferResult<Ty> {
        for tp in &mut sig.ty_params {
            let ty = Ty::new(TyKind::Param(ParamTy {
                name: self.db[tp.id].name,
                var: self.fresh_var(),
            }));

            self.db[tp.id].ty = self.db.types.typ;
            self.db[tp.id].kind = Box::new(DefKind::Ty(ty));
        }

        for param in &mut sig.params {
            param.ty = self.typeck_ty(&param.ty_annot)?;
            self.db[param.id].ty = param.ty;
        }

        let ret = if let Some(ret) = &sig.ret { self.typeck_ty(ret)? } else { self.db.types.unit };

        Ok(Ty::new(TyKind::Fn(FnTy {
            params: sig
                .params
                .iter()
                .map(|p| FnTyParam { name: Some(self.db[p.id].name), ty: p.ty })
                .collect(),
            ret,
        })))
    }

    fn typeck_fn_bodies(&mut self, hir: &mut Hir) -> InferResult<()> {
        for item in &mut hir.items {
            match &mut item.kind {
                ItemKind::Fn(f) => {
                    self.typeck_fn(f)?;
                }
            }
        }

        Ok(())
    }

    fn typeck_fn(&mut self, f: &mut Fn) -> InferResult<()> {
        if self.db[f.id].scope.level.is_local() {
            f.ty = self.typeck_fn_sig(&mut f.sig)?;
            self.db[f.id].ty = f.ty;
        }

        let mut fx = FnCtxt::from_fn(self.db, f);

        self.infer_expr(&mut f.body, &mut fx)?;

        let unify_body_res = self
            .at(Obligation::return_ty(
                f.body.span,
                f.sig.ret.as_ref().map_or(self.db[f.id].span, Spanned::span),
            ))
            .eq(fx.ret_ty, f.body.ty)
            .or_coerce(self, f.body.id);

        // If the function's return type is `()`, we want to let the user end the body with
        // whatever expression they want, so that they don't need to end it with a `()`
        if f.ty.as_fn().unwrap().ret.normalize(&mut self.inner.borrow_mut()).is_unit() {
            Ok(())
        } else {
            unify_body_res.map_err(Into::into)
        }
    }

    fn infer_expr(&mut self, expr: &mut Expr, fx: &mut FnCtxt) -> InferResult<()> {
        let ty = match &mut expr.kind {
            ExprKind::If(if_) => {
                self.infer_expr(&mut if_.cond, fx)?;

                self.at(Obligation::obvious(if_.cond.span))
                    .eq(self.db.types.bool, if_.cond.ty)
                    .or_coerce(self, if_.cond.id)?;

                self.infer_expr(&mut if_.then, fx)?;

                if let Some(otherwise) = if_.otherwise.as_mut() {
                    self.infer_expr(otherwise, fx)?;
                    self.at(Obligation::exprs(expr.span, if_.then.span, otherwise.span))
                        .eq(if_.then.ty, otherwise.ty)
                        .or_coerce(self, otherwise.id)?;
                } else {
                    self.at(Obligation::obvious(if_.then.span))
                        .eq(self.db.types.unit, if_.then.ty)
                        .or_coerce(self, if_.then.id)?;
                }

                if_.then.ty
            }
            ExprKind::Block(blk) => {
                for expr in &mut blk.exprs {
                    self.infer_expr(expr, fx)?;
                }

                blk.exprs.last().map_or_else(|| self.db.types.unit, |e| e.ty)
            }
            ExprKind::Return(ret) => {
                self.infer_expr(&mut ret.expr, fx)?;

                self.at(Obligation::return_ty(ret.expr.span, self.db[fx.id].span))
                    .eq(fx.ret_ty, ret.expr.ty)
                    .or_coerce(self, ret.expr.id)?;

                self.db.types.never
            }
            ExprKind::Call(call) => {
                self.infer_expr(&mut call.callee, fx)?;

                for arg in &mut call.args {
                    self.infer_expr(&mut arg.expr, fx)?;
                }

                if let TyKind::Fn(fun_ty) = call.callee.ty.kind() {
                    #[derive(Debug)]
                    struct PassedArg {
                        is_named: bool,
                        span: Span,
                    }

                    if call.args.len() != fun_ty.params.len() {
                        return Err(InferError::ArgMismatch {
                            expected: fun_ty.params.len(),
                            found: call.args.len(),
                            span: expr.span,
                        });
                    }

                    let mut already_passed_args = UstrMap::<PassedArg>::default();

                    // Resolve positional arg indices
                    for (idx, arg) in call.args.iter_mut().enumerate() {
                        if arg.name.is_none() {
                            arg.index = Some(idx);
                            already_passed_args.insert(
                                fun_ty.params[idx].name.expect("to have a name"),
                                PassedArg { is_named: false, span: arg.expr.span },
                            );
                        }
                    }

                    // Resolve named arg indices
                    for arg in &mut call.args {
                        if let Some(arg_name) = &arg.name {
                            let name = arg_name.name();

                            let idx = fun_ty
                                .params
                                .iter()
                                .enumerate()
                                .find_map(
                                    |(i, p)| if p.name == Some(name) { Some(i) } else { None },
                                )
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
                    call.args.sort_by_key(|arg| arg.index.expect("arg index to be resolved"));

                    // Unify all args with their corresponding param type
                    for arg in &call.args {
                        let idx = arg.index.expect("arg index to be resolved");
                        self.at(Obligation::obvious(arg.expr.span))
                            .eq(fun_ty.params[idx].ty, arg.expr.ty)
                            .or_coerce(self, arg.expr.id)?;
                    }

                    fun_ty.ret
                } else {
                    return Err(InferError::UncallableTy {
                        ty: call.callee.ty.normalize(&mut self.inner.borrow_mut()),
                        span: call.callee.span,
                    });
                }
            }
            ExprKind::BinOp(bin) => {
                self.infer_expr(&mut bin.lhs, fx)?;
                self.infer_expr(&mut bin.rhs, fx)?;

                self.at(Obligation::exprs(expr.span, bin.lhs.span, bin.rhs.span))
                    .eq(bin.lhs.ty, bin.rhs.ty)
                    .or_coerce(self, bin.rhs.id)?;

                match bin.op {
                    BinOpKind::Cmp(..) => (),
                    BinOpKind::And | BinOpKind::Or => {
                        self.at(Obligation::obvious(bin.lhs.span))
                            .eq(self.db.types.bool, bin.lhs.ty)
                            .or_coerce(self, bin.lhs.id)?;

                        self.at(Obligation::obvious(bin.rhs.span))
                            .eq(self.db.types.bool, bin.rhs.ty)
                            .or_coerce(self, bin.rhs.id)?;
                    }
                    _ => {
                        // TODO: type check arithmetic operations
                    }
                }

                match bin.op {
                    BinOpKind::Cmp(..) => self.db.types.bool,
                    _ => bin.lhs.ty,
                }
            }
            ExprKind::Cast(cast) => {
                todo!()
            }
            ExprKind::Name(name) => {
                let def_ty = self.lookup(name.id);
                let ty = def_ty.normalize(&mut self.inner.borrow_mut());

                let ty_params: Vec<ParamTy> =
                    ty.collect_params().into_iter().filter(|p| !fx.ty_params.contains(p)).collect();

                let instantiation: Instantiation = match &name.args {
                    Some(args) if args.len() == ty_params.len() => {
                        let arg_tys: Vec<Ty> =
                            args.iter().map(|arg| self.typeck_ty(arg)).try_collect()?;
                        ty_params
                            .into_iter()
                            .zip(arg_tys)
                            .map(|(param, arg)| (param.var, arg))
                            .collect()
                    }
                    Some(args) => {
                        return Err(InferError::TyArgMismatch {
                            expected: ty_params.len(),
                            found: args.len(),
                            span: expr.span,
                        });
                    }
                    _ => ty_params
                        .into_iter()
                        .map(|param| (param.var, self.fresh_ty_var()))
                        .collect(),
                };

                let ty = instantiate(ty, instantiation.clone());
                name.instantiation = instantiation;

                ty
            }
            ExprKind::Lit(lit) => match &lit.kind {
                LitKind::Int(..) => self.fresh_int_var(),
                LitKind::Bool(..) => self.db.types.bool,
                LitKind::Unit => self.db.types.unit,
            },
        };

        expr.ty = ty;

        Ok(())
    }

    fn typeck_ty(&mut self, ty: &hir::Ty) -> InferResult<Ty> {
        match ty {
            hir::Ty::Name(name) => {
                let def = &self.db[name.id];

                match def.kind.as_ref() {
                    DefKind::Ty(ty) => Ok(*ty),
                    _ => Err(InferError::ExpectedTy { ty: def.ty, span: name.span }),
                }
            }
            hir::Ty::Unit(_) => Ok(self.db.types.unit),
            hir::Ty::Never(_) => Ok(self.db.types.never),
            hir::Ty::Infer(_) => Ok(self.fresh_ty_var()),
        }
    }
}

struct FnCtxt {
    pub id: DefId,
    pub ret_ty: Ty,
    pub ty_params: Vec<ParamTy>,
}

impl FnCtxt {
    fn from_fn(db: &Db, fun: &Fn) -> Self {
        FnCtxt {
            id: fun.id,
            ret_ty: fun.ty.as_fn().unwrap().ret,
            ty_params: fun
                .sig
                .ty_params
                .iter()
                .map(|tp| {
                    db[tp.id]
                        .kind
                        .as_ty()
                        .expect("to be a type")
                        .as_param()
                        .expect("to be a param type")
                })
                .cloned()
                .collect(),
        }
    }
}
