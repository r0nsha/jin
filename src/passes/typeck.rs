mod apply_adjustments;
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
    hir::{self, Expr, ExprKind, Fn, FnSig, Hir, Item, ItemKind, LitKind},
    passes::typeck::{
        error::InferError, infcx::InferCtxt, instantiate::instantiate, normalize::NormalizeTy,
        unify::Obligation,
    },
    span::{Span, Spanned},
    ty::{tcx::TyCtxt, FnTy, FnTyParam, Instantiation, ParamTy, Ty, TyKind},
};

pub type InferResult<T> = Result<T, InferError>;

pub fn typeck(db: &mut Db, tcx: &mut TyCtxt, hir: &mut Hir) -> Result<(), Diagnostic> {
    typeck_inner(db, tcx, hir).map_err(|err| err.into_diagnostic(db))?;
    // TODO:
    // apply_adjustments(db, tcx, hir);
    Ok(())
}

fn typeck_inner(db: &mut Db, tcx: &mut TyCtxt, hir: &mut Hir) -> InferResult<()> {
    let mut cx = InferCtxt::new(db, tcx);

    cx.typeck_function_signatures(hir)?;
    cx.typeck_function_bodies(hir)?;

    cx.subst(hir);

    Ok(())
}

impl InferCtxt<'_> {
    fn typeck_function_signatures(&mut self, hir: &mut Hir) -> InferResult<()> {
        for item in &mut hir.items {
            match &mut item.kind {
                ItemKind::Fn(fun) => {
                    fun.ty = self.typeck_function_sig(&mut fun.sig)?;
                    self.db[fun.id].ty = fun.ty;
                }
            }
        }

        Ok(())
    }

    fn typeck_function_sig(&mut self, sig: &mut FnSig) -> InferResult<Ty> {
        for tp in &mut sig.ty_params {
            let ty = Ty::new(TyKind::Param(ParamTy {
                name: self.db[tp.id].name,
                var: self.fresh_var(),
            }));

            self.db[tp.id].ty = self.tcx.types.typ;
            self.db[tp.id].kind = Box::new(DefKind::Ty(ty));
        }

        for param in &mut sig.params {
            param.ty = self.typeck_ty(&param.ty_annot)?;
            self.db[param.id].ty = param.ty;
        }

        let ret = if let Some(ret) = &sig.ret { self.typeck_ty(ret)? } else { self.tcx.types.unit };

        Ok(Ty::new(TyKind::Fn(FnTy {
            params: sig
                .params
                .iter()
                .map(|p| FnTyParam { name: Some(self.db[p.id].name), ty: p.ty })
                .collect(),
            ret,
        })))
    }

    fn typeck_function_bodies(&mut self, hir: &mut Hir) -> InferResult<()> {
        for item in &mut hir.items {
            match &mut item.kind {
                ItemKind::Fn(fun) => {
                    let mut fx = FnCtxt::from_fn(self.db, fun);
                    fun.infer(self, &mut fx)?;
                }
            }
        }

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
            hir::Ty::Unit(_) => Ok(self.tcx.types.unit),
            hir::Ty::Never(_) => Ok(self.tcx.types.never),
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

trait Infer<'db> {
    fn infer(&mut self, cx: &mut InferCtxt<'db>, fx: &mut FnCtxt) -> InferResult<()>;
}

impl Infer<'_> for Expr {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        let ty = match &mut self.kind {
            ExprKind::If(if_) => {
                if_.cond.infer(cx, fx)?;

                cx.at(Obligation::obvious(if_.cond.span)).eq(cx.tcx.types.bool, if_.cond.ty)?;

                if_.then.infer(cx, fx)?;

                if let Some(otherwise) = if_.otherwise.as_mut() {
                    otherwise.infer(cx, fx)?;
                    cx.at(Obligation::exprs(self.span, if_.then.span, otherwise.span))
                        .eq(if_.then.ty, otherwise.ty)?;
                } else {
                    cx.at(Obligation::obvious(if_.then.span)).eq(cx.tcx.types.unit, if_.then.ty)?;
                }

                if_.then.ty
            }
            ExprKind::Block(blk) => {
                for expr in &mut blk.exprs {
                    expr.infer(cx, fx)?;
                }

                blk.exprs.last().map_or_else(|| cx.tcx.types.unit, |e| e.ty)
            }
            ExprKind::Return(ret) => {
                ret.expr.infer(cx, fx)?;

                cx.at(Obligation::return_ty(ret.expr.span, cx.db[fx.id].span))
                    .eq(fx.ret_ty, ret.expr.ty)?;

                cx.tcx.types.never
            }
            ExprKind::Call(call) => {
                call.callee.infer(cx, fx)?;

                for arg in &mut call.args {
                    arg.expr.infer(cx, fx)?;
                }

                if let TyKind::Fn(fun_ty) = call.callee.ty.kind() {
                    if call.args.len() != fun_ty.params.len() {
                        return Err(InferError::ArgMismatch {
                            expected: fun_ty.params.len(),
                            found: call.args.len(),
                            span: self.span,
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
                        cx.at(Obligation::obvious(arg.expr.span))
                            .eq(fun_ty.params[idx].ty, arg.expr.ty)?;
                    }

                    fun_ty.ret
                } else {
                    return Err(InferError::UncallableTy {
                        ty: call.callee.ty.normalize(&mut cx.inner.borrow_mut()),
                        span: call.callee.span,
                    });
                }
            }
            ExprKind::Bin(bin) => {
                bin.lhs.infer(cx, fx)?;
                bin.rhs.infer(cx, fx)?;

                cx.at(Obligation::exprs(self.span, bin.lhs.span, bin.rhs.span))
                    .eq(bin.lhs.ty, bin.rhs.ty)?;

                match bin.op {
                    BinOpKind::Cmp(..) => (),
                    BinOpKind::And | BinOpKind::Or => {
                        cx.at(Obligation::obvious(bin.lhs.span))
                            .eq(cx.tcx.types.bool, bin.lhs.ty)?;
                        cx.at(Obligation::obvious(bin.rhs.span))
                            .eq(cx.tcx.types.bool, bin.rhs.ty)?;
                    }
                    _ => {
                        // TODO: type check arithmetic operations
                    }
                }

                match bin.op {
                    BinOpKind::Cmp(..) => cx.tcx.types.bool,
                    _ => bin.lhs.ty,
                }
            }
            ExprKind::Name(name) => {
                let def_ty = cx.lookup(name.id);
                let ty = def_ty.normalize(&mut cx.inner.borrow_mut());

                let ty_params: Vec<ParamTy> =
                    ty.collect_params().into_iter().filter(|p| !fx.ty_params.contains(p)).collect();

                let instantiation: Instantiation = match &name.args {
                    Some(args) if args.len() == ty_params.len() => {
                        let arg_tys: Vec<Ty> =
                            args.iter().map(|arg| cx.typeck_ty(arg)).try_collect()?;
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
                            span: self.span,
                        });
                    }
                    _ => {
                        ty_params.into_iter().map(|param| (param.var, cx.fresh_ty_var())).collect()
                    }
                };

                let ty = instantiate(ty, instantiation.clone());
                name.instantiation = instantiation;

                ty
            }
            ExprKind::Lit(lit) => match &lit.kind {
                LitKind::Int(..) => cx.fresh_int_var(),
                LitKind::Bool(..) => cx.tcx.types.bool,
                LitKind::Unit => cx.tcx.types.unit,
            },
        };

        self.ty = ty;

        Ok(())
    }
}

// TODO: inline
impl Infer<'_> for Item {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, fx: &mut FnCtxt) -> InferResult<()> {
        match &mut self.kind {
            ItemKind::Fn(fun) => fun.infer(cx, fx)?,
        }

        self.ty = cx.tcx.types.unit;

        Ok(())
    }
}

impl Infer<'_> for Fn {
    fn infer(&mut self, cx: &mut InferCtxt<'_>, _fx: &mut FnCtxt) -> InferResult<()> {
        if cx.db[self.id].scope.level.is_local() {
            self.ty = cx.typeck_function_sig(&mut self.sig)?;
            cx.db[self.id].ty = self.ty;
        }

        let mut fx = FnCtxt::from_fn(cx.db, self);

        self.body.infer(cx, &mut fx)?;

        let unify_body_res = cx
            .at(Obligation::return_ty(
                self.body.span,
                self.sig.ret.as_ref().map_or(cx.db[self.id].span, Spanned::span),
            ))
            .eq(fx.ret_ty, self.body.ty);

        // If the function's return type is `()`, we want to let the user end the body with
        // whatever expression they want, so that they don't need to end it with a `()`
        if self.ty.as_fn().unwrap().ret.normalize(&mut cx.inner.borrow_mut()).is_unit() {
            Ok(())
        } else {
            unify_body_res
        }
    }
}

#[derive(Debug)]
struct PassedArg {
    is_named: bool,
    span: Span,
}
