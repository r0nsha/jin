mod attrs;
mod coerce;
mod error;
mod instantiate;
mod normalize;
mod subst;
mod tcx;
mod unify;

use ustr::UstrMap;

use crate::{
    ast::{BinOp, UnOp},
    db::{Db, DefId, DefKind},
    diagnostics::Diagnostic,
    hir::{self, Expr, ExprKind, Fn, FnKind, FnSig, Hir, Let, Lit, Pat},
    span::{Span, Spanned},
    sym,
    ty::{FnTy, FnTyParam, Instantiation, ParamTy, Ty, TyKind},
    typeck::{
        coerce::CoerceExt,
        error::TypeckError,
        instantiate::instantiate,
        tcx::{Env, TyCtxt},
        unify::Obligation,
    },
};

pub type TypeckResult<T> = Result<T, TypeckError>;

pub fn typeck(db: &mut Db, hir: &mut Hir) -> Result<(), Diagnostic> {
    fn inner(db: &mut Db, hir: &mut Hir) -> TypeckResult<()> {
        let mut cx = TyCtxt::new(db);

        TyCtxt::report_cyclic_global_variables(hir)?;

        cx.typeck_defs(hir)?;
        cx.typeck_bodies(hir)?;
        cx.subst(hir);

        Ok(())
    }

    inner(db, hir).map_err(|err| err.into_diagnostic(db))
}

impl TyCtxt<'_> {
    fn report_cyclic_global_variables(hir: &Hir) -> TypeckResult<()> {
        fn inner(hir: &Hir, let_: &Let, source_ids: &[DefId]) -> Option<(DefId, Span)> {
            let mut found_cycle = None;

            let_.value.walk(|expr| match &expr.kind {
                ExprKind::Name(name) if source_ids.contains(&name.id) => {
                    found_cycle = Some((let_.pat.ids()[0], expr.span));
                }
                ExprKind::Name(name) => {
                    if let Some(let_) =
                        hir.lets.iter().find(|let_| let_.pat.any(|n| n.id == name.id))
                    {
                        found_cycle = inner(hir, let_, source_ids);
                    }
                }
                _ => (),
            });

            found_cycle
        }

        for let_ in &hir.lets {
            let ids = let_.pat.ids();

            if let Some((cyclic_id, cause_span)) = inner(hir, let_, &ids) {
                return Err(TypeckError::CyclicGlobalVars {
                    source: ids[0],
                    cyclic: cyclic_id,
                    cause_span,
                });
            }
        }

        Ok(())
    }

    fn typeck_defs(&mut self, hir: &mut Hir) -> TypeckResult<()> {
        for f in &mut hir.fns {
            let ty = self.typeck_fn_sig(&mut f.sig)?;
            self.db[f.id].ty = ty;
        }

        for let_ in &mut hir.lets {
            match &let_.pat {
                Pat::Name(name) => self.db[name.id].ty = self.fresh_ty_var(),
                Pat::Ignore(_) => (),
            }
        }

        for let_ in &mut hir.extern_lets {
            self.typeck_attrs(let_.module_id, &mut let_.attrs)?;
            self.db[let_.id].ty = self.typeck_ty(&let_.ty_annot)?;
        }

        Ok(())
    }

    fn typeck_fn_sig(&mut self, sig: &mut FnSig) -> TypeckResult<Ty> {
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

    fn typeck_bodies(&mut self, hir: &mut Hir) -> TypeckResult<()> {
        for let_ in &mut hir.lets {
            self.typeck_let(let_)?;
        }

        for f in &mut hir.fns {
            self.typeck_fn(f)?;
        }

        Ok(())
    }

    fn typeck_fn(&mut self, f: &mut Fn) -> TypeckResult<()> {
        if self.db[f.id].scope.level.is_local() {
            let ty = self.typeck_fn_sig(&mut f.sig)?;
            self.db[f.id].ty = ty;
        }

        let mut env = Env::new(f.module_id, Some(f.id));

        self.typeck_attrs(env.module_id(), &mut f.attrs)?;

        let ret_ty = self.db[f.id].ty.as_fn().unwrap().ret;

        match &mut f.kind {
            FnKind::Bare { body } => {
                self.typeck_expr(body, &mut env, Some(ret_ty))?;

                let unify_body_res = self
                    .at(Obligation::return_ty(
                        body.span,
                        f.sig.ret.as_ref().map_or(self.db[f.id].span, Spanned::span),
                    ))
                    .eq(ret_ty, body.ty)
                    .or_coerce(self, body.id);

                // If the function's return type is `()`, we want to let the user end the body with
                // whatever expression they want, so that they don't need to end it with a `()`
                if self.normalize(ret_ty).is_unit() {
                    Ok(())
                } else {
                    unify_body_res.map_err(Into::into)
                }
            }
            FnKind::Extern => Ok(()),
        }
    }

    fn typeck_let(&mut self, let_: &mut Let) -> TypeckResult<Ty> {
        let mut env = Env::new(let_.module_id, None);

        self.typeck_attrs(env.module_id(), &mut let_.attrs)?;

        let ty =
            if let Some(ty) = &let_.ty_annot { self.typeck_ty(ty)? } else { self.fresh_ty_var() };

        self.typeck_expr(&mut let_.value, &mut env, Some(ty))?;

        self.at(Obligation::obvious(let_.value.span))
            .eq(ty, let_.value.ty)
            .or_coerce(self, let_.value.id)?;

        match &let_.pat {
            Pat::Name(name) => {
                self.db[name.id].ty = ty;

                if let Some(value) = self.db.const_storage.expr(let_.value.id) {
                    self.db.const_storage.insert_def(name.id, value.clone());
                }
            }
            Pat::Ignore(_) => (),
        }

        Ok(ty)
    }

    fn typeck_expr(
        &mut self,
        expr: &mut Expr,
        env: &mut Env,
        expected_ty: Option<Ty>,
    ) -> TypeckResult<()> {
        expr.ty = match &mut expr.kind {
            ExprKind::Let(let_) => {
                let ty = self.typeck_let(let_)?;

                match &let_.pat {
                    Pat::Name(name) => self.db[name.id].ty = ty,
                    Pat::Ignore(_) => (),
                }

                self.db.types.unit
            }
            ExprKind::If(if_) => {
                self.typeck_expr(&mut if_.cond, env, Some(self.db.types.bool))?;

                self.at(Obligation::obvious(if_.cond.span))
                    .eq(self.db.types.bool, if_.cond.ty)
                    .or_coerce(self, if_.cond.id)?;

                self.typeck_expr(&mut if_.then, env, expected_ty)?;

                if let Some(otherwise) = if_.otherwise.as_mut() {
                    self.typeck_expr(otherwise, env, Some(if_.then.ty))?;
                    self.at(Obligation::exprs(expr.span, if_.then.span, otherwise.span))
                        .eq(if_.then.ty, otherwise.ty)
                        .or_coerce(self, otherwise.id)?;

                    if_.then.ty
                } else {
                    // NOTE: We don't unify here since, since we allow non-unit blocks to
                    // _become_ unit blocks, meaning that a block that doesn't return a unit value,
                    // but is expected to - is assumed to return it anyways.
                    if_.then.ty = self.db.types.unit;

                    self.db.types.unit
                }
            }
            ExprKind::Block(blk) => {
                if blk.exprs.is_empty() {
                    self.db.types.unit
                } else {
                    let last = blk.exprs.len() - 1;

                    for (i, expr) in blk.exprs.iter_mut().enumerate() {
                        let expected_ty =
                            if i == last { expected_ty } else { Some(self.db.types.unit) };
                        self.typeck_expr(expr, env, expected_ty)?;
                    }

                    blk.exprs.last().unwrap().ty
                }
            }
            ExprKind::Return(ret) => {
                if let Some(fn_id) = env.fn_id() {
                    let ret_ty = self.db[fn_id].ty.as_fn().unwrap().ret;

                    self.typeck_expr(&mut ret.expr, env, Some(ret_ty))?;

                    self.at(Obligation::return_ty(ret.expr.span, self.db[fn_id].span))
                        .eq(ret_ty, ret.expr.ty)
                        .or_coerce(self, ret.expr.id)?;

                    self.db.types.never
                } else {
                    return Err(TypeckError::InvalidReturn(expr.span));
                }
            }
            ExprKind::Call(call) => {
                self.typeck_expr(&mut call.callee, env, None)?;

                for arg in &mut call.args {
                    self.typeck_expr(&mut arg.expr, env, None)?;
                }

                if let TyKind::Fn(fun_ty) = call.callee.ty.kind() {
                    #[derive(Debug)]
                    struct PassedArg {
                        is_named: bool,
                        span: Span,
                    }

                    if call.args.len() != fun_ty.params.len() {
                        return Err(TypeckError::ArgMismatch {
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
                                .ok_or(TypeckError::NamedParamNotFound { word: *arg_name })?;

                            // Report named arguments that are passed twice
                            if let Some(passed_arg) = already_passed_args.insert(
                                arg_name.name(),
                                PassedArg { is_named: true, span: arg_name.span() },
                            ) {
                                return Err(TypeckError::MultipleNamedArgs {
                                    name: arg_name.name(),
                                    prev: passed_arg.span,
                                    dup: arg_name.span(),
                                    is_named: passed_arg.is_named,
                                });
                            }

                            arg.index = Some(idx);
                        }
                    }

                    // Unify all args with their corresponding param type
                    for arg in &call.args {
                        let idx = arg.index.expect("arg index to be resolved");
                        self.at(Obligation::obvious(arg.expr.span))
                            .eq(fun_ty.params[idx].ty, arg.expr.ty)
                            .or_coerce(self, arg.expr.id)?;
                    }

                    fun_ty.ret
                } else {
                    // TODO: assume a function here?
                    return Err(TypeckError::UncallableTy {
                        ty: self.normalize(call.callee.ty),
                        span: call.callee.span,
                    });
                }
            }
            ExprKind::Unary(un) => {
                self.typeck_expr(&mut un.expr, env, None)?;

                match un.op {
                    UnOp::Neg => {
                        // TODO: Only allow signed integers (need traits)
                        self.at(Obligation::obvious(un.expr.span))
                            .eq(self.fresh_int_var(), un.expr.ty)
                            .or_coerce(self, un.expr.id)?;
                    }
                    UnOp::Not => {
                        // TODO: Allow bitnot (integers, need traits)
                        self.at(Obligation::obvious(un.expr.span))
                            .eq(self.db.types.bool, un.expr.ty)
                            .or_coerce(self, un.expr.id)?;
                    }
                }

                un.expr.ty
            }
            ExprKind::Binary(bin) => {
                self.typeck_expr(&mut bin.lhs, env, None)?;
                self.typeck_expr(&mut bin.rhs, env, Some(bin.lhs.ty))?;

                self.at(Obligation::exprs(expr.span, bin.lhs.span, bin.rhs.span))
                    .eq(bin.lhs.ty, bin.rhs.ty)
                    .or_coerce(self, bin.rhs.id)?;

                match bin.op {
                    BinOp::And | BinOp::Or => {
                        self.at(Obligation::obvious(bin.lhs.span))
                            .eq(self.db.types.bool, bin.lhs.ty)
                            .or_coerce(self, bin.lhs.id)?;

                        self.at(Obligation::obvious(bin.rhs.span))
                            .eq(self.db.types.bool, bin.rhs.ty)
                            .or_coerce(self, bin.rhs.id)?;
                    }
                    _ => {
                        // TODO: type check arithmetic operations (traits)
                        // TODO: type check cmp operations (traits)
                    }
                }

                match bin.op {
                    BinOp::Cmp(..) => self.db.types.bool,
                    _ => bin.lhs.ty,
                }
            }
            ExprKind::Cast(cast) => {
                self.typeck_expr(&mut cast.expr, env, None)?;
                self.typeck_ty(&cast.target)?
            }
            ExprKind::MemberAccess(access) => {
                self.typeck_expr(&mut access.expr, env, None)?;

                let ty = self.normalize(access.expr.ty);

                match ty.kind() {
                    TyKind::Str if access.member.name() == sym::PTR => {
                        Ty::new(TyKind::RawPtr(self.db.types.u8))
                    }
                    TyKind::Str if access.member.name() == sym::LEN => self.db.types.uint,
                    _ => return Err(TypeckError::InvalidMember { ty, member: access.member }),
                }
            }
            ExprKind::Name(name) => {
                let def_ty = self.lookup(name.id);
                let ty = self.normalize(def_ty);

                let ty_params = ty.collect_params();

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
                        return Err(TypeckError::TyArgMismatch {
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
            ExprKind::Lit(value) => match value {
                Lit::Str(..) => self.db.types.str,
                Lit::Int(..) => self.fresh_int_var(),
                Lit::Bool(..) => self.db.types.bool,
                Lit::Unit => self.db.types.unit,
            },
        };

        self.db.const_storage.eval_expr(expr).map_err(|e| TypeckError::ConstEval(e, expr.span))?;

        Ok(())
    }

    fn typeck_ty(&mut self, ty: &hir::Ty) -> TypeckResult<Ty> {
        match ty {
            hir::Ty::RawPtr(pointee, _) => Ok(Ty::new(TyKind::RawPtr(self.typeck_ty(pointee)?))),
            hir::Ty::Name(name) => {
                let def = &self.db[name.id];

                match def.kind.as_ref() {
                    DefKind::Ty(ty) => Ok(*ty),
                    _ => Err(TypeckError::ExpectedTy { ty: def.ty, span: name.span }),
                }
            }
            hir::Ty::Unit(_) => Ok(self.db.types.unit),
            hir::Ty::Infer(_) => Ok(self.fresh_ty_var()),
        }
    }
}
