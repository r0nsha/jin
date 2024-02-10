use crate::{
    ast::{self, Ast},
    counter::Counter,
    db::{
        Adt, AdtField, AdtId, AdtKind, Db, DefId, DefKind, FnInfo, Intrinsic, ModuleId, StructDef,
        UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    hir,
    hir::{ExprId, FnParam, Hir},
    macros::create_bool_enum,
    middle::{BinOp, CallConv, CmpOp, IsUfcs, Mutability, Pat, TyExpr, TyParam, UnOp, Vis},
    span::{Span, Spanned},
    sym,
    ty::{
        FloatVar, FnTy, FnTyFlags, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty, TyKind,
        TyVar,
    },
    typeck2::{fns, items, ns::Env, tyexpr, tyexpr::AllowTyHole, Typeck},
    word::{Word, WordMap},
};

#[allow(clippy::too_many_lines)]
fn check_expr(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    expr: &ast::Expr,
    expected_ty: Option<Ty>,
) -> DiagnosticResult<hir::Expr> {
    match expr {
        ast::Expr::Let(let_) => {
            let span = let_.span;
            let ty = tyexpr::check_optional(cx, &env, let_.ty_expr.as_ref(), AllowTyHole::Yes)?;
            let pat = cx.define().local_pat(env, &let_.pat, ty);
            let let_ = items::check_let_body(cx, pat, let_)?;
            Ok(cx.expr(hir::ExprKind::Let(let_), cx.db.types.unit, span))
        }
        ast::Expr::Fn { params, ret, body, span } => {
            let sig = fns::check_expr_sig(cx, env, params, ret.as_ref(), *span)?;

            let word = sig.word;
            let ty = sig.ty;
            let id = self.define_def(
                env,
                Vis::Private,
                DefKind::Fn(FnInfo::Bare),
                word,
                Mutability::Imm,
                ty,
            )?;

            let mut fun = self.check_fn_expr_body(env, sig, id, body, *span)?;

            self.hir.fns.push_with_key(|id| {
                fun.id = id;
                fun
            });

            Ok(self.expr(
                hir::ExprKind::Name(hir::Name {
                    id,
                    word,
                    instantiation: Instantiation::default(),
                }),
                ty,
                *span,
            ))
        }
        ast::Expr::Assign { lhs, rhs, op, span } => {
            let lhs = self.check_expr(env, lhs, None)?;
            self.check_assign_lhs(&lhs)?;

            let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

            if let Some(op) = op {
                self.check_bin_op(&lhs, &rhs, *op, *span)?;
            } else {
                self.at(Obligation::exprs(*span, lhs.span, rhs.span))
                    .eq(lhs.ty, rhs.ty)
                    .or_coerce(self, rhs.id)?;
            }

            Ok(self.expr(
                hir::ExprKind::Assign(hir::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: *op,
                }),
                self.db.types.unit,
                *span,
            ))
        }
        ast::Expr::Swap { lhs, rhs, span } => {
            let lhs = self.check_expr(env, lhs, None)?;
            self.check_swap_lhs(&lhs)?;

            let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

            self.at(Obligation::exprs(*span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(self, rhs.id)?;

            let ty = lhs.ty;

            Ok(self.expr(
                hir::ExprKind::Swap(hir::Swap { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
                ty,
                *span,
            ))
        }
        ast::Expr::Return { expr, span } => {
            if let Some(fn_id) = env.fn_id() {
                let ret_ty = self.def_ty(fn_id).as_fn().unwrap().ret;

                let expr = if let Some(expr) = expr {
                    self.check_expr(env, expr, Some(ret_ty))?
                } else {
                    self.unit_expr(*span)
                };

                self.at(Obligation::return_ty(expr.span, self.db[fn_id].span))
                    .eq(ret_ty, expr.ty)
                    .or_coerce(self, expr.id)?;

                Ok(self.expr(
                    hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }),
                    self.db.types.never,
                    *span,
                ))
            } else {
                Err(Diagnostic::error("cannot return outside of function scope")
                    .with_label(Label::primary(*span, "invalid return")))
            }
        }
        ast::Expr::If { cond, then, otherwise, span } => self.check_if(
            env,
            cond.as_ref(),
            then.as_ref(),
            otherwise.as_deref(),
            *span,
            expected_ty,
        ),
        ast::Expr::Match { expr, arms, span } => {
            self.check_match(env, expr, arms, *span, expected_ty)
        }
        ast::Expr::Loop { cond, expr, span } => {
            let cond = if let Some(cond) = cond.as_ref() {
                let cond = self.check_expr(env, cond, Some(self.db.types.bool))?;
                self.eq_obvious_expr(self.db.types.bool, &cond)?;
                Some(Box::new(cond))
            } else {
                None
            };

            let expr = env.with_anon_scope(ScopeKind::Loop, |env| {
                self.check_expr(env, expr, Some(self.db.types.never))
            })?;

            // NOTE: expected & actual types are flipped here so that all types are accepted
            // (since all types coerce to `never`)
            self.at(Obligation::obvious(expr.span))
                .eq(expr.ty, self.db.types.never)
                .or_coerce(self, expr.id)?;

            Ok(self.expr(
                hir::ExprKind::Loop(hir::Loop { cond, expr: Box::new(expr) }),
                self.db.types.never,
                *span,
            ))
        }
        ast::Expr::Break { span } => {
            if env.in_scope_kind(&ScopeKind::Loop) {
                Ok(self.expr(hir::ExprKind::Break, self.db.types.never, *span))
            } else {
                Err(Diagnostic::error("cannot break outside of a loop")
                    .with_label(Label::primary(*span, "break outside of loop")))
            }
        }
        ast::Expr::Block { exprs, span } => {
            env.with_anon_scope(ScopeKind::Block, |env| -> TypeckResult<hir::Expr> {
                let (exprs, ty) = if exprs.is_empty() {
                    (vec![], self.db.types.unit)
                } else {
                    let mut new_exprs = vec![];

                    for (pos, expr) in exprs.iter().with_position() {
                        let expected_ty = match pos {
                            Position::Last => expected_ty,
                            _ => Some(self.db.types.unit),
                        };

                        new_exprs.push(self.check_expr(env, expr, expected_ty)?);
                    }

                    let ty = new_exprs.last().unwrap().ty;

                    (new_exprs, ty)
                };

                Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs }), ty, *span))
            })
        }
        ast::Expr::Unsafe { expr, span } => {
            let expr = self.check_expr(env, expr, expected_ty)?;
            let ty = expr.ty;
            Ok(self.expr(hir::ExprKind::Unsafe(hir::Unsafe { expr: Box::new(expr) }), ty, *span))
        }
        ast::Expr::MethodCall { expr, method, targs, args, span } => {
            let targs = self.check_optional_ty_args(env, targs.as_deref(), AllowTyHole::Yes)?;
            let mut args = self.check_call_args(env, args)?;

            // Try looking up an associated function call first
            if let ast::Expr::Name { word, targs: name_targs, .. } = expr.as_ref() {
                let id = self.lookup(env, env.module_id(), &Query::Name(*word))?;
                let name_targs =
                    self.check_optional_ty_args(env, name_targs.as_deref(), AllowTyHole::Yes)?;

                if let Some(assoc_ty) = self.try_extract_assoc_ty(id) {
                    let query_args = self.map_call_args_for_query(&args);
                    let query = FnQuery::new(*method, targs.as_deref(), &query_args, IsUfcs::No);

                    let (ty, _) = self.apply_ty_args_to_ty(
                        env,
                        assoc_ty.ty(self.db),
                        name_targs.as_deref(),
                        *span,
                    )?;

                    let (callee, _) = self.check_query_in_ty(
                        env,
                        ty,
                        word.span(),
                        &Query::Fn(query),
                        targs.as_deref(),
                        *span,
                    )?;

                    return self.check_call(callee, args, *span, IsUfcs::No);
                }
            }

            let expr = self.check_expr(env, expr, expected_ty)?;

            let mut is_ufcs = IsUfcs::No;
            let lookup_in_module = match self.normalize(expr.ty).kind() {
                TyKind::Module(in_module) => *in_module,
                TyKind::Type(ty) => {
                    // This is probably a union variant
                    let (callee, _) = self.check_query_in_ty(
                        env,
                        *ty,
                        *span,
                        &Query::Name(*method),
                        targs.as_deref(),
                        expr.span,
                    )?;
                    return self.check_call(callee, args, *span, IsUfcs::No);
                }
                _ => {
                    // This is a UFCS call: add `expr` as the first argument of the call
                    is_ufcs = IsUfcs::Yes;
                    args.insert(0, hir::CallArg { name: None, expr, index: None });
                    env.module_id()
                }
            };

            let id = self.lookup_fn_for_call(
                env,
                lookup_in_module,
                *method,
                targs.as_deref(),
                &args,
                is_ufcs,
            )?;

            let callee = self.check_name(env, id, *method, *span, targs.as_deref())?;

            self.check_call(callee, args, *span, is_ufcs)
        }
        ast::Expr::Call { callee, args, span } => {
            let args = self.check_call_args(env, args)?;

            let callee = match callee.as_ref() {
                ast::Expr::Name { word, targs, span } => {
                    let targs =
                        self.check_optional_ty_args(env, targs.as_deref(), AllowTyHole::Yes)?;

                    let id = self.lookup_fn_for_call(
                        env,
                        env.module_id(),
                        *word,
                        targs.as_deref(),
                        &args,
                        IsUfcs::No,
                    )?;

                    self.check_name(env, id, *word, *span, targs.as_deref())?
                }
                _ => self.check_expr(env, callee, None)?,
            };

            self.check_call(callee, args, *span, IsUfcs::No)
        }
        ast::Expr::Unary { expr, op, span } => {
            let expr = self.check_expr(env, expr, None)?;

            let ty = self.normalize(expr.ty);

            match op {
                UnOp::Neg => {
                    if ty.is_any_int() || ty.is_any_float() {
                        Ok(self.expr(
                            hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                            ty,
                            *span,
                        ))
                    } else {
                        Err(errors::invalid_un_op(self.db, *op, ty, *span))
                    }
                }
                UnOp::Not => {
                    if ty.is_any_int() || ty.is_bool() {
                        Ok(self.expr(
                            hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                            ty,
                            *span,
                        ))
                    } else {
                        Err(errors::invalid_un_op(self.db, *op, ty, *span))
                    }
                }
                UnOp::Ref(mutability) => self.check_ref(expr, ty, *op, *mutability, *span),
            }
        }
        ast::Expr::Binary { lhs, rhs, op, span } => {
            let lhs = self.check_expr(env, lhs, None)?;
            let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

            self.check_bin_op(&lhs, &rhs, *op, *span)?;

            let result_ty = match op {
                BinOp::Cmp(..) => self.db.types.bool,
                _ => lhs.ty,
            };

            Ok(self.expr(
                hir::ExprKind::Binary(hir::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: *op,
                }),
                result_ty,
                *span,
            ))
        }
        ast::Expr::Deref { expr, span } => {
            let expected_ty = self.fresh_ty_var().raw_ptr();
            let expr = self.check_expr(env, expr, Some(expected_ty))?;

            match self.normalize(expr.ty).auto_deref().kind() {
                TyKind::RawPtr(pointee) => Ok(self.expr(
                    hir::ExprKind::Deref(hir::Deref { expr: Box::new(expr) }),
                    *pointee,
                    *span,
                )),
                ty => Err(errors::ty_mismatch(
                    &expected_ty.to_string(self.db),
                    &ty.to_string(self.db),
                    expr.span,
                )),
            }
        }
        ast::Expr::Cast { expr, target, span } => {
            let expr = self.check_expr(env, expr, None)?;
            let target = self.check_ty_expr(env, target, AllowTyHole::Yes)?;

            Ok(self.expr(
                hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }),
                target,
                *span,
            ))
        }
        ast::Expr::Transmute { expr, target, span } => {
            let expr = self.check_expr(env, expr, None)?;
            let target = self.check_ty_expr(env, target, AllowTyHole::Yes)?;

            Ok(self.expr(
                hir::ExprKind::Transmute(hir::Transmute { expr: Box::new(expr), target }),
                target,
                *span,
            ))
        }
        ast::Expr::Field { expr, field, span } => {
            let expr = self.check_expr(env, expr, expected_ty)?;
            self.check_field(env, expr, *field, *span)
        }
        ast::Expr::Index { expr, index, span } => {
            let expected_slice_ty = Ty::new(TyKind::Slice(self.fresh_ty_var()));

            let expr = self.check_expr(env, expr, Some(expected_slice_ty))?;

            let expr_ty = self.normalize(expr.ty);

            let elem_ty = match expr_ty.auto_deref().kind() {
                TyKind::Slice(inner) => *inner,
                _ => {
                    return Err(errors::ty_mismatch(
                        &expected_slice_ty.to_string(self.db),
                        &expr_ty.to_string(self.db),
                        expr.span,
                    ))
                }
            };

            let uint = self.db.types.uint;
            let index = self.check_expr(env, index, Some(uint))?;
            self.eq_obvious_expr(uint, &index)?;

            Ok(self.expr(
                hir::ExprKind::Index(hir::Index { expr: Box::new(expr), index: Box::new(index) }),
                elem_ty,
                *span,
            ))
        }
        ast::Expr::Slice { expr, low, high, span } => {
            let expected_slice_ty = Ty::new(TyKind::Slice(self.fresh_ty_var()));

            let expr = self.check_expr(env, expr, Some(expected_slice_ty))?;
            let expr_ty = self.normalize(expr.ty).auto_deref();
            self.at(Obligation::obvious(expr.span))
                .eq(expected_slice_ty, expr_ty)
                .or_coerce(self, expr.id)?;

            let uint = self.db.types.uint;
            let low = if let Some(low) = low {
                let low = self.check_expr(env, low, Some(uint))?;
                self.eq_obvious_expr(uint, &low)?;
                Some(Box::new(low))
            } else {
                None
            };

            let high = if let Some(high) = high {
                let high = self.check_expr(env, high, Some(uint))?;
                self.eq_obvious_expr(uint, &high)?;
                Some(Box::new(high))
            } else {
                None
            };

            Ok(self.expr(
                hir::ExprKind::Slice(hir::Slice { expr: Box::new(expr), low, high }),
                expr_ty,
                *span,
            ))
        }
        ast::Expr::Name { word, targs, span } => {
            let id = self.lookup(env, env.module_id(), &Query::Name(*word))?;
            let targs = self.check_optional_ty_args(env, targs.as_deref(), AllowTyHole::Yes)?;
            self.check_name(env, id, *word, *span, targs.as_deref())
        }
        ast::Expr::SliceLit { exprs, span } => {
            let elem_ty = expected_ty
                .and_then(|t| self.normalize(t).slice_elem())
                .unwrap_or_else(|| self.fresh_ty_var());

            let mut new_exprs = vec![];

            for expr in exprs {
                let new_expr = self.check_expr(env, expr, Some(elem_ty))?;
                self.eq_obvious_expr(elem_ty, &new_expr)?;
                new_exprs.push(new_expr);
            }

            Ok(self.expr(
                hir::ExprKind::SliceLit(hir::SliceLit { exprs: new_exprs, cap: None }),
                Ty::new(TyKind::Slice(elem_ty)),
                *span,
            ))
        }
        ast::Expr::SliceLitCap { cap, span } => {
            let uint = self.db.types.uint;
            let cap = self.check_expr(env, cap, Some(uint))?;
            self.eq_obvious_expr(uint, &cap)?;

            Ok(self.expr(
                hir::ExprKind::SliceLit(hir::SliceLit { exprs: vec![], cap: Some(Box::new(cap)) }),
                Ty::new(TyKind::Slice(self.fresh_ty_var())),
                *span,
            ))
        }
        ast::Expr::BoolLit { value, span } => {
            Ok(self.expr(hir::ExprKind::BoolLit(*value), self.db.types.bool, *span))
        }
        ast::Expr::IntLit { value, span } => {
            Ok(self.expr(hir::ExprKind::IntLit(*value), self.fresh_int_var(), *span))
        }
        ast::Expr::FloatLit { value, span } => {
            Ok(self.expr(hir::ExprKind::FloatLit(*value), self.fresh_float_var(), *span))
        }
        ast::Expr::StrLit { value, span } => {
            Ok(self.expr(hir::ExprKind::StrLit(*value), self.db.types.str, *span))
        }
    }
}

// fn check_if(
//     &mut self,
//     env: &mut Env,
//     cond: &ast::Expr,
//     then: &ast::Expr,
//     otherwise: Option<&ast::Expr>,
//     span: Span,
//     expected_ty: Option<Ty>,
// ) -> TypeckResult<hir::Expr> {
//     let cond = self.check_expr(env, cond, Some(self.db.types.bool))?;
//     self.eq_obvious_expr(self.db.types.bool, &cond)?;
//
//     let then = self.check_expr(env, then, expected_ty)?;
//
//     let otherwise = if let Some(otherwise) = otherwise.as_ref() {
//         let otherwise = self.check_expr(env, otherwise, Some(then.ty))?;
//
//         self.at(Obligation::exprs(span, then.span, otherwise.span))
//             .eq(then.ty, otherwise.ty)
//             .or_coerce(self, otherwise.id)?;
//
//         otherwise
//     } else {
//         self.eq_obvious_expr(self.db.types.unit, &then)?;
//         self.unit_expr(span)
//     };
//
//     let ty = otherwise.ty;
//     let cond_span = cond.span;
//
//     Ok(self.expr(
//         hir::ExprKind::Match(hir::Match {
//             expr: Box::new(cond),
//             arms: vec![
//                 hir::MatchArm {
//                     pat: hir::MatchPat::Bool(true, cond_span),
//                     guard: None,
//                     expr: Box::new(then),
//                 },
//                 hir::MatchArm {
//                     pat: hir::MatchPat::Bool(false, cond_span),
//                     guard: None,
//                     expr: Box::new(otherwise),
//                 },
//             ],
//         }),
//         ty,
//         span,
//     ))
// }

// fn check_name(
//     &mut self,
//     env: &Env,
//     id: DefId,
//     word: Word,
//     span: Span,
//     targs: Option<&[Ty]>,
// ) -> TypeckResult<hir::Expr> {
//     if let DefKind::Adt(adt_id) = self.db[id].kind.as_ref() {
//         match &self.db[*adt_id].kind {
//             AdtKind::Struct(_) => {
//                 return self.check_name_struct(env, id, word, span, targs,
// *adt_id);             }
//             AdtKind::Union(_) => (),
//         }
//     }
//
//     let def_ty = self.normalize(self.def_ty(id));
//     let (ty, instantiation) = self.apply_ty_args_to_ty(env, def_ty, targs,
// span)?;
//
//     Ok(self.expr(hir::ExprKind::Name(hir::Name { id, word, instantiation }),
// ty, span)) }

// fn check_name_struct(
//     &mut self,
//     env: &Env,
//     id: DefId,
//     word: Word,
//     span: Span,
//     targs: Option<&[Ty]>,
//     adt_id: AdtId,
// ) -> TypeckResult<hir::Expr> {
//     let adt = &self.db[adt_id];
//     let struct_def = adt.as_struct().unwrap();
//
//     // NOTE: if the named definition is a struct, we want to return its
//     // constructor function's type
//     if struct_def.ctor_vis == Vis::Private &&
// self.db[adt.def_id].scope.module_id != env.module_id()     {
//         let private_field = struct_def
//             .fields
//             .iter()
//             .find(|f| f.vis == Vis::Private)
//             .expect("to have at least one private field");
//
//         return Err(Diagnostic::error(format!(
//             "constructor of type `{}` is private because `{}` is private",
//             adt.name, private_field.name
//         ))
//         .with_label(Label::primary(span, "private type constructor"))
//         .with_label(Label::secondary(
//             private_field.name.span(),
//             format!("`{}` is private", private_field.name),
//         )));
//     }
//
//     let (ty, instantiation) = self.apply_ty_args_to_ty(env,
// struct_def.ctor_ty, targs, span)?;
//
//     Ok(self.expr(hir::ExprKind::Name(hir::Name { id, word, instantiation }),
// ty, span)) }

// Applies type arguments to the given type
// fn apply_ty_args_to_ty(
//     &mut self,
//     env: &Env,
//     ty: Ty,
//     targs: Option<&[Ty]>,
//     span: Span,
// ) -> TypeckResult<(Ty, Instantiation)> {
//     let mut ty_params = ty.collect_params();
//
//     // NOTE: map type params that are part of the current polymorphic
// function to     // themselves, so that we don't instantiate them. that's
// quite ugly     // though.
//     if let Some(fn_id) = env.fn_id() {
//         let fn_ty_params = self.def_ty(fn_id).collect_params();
//         for ftp in fn_ty_params {
//             if let Some(tp) = ty_params.iter_mut().find(|p| p.var == ftp.var)
// {                 *tp = ftp.clone();
//             }
//         }
//     }
//
//     let instantiation: Instantiation = match &targs {
//         Some(args) if args.len() == ty_params.len() => {
//             ty_params.into_iter().zip(args.iter()).map(|(param, arg)|
// (param.var, *arg)).collect()         }
//         Some(args) => {
//             return Err(errors::ty_arg_mismatch(ty_params.len(), args.len(),
// span));         }
//         _ => self.fresh_instantiation(env, ty_params),
//     };
//
//     Ok((instantiation.fold(ty), instantiation))
// }

// fn fresh_instantiation(&mut self, env: &Env, ty_params: Vec<ParamTy>) ->
// Instantiation {     let env_fn_ty_params = env.fn_id().map_or(vec![], |id|
// self.def_ty(id).collect_params());
//
//     ty_params
//         .into_iter()
//         .map(|param| {
//             (
//                 param.var,
//                 // If the type param is one of the current function's type
//                 // params, we don't want to instantiate it
//                 if env_fn_ty_params.iter().any(|p| p.var == param.var) {
//                     Ty::new(TyKind::Param(param))
//                 } else {
//                     self.fresh_ty_var()
//                 },
//             )
//         })
//         .collect()
// }

// fn check_field(
//     &mut self,
//     env: &Env,
//     expr: hir::Expr,
//     field: Word,
//     span: Span,
// ) -> TypeckResult<hir::Expr> {
//     let ty = self.normalize(expr.ty).auto_deref();
//
//     let res_ty = match ty.kind() {
//         TyKind::Module(module_id) => {
//             let id = self.lookup(env, *module_id, &Query::Name(field))?;
//             return self.check_name(env, id, field, span, None);
//         }
//         TyKind::Adt(adt_id, targs) => {
//             let adt = &self.db[*adt_id];
//
//             match &adt.kind {
//                 AdtKind::Struct(struct_def) => {
//                     if let Some(field) =
// struct_def.field_by_name(field.name().as_str()) {
// self.check_field_access(env, adt, field, span)?;
// Some(adt.instantiation(targs).fold(field.ty))                     } else {
//                         None
//                     }
//                 }
//                 AdtKind::Union(_) => None,
//             }
//         }
//         TyKind::Type(ty) => {
//             // This is a union variant
//             let (expr, can_implicitly_call) =
//                 self.check_query_in_ty(env, *ty, span, &Query::Name(field),
// None, expr.span)?;
//
//             return if can_implicitly_call {
//                 self.check_call(expr, vec![], span, IsUfcs::No)
//             } else {
//                 Ok(expr)
//             };
//         }
//         TyKind::Slice(..) if field.name() == sym::CAP =>
// Some(self.db.types.uint),         TyKind::Slice(..) | TyKind::Str if
// field.name() == sym::LEN => Some(self.db.types.uint),
//         TyKind::Slice(elem_ty) if field.name() == sym::PTR =>
// Some(elem_ty.raw_ptr()),         TyKind::Str if field.name() == sym::PTR =>
// Some(self.db.types.u8.raw_ptr()),         _ => None,
//     };
//
//     if let Some(res_ty) = res_ty {
//         Ok(self.expr(
//             hir::ExprKind::Field(hir::Field { expr: Box::new(expr), field }),
//             res_ty,
//             span,
//         ))
//     } else {
//         Err(errors::field_not_found(self.db, ty, expr.span, field))
//     }
// }

// fn check_field_access(
//     &self,
//     env: &Env,
//     adt: &Adt,
//     field: &AdtField,
//     span: Span,
// ) -> TypeckResult<()> {
//     if field.vis == Vis::Private && self.db[adt.def_id].scope.module_id !=
// env.module_id() {         return Err(Diagnostic::error(format!(
//             "field `{}` of type `{}` is private",
//             field.name, adt.name
//         ))
//         .with_label(Label::primary(span, "private field")));
//     }
//
//     Ok(())
// }

// /// Tries to look up `name` in the namespace of `ty`.
// /// Returns the evaluated expression, and whether it can be implicitly
// /// called.
// fn check_query_in_ty(
//     &mut self,
//     env: &Env,
//     ty: Ty,
//     ty_span: Span,
//     query: &Query,
//     targs: Option<&[Ty]>,
//     span: Span,
// ) -> TypeckResult<(hir::Expr, bool)> {
//     match self.lookup_in_ty(env.module_id(), ty, ty_span, query)? {
//         TyLookup::Variant(variant_id) => {
//             let TyKind::Adt(adt_id, targs) = ty.kind() else { unreachable!()
// };
//
//             let variant = &self.db[variant_id];
//             let adt = &self.db[*adt_id];
//
//             let instantiation = adt.instantiation(targs);
//             let ctor_ty = instantiation.fold(variant.ctor_ty);
//
//             // Union variants without fields are implicitly called for
// convenience             let can_implicitly_call = variant.fields.is_empty();
//             let expr = self.expr(
//                 hir::ExprKind::Variant(hir::Variant { id: variant.id,
// instantiation }),                 ctor_ty,
//                 span,
//             );
//
//             Ok((expr, can_implicitly_call))
//         }
//         TyLookup::AssocFn(id) => {
//             let expr = self.check_name(env, id, query.word(), span, targs)?;
//             Ok((expr, false))
//         }
//     }
// }

// fn map_call_args_for_query(&mut self, args: &[hir::CallArg]) ->
// Vec<FnTyParam> {     args.iter()
//         .map(|a| FnTyParam { name: a.name.map(|w| w.name()), ty:
// self.normalize(a.expr.ty) })         .collect::<Vec<_>>()
// }

// fn lookup_fn_for_call(
//     &mut self,
//     env: &Env,
//     in_module: ModuleId,
//     word: Word,
//     ty_args: Option<&[Ty]>,
//     args: &[hir::CallArg],
//     is_ufcs: IsUfcs,
// ) -> TypeckResult<DefId> {
//     let args = self.map_call_args_for_query(args);
//     let query = FnQuery::new(word, ty_args, &args, is_ufcs);
//     self.lookup(env, in_module, &Query::Fn(query))
// }

// fn check_call(
//     &mut self,
//     callee: hir::Expr,
//     args: Vec<hir::CallArg>,
//     span: Span,
//     is_ufcs: IsUfcs,
// ) -> TypeckResult<hir::Expr> {
//     let callee_ty = self.normalize(callee.ty);
//
//     match callee_ty.kind() {
//         TyKind::Fn(fn_ty) => self.check_call_fn(callee, args, fn_ty, span,
// is_ufcs),         TyKind::Type(ty) => {
//             if args.len() != 1 {
//                 return Err(errors::arg_mismatch(1, args.len(), span));
//             }
//
//             let arg = &args[0];
//
//             if let Some(name) = arg.name {
//                 return Err(errors::named_param_not_found(name));
//             }
//
//             Ok(self.expr(
//                 hir::ExprKind::Cast(hir::Cast { expr:
// Box::new(arg.expr.clone()), target: *ty }),                 *ty,
//                 span,
//             ))
//         }
//         _ => {
//             let ty = self.normalize(callee.ty);
//             let span = callee.span;
//
//             Err(Diagnostic::error(format!("expected a function, found `{}`",
// ty.display(self.db)))                 .with_label(Label::primary(span,
// "expected a function")))         }
//     }
// }

// fn check_call_fn(
//     &mut self,
//     callee: hir::Expr,
//     mut args: Vec<hir::CallArg>,
//     fn_ty: &FnTy,
//     span: Span,
//     is_ufcs: IsUfcs,
// ) -> TypeckResult<hir::Expr> {
//     #[derive(Debug)]
//     struct PassedArg {
//         is_named: bool,
//         span: Span,
//     }
//
//     if !fn_ty.is_c_variadic() && args.len() != fn_ty.params.len() {
//         return Err(errors::arg_mismatch(fn_ty.params.len(), args.len(),
// span));     }
//
//     let mut already_passed_args = UstrMap::<PassedArg>::default();
//
//     // Resolve positional arg indices
//     for (idx, arg) in args.iter_mut().enumerate() {
//         if arg.name.is_none() {
//             arg.index = Some(idx);
//
//             if let Some(param_name) = fn_ty.params.get(idx).and_then(|p|
// p.name) {                 already_passed_args
//                     .insert(param_name, PassedArg { is_named: false, span:
// arg.expr.span });             }
//         }
//
//         let arg_ty = self.normalize(arg.expr.ty);
//
//         if arg_ty.is_type() {
//             return Err(errors::generic_expected_found(
//                 "a value",
//                 &format!("type `{}`", arg_ty.display(self.db)),
//                 arg.expr.span,
//             ));
//         }
//     }
//
//     // Resolve named arg indices
//     for arg in &mut args {
//         if let Some(arg_name) = &arg.name {
//             let name = arg_name.name();
//
//             let idx = fn_ty
//                 .params
//                 .iter()
//                 .enumerate()
//                 .find_map(|(i, p)| if p.name == Some(name) { Some(i) } else {
// None })                 .ok_or_else(||
// errors::named_param_not_found(*arg_name))?;
//
//             // Report named arguments that are passed twice
//             if let Some(passed_arg) = already_passed_args
//                 .insert(arg_name.name(), PassedArg { is_named: true, span:
// arg_name.span() })             {
//                 let name = arg_name.name();
//                 let prev = passed_arg.span;
//                 let dup = arg_name.span();
//                 let is_named = passed_arg.is_named;
//
//                 return Err(Diagnostic::error(if is_named {
//                     format!("argument `{name}` is passed multiple times")
//                 } else {
//                     format!("argument `{name}` is already passed
// positionally")                 })
//                 .with_label(Label::primary(dup, format!("`{name}` is passed
// again here")))                 .with_label(Label::secondary(prev,
// format!("`{name}` is already passed here"))));             }
//
//             arg.index = Some(idx);
//         }
//     }
//
//     // Unify all args with their corresponding param type
//     for (arg_idx, arg) in args.iter().enumerate() {
//         let param_idx = arg.index.expect("arg index to be resolved");
//
//         if let Some(param) = fn_ty.params.get(param_idx) {
//             let coerce_options = CoerceOptions {
//                 allow_owned_to_ref: is_ufcs == IsUfcs::Yes && arg_idx == 0,
//                 ..CoerceOptions::default()
//             };
//             self.at(Obligation::obvious(arg.expr.span)).eq(param.ty,
// arg.expr.ty).or_coerce_ex(                 self,
//                 arg.expr.id,
//                 coerce_options,
//             )?;
//         }
//     }
//
//     Ok(self.expr(
//         hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args }),
//         fn_ty.ret,
//         span,
//     ))
// }

// fn check_ref(
//     &mut self,
//     expr: hir::Expr,
//     ty: Ty,
//     op: UnOp,
//     mutability: Mutability,
//     span: Span,
// ) -> TypeckResult<hir::Expr> {
//     // if ty.can_create_ref(self.db) {
//     Ok(self.expr(
//         hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op }),
//         ty.create_ref(mutability),
//         span,
//     ))
//     // } else {
//     //     Err(Diagnostic::error()
//     //         .with_message(format!(
//     //             "cannot take a reference to value of type `{}`",
//     //             ty.display(self.db)
//     //         ))
//     //         .with_label(Label::primary(span, "cannot take
//     // reference"))) }
// }

// fn check_call_args(
//     &mut self,
//     env: &mut Env,
//     args: &[ast::CallArg],
// ) -> TypeckResult<Vec<hir::CallArg>> {
//     let mut new_args = vec![];
//
//     for arg in args {
//         new_args.push(match arg {
//             ast::CallArg::Named(name, expr) => hir::CallArg {
//                 name: Some(*name),
//                 expr: self.check_expr(env, expr, None)?,
//                 index: None,
//             },
//             ast::CallArg::Positional(expr) => {
//                 hir::CallArg { name: None, expr: self.check_expr(env, expr,
// None)?, index: None }             }
//         });
//     }
//
//     Ok(new_args)
// }

// fn check_bin_op(
//     &mut self,
//     lhs: &hir::Expr,
//     rhs: &hir::Expr,
//     op: BinOp,
//     span: Span,
// ) -> TypeckResult<()> {
//     fn can_use_eq(ty: Ty) -> bool {
//         match ty.kind() {
//             TyKind::Ref(ty, _) => can_use_eq(*ty),
//             TyKind::RawPtr(_)
//             | TyKind::Int(_)
//             | TyKind::Uint(_)
//             | TyKind::Float(_)
//             | TyKind::Str
//             | TyKind::Bool
//             | TyKind::Infer(InferTy::Int(_) | InferTy::Float(_)) => true,
//             _ => false,
//         }
//     }
//
//     match op {
//         BinOp::And | BinOp::Or => {
//             self.eq_obvious_expr(self.db.types.bool, lhs)?;
//             self.eq_obvious_expr(self.db.types.bool, rhs)?;
//         }
//         BinOp::Cmp(CmpOp::Eq | CmpOp::Ne) => {
//             self.at(Obligation::exprs(span, lhs.span, rhs.span))
//                 .eq(lhs.ty, rhs.ty)
//                 .or_coerce(self, rhs.id)?;
//
//             let ty = self.normalize(lhs.ty);
//             if !can_use_eq(ty) {
//                 return Err(errors::invalid_bin_op(self.db, op, ty, span));
//             }
//         }
//         BinOp::Add => {
//             if self.normalize(lhs.ty).is_raw_ptr() {
//                 self.eq_obvious_expr(self.db.types.uint, rhs)?;
//                 return Ok(());
//             }
//
//             self.at(Obligation::exprs(span, lhs.span, rhs.span))
//                 .eq(lhs.ty, rhs.ty)
//                 .or_coerce(self, rhs.id)?;
//
//             let ty = self.normalize(lhs.ty);
//             if !ty.is_any_int() && !ty.is_any_float() {
//                 return Err(errors::invalid_bin_op(self.db, op, ty, span));
//             }
//         }
//         BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Cmp(_) => {
//             self.at(Obligation::exprs(span, lhs.span, rhs.span))
//                 .eq(lhs.ty, rhs.ty)
//                 .or_coerce(self, rhs.id)?;
//
//             let ty = self.normalize(lhs.ty);
//             if !ty.is_any_int() && !ty.is_any_float() {
//                 return Err(errors::invalid_bin_op(self.db, op, ty, span));
//             }
//         }
//         BinOp::Rem | BinOp::Shl | BinOp::Shr | BinOp::BitAnd | BinOp::BitOr |
// BinOp::BitXor => {             self.at(Obligation::exprs(span, lhs.span,
// rhs.span))                 .eq(lhs.ty, rhs.ty)
//                 .or_coerce(self, rhs.id)?;
//
//             let ty = self.normalize(lhs.ty);
//             if !ty.is_any_int() {
//                 return Err(errors::invalid_bin_op(self.db, op, ty, span));
//             }
//         }
//     }
//
//     Ok(())
// }

// fn check_assign_lhs(&self, expr: &hir::Expr) -> TypeckResult<()> {
//     Self::check_assign_lhs_aux(expr).then_some(()).ok_or_else(|| {
//         Diagnostic::error("invalid left-hand side of assignment")
//             .with_label(Label::primary(expr.span, "expression is not
// assignable"))     })
// }

// fn check_swap_lhs(&self, expr: &hir::Expr) -> TypeckResult<()> {
//     Self::check_assign_lhs_aux(expr).then_some(()).ok_or_else(|| {
//         Diagnostic::error("invalid left-hand side of swap")
//             .with_label(Label::primary(expr.span, "expression is not
// swappable"))     })
// }

// fn check_assign_lhs_aux(expr: &hir::Expr) -> bool {
//     match &expr.kind {
//         hir::ExprKind::Unsafe(uns) => Self::check_assign_lhs_aux(&uns.expr),
//         hir::ExprKind::Deref(_)
//         | hir::ExprKind::Field(_)
//         | hir::ExprKind::Index(_)
//         | hir::ExprKind::Name(_) => true,
//         hir::ExprKind::Slice(_)
//         | hir::ExprKind::Match(_)
//         | hir::ExprKind::Loop(_)
//         | hir::ExprKind::Break
//         | hir::ExprKind::Block(_)
//         | hir::ExprKind::Return(_)
//         | hir::ExprKind::Call(_)
//         | hir::ExprKind::Unary(_)
//         | hir::ExprKind::Binary(_)
//         | hir::ExprKind::Cast(_)
//         | hir::ExprKind::Transmute(_)
//         | hir::ExprKind::Let(_)
//         | hir::ExprKind::Assign(_)
//         | hir::ExprKind::Swap(_)
//         | hir::ExprKind::Variant(_)
//         | hir::ExprKind::SliceLit(_)
//         | hir::ExprKind::BoolLit(_)
//         | hir::ExprKind::IntLit(_)
//         | hir::ExprKind::FloatLit(_)
//         | hir::ExprKind::StrLit(_) => false,
//     }
// }
