use itertools::Itertools as _;
use ustr::UstrMap;

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
    typeck2::{
        coerce::{CoerceExt as _, CoerceOptions},
        errors, fns, items,
        lookup::{AssocLookup, FnQuery, Query},
        ns::{Env, ScopeKind},
        pmatch, tyexpr,
        tyexpr::AllowTyHole,
        types,
        unify::Obligation,
        Typeck,
    },
    word::{Word, WordMap},
};

#[allow(clippy::too_many_lines)]
pub(super) fn check_expr(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    expr: &ast::Expr,
    expected_ty: Option<Ty>,
) -> DiagnosticResult<hir::Expr> {
    match expr {
        ast::Expr::Let(let_) => {
            let span = let_.span;
            let ty = tyexpr::check_optional(cx, env, let_.ty_expr.as_ref(), AllowTyHole::Yes)?;
            let pat = cx.define().local_pat(env, &let_.pat, ty);
            let let_ = items::check_let_body(cx, env, pat, ty, let_)?;
            Ok(cx.expr(hir::ExprKind::Let(let_), cx.db.types.unit, span))
        }
        ast::Expr::Fn { params, ret, body, span } => {
            let sig = fns::check_expr_sig(cx, env, params, ret.as_ref(), *span)?;

            let word = sig.word;
            let ty = sig.ty;
            let id =
                cx.define().new_local(env, DefKind::Fn(FnInfo::Bare), word, Mutability::Imm, ty);

            todo!()

            // let mut fun = self.check_fn_expr_body(env, sig, id, body,
            // *span)?;
            //
            // self.hir.fns.push_with_key(|id| {
            //     fun.id = id;
            //     fun
            // });
            //
            // Ok(cx.expr(
            //     hir::ExprKind::Name(hir::Name {
            //         id,
            //         word,
            //         instantiation: Instantiation::default(),
            //     }),
            //     ty,
            //     *span,
            // ))
        }
        ast::Expr::Assign { lhs, rhs, op, span } => {
            let lhs = check_expr(cx, env, lhs, None)?;
            check_assign_lhs(&lhs)?;

            let rhs = check_expr(cx, env, rhs, Some(lhs.ty))?;

            if let Some(op) = op {
                check_bin_op(cx, &lhs, &rhs, *op, *span)?;
            } else {
                cx.at(Obligation::exprs(*span, lhs.span, rhs.span))
                    .eq(lhs.ty, rhs.ty)
                    .or_coerce(cx, rhs.id)?;
            }

            Ok(cx.expr(
                hir::ExprKind::Assign(hir::Assign {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op: *op,
                }),
                cx.db.types.unit,
                *span,
            ))
        }
        ast::Expr::Swap { lhs, rhs, span } => {
            let lhs = check_expr(cx, env, lhs, None)?;
            check_swap_lhs(&lhs)?;

            let rhs = check_expr(cx, env, rhs, Some(lhs.ty))?;

            cx.at(Obligation::exprs(*span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(cx, rhs.id)?;

            let ty = lhs.ty;

            Ok(cx.expr(
                hir::ExprKind::Swap(hir::Swap { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
                ty,
                *span,
            ))
        }
        ast::Expr::Return { expr, span } => {
            if let Some(fn_id) = env.fn_id() {
                let ret_ty = cx.def_ty(fn_id).as_fn().unwrap().ret;

                let expr = if let Some(expr) = expr {
                    check_expr(cx, env, expr, Some(ret_ty))?
                } else {
                    cx.unit_expr(*span)
                };

                cx.at(Obligation::return_ty(expr.span, cx.db[fn_id].span))
                    .eq(ret_ty, expr.ty)
                    .or_coerce(cx, expr.id)?;

                Ok(cx.expr(
                    hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }),
                    cx.db.types.never,
                    *span,
                ))
            } else {
                Err(Diagnostic::error("cannot return outside of function scope")
                    .with_label(Label::primary(*span, "invalid return")))
            }
        }
        ast::Expr::If { cond, then, otherwise, span } => check_if(
            cx,
            env,
            cond.as_ref(),
            then.as_ref(),
            otherwise.as_deref(),
            *span,
            expected_ty,
        ),
        ast::Expr::Match { expr, arms, span } => {
            pmatch::check(cx, env, expr, arms, *span, expected_ty)
        }
        ast::Expr::Loop { cond, expr, span } => {
            let cond = if let Some(cond) = cond.as_ref() {
                let cond = check_expr(cx, env, cond, Some(cx.db.types.bool))?;
                cx.eq_obvious_expr(cx.db.types.bool, &cond)?;
                Some(Box::new(cond))
            } else {
                None
            };

            let expr = env.with_anon_scope(ScopeKind::Loop, |env| {
                check_expr(cx, env, expr, Some(cx.db.types.never))
            })?;

            // NOTE: expected & actual types are flipped here so that all types are accepted
            // (since all types coerce to `never`)
            cx.at(Obligation::obvious(expr.span))
                .eq(expr.ty, cx.db.types.never)
                .or_coerce(cx, expr.id)?;

            Ok(cx.expr(
                hir::ExprKind::Loop(hir::Loop { cond, expr: Box::new(expr) }),
                cx.db.types.never,
                *span,
            ))
        }
        ast::Expr::Break { span } => {
            if env.in_scope_kind(&ScopeKind::Loop) {
                Ok(cx.expr(hir::ExprKind::Break, cx.db.types.never, *span))
            } else {
                Err(Diagnostic::error("cannot break outside of a loop")
                    .with_label(Label::primary(*span, "break outside of loop")))
            }
        }
        ast::Expr::Block { exprs, span } => {
            env.with_anon_scope(ScopeKind::Block, |env| -> DiagnosticResult<hir::Expr> {
                let (exprs, ty) = if exprs.is_empty() {
                    (vec![], cx.db.types.unit)
                } else {
                    let mut new_exprs = vec![];

                    for (pos, expr) in exprs.iter().with_position() {
                        let expected_ty = match pos {
                            itertools::Position::Last => expected_ty,
                            _ => Some(cx.db.types.unit),
                        };

                        new_exprs.push(check_expr(cx, env, expr, expected_ty)?);
                    }

                    let ty = new_exprs.last().unwrap().ty;

                    (new_exprs, ty)
                };

                Ok(cx.expr(hir::ExprKind::Block(hir::Block { exprs }), ty, *span))
            })
        }
        ast::Expr::Unsafe { expr, span } => {
            let expr = check_expr(cx, env, expr, expected_ty)?;
            let ty = expr.ty;
            Ok(cx.expr(hir::ExprKind::Unsafe(hir::Unsafe { expr: Box::new(expr) }), ty, *span))
        }
        ast::Expr::MethodCall { expr, method, targs, args, span } => {
            let targs = tyexpr::check_optional_targs(cx, env, targs.as_deref(), AllowTyHole::Yes)?;
            let mut args = check_call_args(cx, env, args)?;

            // Try looking up an associated function call first
            if let ast::Expr::Name { word, targs: name_targs, .. } = expr.as_ref() {
                let id = cx.lookup().with_env(env).query(
                    env.module_id(),
                    env.module_id(),
                    &Query::Name(*word),
                )?;
                let name_targs =
                    tyexpr::check_optional_targs(cx, env, name_targs.as_deref(), AllowTyHole::Yes)?;

                if let Some(assoc_ty) = types::try_extract_assoc_ty(cx, id) {
                    let query_args = map_call_args_for_query(cx, &args);
                    let query = FnQuery::new(*method, targs.as_deref(), &query_args, IsUfcs::No);

                    let (ty, _) = types::apply_targs_to_ty(
                        cx,
                        env,
                        assoc_ty.ty(cx.db),
                        name_targs.as_deref(),
                        *span,
                    )?;

                    let (callee, _) = check_query_in_ty(
                        cx,
                        env,
                        ty,
                        word.span(),
                        &Query::Fn(query),
                        targs.as_deref(),
                        *span,
                    )?;

                    return check_call(cx, callee, args, *span, IsUfcs::No);
                }
            }

            let expr = check_expr(cx, env, expr, expected_ty)?;

            let mut is_ufcs = IsUfcs::No;
            let lookup_in_module = match cx.normalize(expr.ty).kind() {
                TyKind::Module(in_module) => *in_module,
                TyKind::Type(ty) => {
                    // This is probably a union variant
                    let (callee, _) = check_query_in_ty(
                        cx,
                        env,
                        *ty,
                        *span,
                        &Query::Name(*method),
                        targs.as_deref(),
                        expr.span,
                    )?;
                    return check_call(cx, callee, args, *span, IsUfcs::No);
                }
                _ => {
                    // This is a UFCS call: add `expr` as the first argument of the call
                    is_ufcs = IsUfcs::Yes;
                    args.insert(0, hir::CallArg { name: None, expr, index: None });
                    env.module_id()
                }
            };

            let id = lookup_fn_for_call(
                cx,
                env,
                lookup_in_module,
                *method,
                targs.as_deref(),
                &args,
                is_ufcs,
            )?;

            let callee = check_name(cx, env, id, *method, *span, targs.as_deref())?;

            check_call(cx, callee, args, *span, is_ufcs)
        }
        ast::Expr::Call { callee, args, span } => {
            let args = check_call_args(cx, env, args)?;

            let callee = match callee.as_ref() {
                ast::Expr::Name { word, targs, span } => {
                    let targs =
                        tyexpr::check_optional_targs(cx, env, targs.as_deref(), AllowTyHole::Yes)?;

                    let id = lookup_fn_for_call(
                        cx,
                        env,
                        env.module_id(),
                        *word,
                        targs.as_deref(),
                        &args,
                        IsUfcs::No,
                    )?;

                    check_name(cx, env, id, *word, *span, targs.as_deref())?
                }
                _ => check_expr(cx, env, callee, None)?,
            };

            check_call(cx, callee, args, *span, IsUfcs::No)
        }
        ast::Expr::Unary { expr, op, span } => {
            let expr = check_expr(cx, env, expr, None)?;
            let ty = cx.normalize(expr.ty);

            match op {
                UnOp::Neg => {
                    if ty.is_any_int() || ty.is_any_float() {
                        Ok(cx.expr(
                            hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                            ty,
                            *span,
                        ))
                    } else {
                        Err(errors::invalid_un_op(cx.db, *op, ty, *span))
                    }
                }
                UnOp::Not => {
                    if ty.is_any_int() || ty.is_bool() {
                        Ok(cx.expr(
                            hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                            ty,
                            *span,
                        ))
                    } else {
                        Err(errors::invalid_un_op(cx.db, *op, ty, *span))
                    }
                }
                UnOp::Ref(mutability) => check_ref(cx, expr, ty, *op, *mutability, *span),
            }
        }
        ast::Expr::Binary { lhs, rhs, op, span } => {
            let lhs = check_expr(cx, env, lhs, None)?;
            let rhs = check_expr(cx, env, rhs, Some(lhs.ty))?;

            check_bin_op(cx, &lhs, &rhs, *op, *span)?;

            let result_ty = match op {
                BinOp::Cmp(..) => cx.db.types.bool,
                _ => lhs.ty,
            };

            Ok(cx.expr(
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
            let expected_ty = cx.fresh_ty_var().raw_ptr();
            let expr = check_expr(cx, env, expr, Some(expected_ty))?;

            match cx.normalize(expr.ty).auto_deref().kind() {
                TyKind::RawPtr(pointee) => Ok(cx.expr(
                    hir::ExprKind::Deref(hir::Deref { expr: Box::new(expr) }),
                    *pointee,
                    *span,
                )),
                ty => Err(errors::ty_mismatch(
                    &expected_ty.to_string(cx.db),
                    &ty.to_string(cx.db),
                    expr.span,
                )),
            }
        }
        ast::Expr::Cast { expr, target, span } => {
            let expr = check_expr(cx, env, expr, None)?;
            let target = tyexpr::check(cx, env, target, AllowTyHole::Yes)?;

            Ok(cx.expr(
                hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }),
                target,
                *span,
            ))
        }
        ast::Expr::Transmute { expr, target, span } => {
            let expr = check_expr(cx, env, expr, None)?;
            let target = tyexpr::check(cx, env, target, AllowTyHole::Yes)?;

            Ok(cx.expr(
                hir::ExprKind::Transmute(hir::Transmute { expr: Box::new(expr), target }),
                target,
                *span,
            ))
        }
        ast::Expr::Field { expr, field, span } => {
            let expr = check_expr(cx, env, expr, expected_ty)?;
            check_field(cx, env, expr, *field, *span)
        }
        ast::Expr::Index { expr, index, span } => {
            let expected_slice_ty = Ty::new(TyKind::Slice(cx.fresh_ty_var()));

            let expr = check_expr(cx, env, expr, Some(expected_slice_ty))?;
            let expr_ty = cx.normalize(expr.ty);

            let elem_ty = match expr_ty.auto_deref().kind() {
                TyKind::Slice(inner) => *inner,
                _ => {
                    return Err(errors::ty_mismatch(
                        &expected_slice_ty.to_string(cx.db),
                        &expr_ty.to_string(cx.db),
                        expr.span,
                    ))
                }
            };

            let uint = cx.db.types.uint;
            let index = check_expr(cx, env, index, Some(uint))?;
            cx.eq_obvious_expr(uint, &index)?;

            Ok(cx.expr(
                hir::ExprKind::Index(hir::Index { expr: Box::new(expr), index: Box::new(index) }),
                elem_ty,
                *span,
            ))
        }
        ast::Expr::Slice { expr, low, high, span } => {
            let expected_slice_ty = Ty::new(TyKind::Slice(cx.fresh_ty_var()));

            let expr = check_expr(cx, env, expr, Some(expected_slice_ty))?;
            let expr_ty = cx.normalize(expr.ty).auto_deref();
            cx.at(Obligation::obvious(expr.span))
                .eq(expected_slice_ty, expr_ty)
                .or_coerce(cx, expr.id)?;

            let uint = cx.db.types.uint;
            let low = if let Some(low) = low {
                let low = check_expr(cx, env, low, Some(uint))?;
                cx.eq_obvious_expr(uint, &low)?;
                Some(Box::new(low))
            } else {
                None
            };

            let high = if let Some(high) = high {
                let high = check_expr(cx, env, high, Some(uint))?;
                cx.eq_obvious_expr(uint, &high)?;
                Some(Box::new(high))
            } else {
                None
            };

            Ok(cx.expr(
                hir::ExprKind::Slice(hir::Slice { expr: Box::new(expr), low, high }),
                expr_ty,
                *span,
            ))
        }
        ast::Expr::Name { word, targs, span } => {
            let id = cx.lookup().with_env(env).query(
                env.module_id(),
                env.module_id(),
                &Query::Name(*word),
            )?;
            let targs = tyexpr::check_optional_targs(cx, env, targs.as_deref(), AllowTyHole::Yes)?;
            check_name(cx, env, id, *word, *span, targs.as_deref())
        }
        ast::Expr::SliceLit { exprs, span } => {
            let elem_ty = expected_ty
                .and_then(|t| cx.normalize(t).slice_elem())
                .unwrap_or_else(|| cx.fresh_ty_var());

            let mut new_exprs = vec![];

            for expr in exprs {
                let new_expr = check_expr(cx, env, expr, Some(elem_ty))?;
                cx.eq_obvious_expr(elem_ty, &new_expr)?;
                new_exprs.push(new_expr);
            }

            Ok(cx.expr(
                hir::ExprKind::SliceLit(hir::SliceLit { exprs: new_exprs, cap: None }),
                Ty::new(TyKind::Slice(elem_ty)),
                *span,
            ))
        }
        ast::Expr::SliceLitCap { cap, span } => {
            let uint = cx.db.types.uint;
            let cap = check_expr(cx, env, cap, Some(uint))?;
            cx.eq_obvious_expr(uint, &cap)?;

            Ok(cx.expr(
                hir::ExprKind::SliceLit(hir::SliceLit { exprs: vec![], cap: Some(Box::new(cap)) }),
                Ty::new(TyKind::Slice(cx.fresh_ty_var())),
                *span,
            ))
        }
        ast::Expr::BoolLit { value, span } => {
            Ok(cx.expr(hir::ExprKind::BoolLit(*value), cx.db.types.bool, *span))
        }
        ast::Expr::IntLit { value, span } => {
            Ok(cx.expr(hir::ExprKind::IntLit(*value), cx.fresh_int_var(), *span))
        }
        ast::Expr::FloatLit { value, span } => {
            Ok(cx.expr(hir::ExprKind::FloatLit(*value), cx.fresh_float_var(), *span))
        }
        ast::Expr::StrLit { value, span } => {
            Ok(cx.expr(hir::ExprKind::StrLit(*value), cx.db.types.str, *span))
        }
    }
}

fn check_if(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    cond: &ast::Expr,
    then: &ast::Expr,
    otherwise: Option<&ast::Expr>,
    span: Span,
    expected_ty: Option<Ty>,
) -> DiagnosticResult<hir::Expr> {
    let cond = check_expr(cx, env, cond, Some(cx.db.types.bool))?;
    cx.eq_obvious_expr(cx.db.types.bool, &cond)?;

    let then = check_expr(cx, env, then, expected_ty)?;

    let otherwise = if let Some(otherwise) = otherwise.as_ref() {
        let otherwise = check_expr(cx, env, otherwise, Some(then.ty))?;

        cx.at(Obligation::exprs(span, then.span, otherwise.span))
            .eq(then.ty, otherwise.ty)
            .or_coerce(cx, otherwise.id)?;

        otherwise
    } else {
        cx.eq_obvious_expr(cx.db.types.unit, &then)?;
        cx.unit_expr(span)
    };

    let ty = otherwise.ty;
    let cond_span = cond.span;

    Ok(cx.expr(
        hir::ExprKind::Match(hir::Match {
            expr: Box::new(cond),
            arms: vec![
                hir::MatchArm {
                    pat: hir::MatchPat::Bool(true, cond_span),
                    guard: None,
                    expr: Box::new(then),
                },
                hir::MatchArm {
                    pat: hir::MatchPat::Bool(false, cond_span),
                    guard: None,
                    expr: Box::new(otherwise),
                },
            ],
        }),
        ty,
        span,
    ))
}

fn check_name(
    cx: &mut Typeck<'_>,
    env: &Env,
    id: DefId,
    word: Word,
    span: Span,
    targs: Option<&[Ty]>,
) -> DiagnosticResult<hir::Expr> {
    if let DefKind::Adt(adt_id) = cx.db[id].kind.as_ref() {
        match &cx.db[*adt_id].kind {
            AdtKind::Struct(_) => {
                return check_name_struct(cx, env, id, word, span, targs, *adt_id);
            }
            AdtKind::Union(_) => (),
        }
    }

    let def_ty = cx.normalize(cx.def_ty(id));
    let (ty, instantiation) = types::apply_targs_to_ty(cx, env, def_ty, targs, span)?;

    Ok(cx.expr(hir::ExprKind::Name(hir::Name { id, word, instantiation }), ty, span))
}

fn check_name_struct(
    cx: &mut Typeck<'_>,
    env: &Env,
    id: DefId,
    word: Word,
    span: Span,
    targs: Option<&[Ty]>,
    adt_id: AdtId,
) -> DiagnosticResult<hir::Expr> {
    let adt = &cx.db[adt_id];
    let struct_def = adt.as_struct().unwrap();

    // NOTE: if the named definition is a struct, we want to return its
    // constructor function's type
    if struct_def.ctor_vis == Vis::Private && cx.db[adt.def_id].scope.module_id != env.module_id() {
        let private_field = struct_def
            .fields
            .iter()
            .find(|f| f.vis == Vis::Private)
            .expect("to have at least one private field");

        return Err(Diagnostic::error(format!(
            "constructor of type `{}` is private because `{}` is private",
            adt.name, private_field.name
        ))
        .with_label(Label::primary(span, "private type constructor"))
        .with_label(Label::secondary(
            private_field.name.span(),
            format!("`{}` is private", private_field.name),
        )));
    }

    let (ty, instantiation) = types::apply_targs_to_ty(cx, env, struct_def.ctor_ty, targs, span)?;

    Ok(cx.expr(hir::ExprKind::Name(hir::Name { id, word, instantiation }), ty, span))
}

fn check_field(
    cx: &mut Typeck<'_>,
    env: &Env,
    expr: hir::Expr,
    field: Word,
    span: Span,
) -> DiagnosticResult<hir::Expr> {
    let ty = cx.normalize(expr.ty).auto_deref();

    let res_ty = match ty.kind() {
        TyKind::Module(module_id) => {
            let id = cx.lookup().query(env.module_id(), *module_id, &Query::Name(field))?;
            return check_name(cx, env, id, field, span, None);
        }
        TyKind::Adt(adt_id, targs) => {
            let adt = &cx.db[*adt_id];

            match &adt.kind {
                AdtKind::Struct(struct_def) => {
                    if let Some(field) = struct_def.field_by_name(field.name().as_str()) {
                        check_field_access(cx, env, adt, field, span)?;
                        Some(adt.instantiation(targs).fold(field.ty))
                    } else {
                        None
                    }
                }
                AdtKind::Union(_) => None,
            }
        }
        TyKind::Type(ty) => {
            // This is a union variant
            let (expr, can_implicitly_call) =
                check_query_in_ty(cx, env, *ty, span, &Query::Name(field), None, expr.span)?;

            return if can_implicitly_call {
                check_call(cx, expr, vec![], span, IsUfcs::No)
            } else {
                Ok(expr)
            };
        }
        TyKind::Slice(..) if field.name() == sym::CAP => Some(cx.db.types.uint),
        TyKind::Slice(..) | TyKind::Str if field.name() == sym::LEN => Some(cx.db.types.uint),
        TyKind::Slice(elem_ty) if field.name() == sym::PTR => Some(elem_ty.raw_ptr()),
        TyKind::Str if field.name() == sym::PTR => Some(cx.db.types.u8.raw_ptr()),
        _ => None,
    };

    if let Some(res_ty) = res_ty {
        Ok(cx.expr(hir::ExprKind::Field(hir::Field { expr: Box::new(expr), field }), res_ty, span))
    } else {
        Err(errors::field_not_found(cx.db, ty, expr.span, field))
    }
}

pub(super) fn check_field_access(
    cx: &Typeck<'_>,
    env: &Env,
    adt: &Adt,
    field: &AdtField,
    span: Span,
) -> DiagnosticResult<()> {
    if field.vis == Vis::Private && cx.db[adt.def_id].scope.module_id != env.module_id() {
        return Err(Diagnostic::error(format!(
            "field `{}` of type `{}` is private",
            field.name, adt.name
        ))
        .with_label(Label::primary(span, "private field")));
    }

    Ok(())
}

/// Tries to look up `name` in the namespace of `ty`.
/// Returns the evaluated expression, and whether it can be implicitly
/// called.
fn check_query_in_ty(
    cx: &mut Typeck<'_>,
    env: &Env,
    ty: Ty,
    ty_span: Span,
    query: &Query,
    targs: Option<&[Ty]>,
    span: Span,
) -> DiagnosticResult<(hir::Expr, bool)> {
    match cx.lookup().query_assoc_ns(env.module_id(), ty, ty_span, query)? {
        AssocLookup::Variant(variant_id) => {
            let TyKind::Adt(adt_id, targs) = ty.kind() else { unreachable!() };

            let variant = &cx.db[variant_id];
            let adt = &cx.db[*adt_id];

            let instantiation = adt.instantiation(targs);
            let ctor_ty = instantiation.fold(variant.ctor_ty);

            // Union variants without fields are implicitly called for convenience
            let can_implicitly_call = variant.fields.is_empty();
            let expr = cx.expr(
                hir::ExprKind::Variant(hir::Variant { id: variant.id, instantiation }),
                ctor_ty,
                span,
            );

            Ok((expr, can_implicitly_call))
        }
        AssocLookup::AssocFn(id) => {
            let expr = check_name(cx, env, id, query.word(), span, targs)?;
            Ok((expr, false))
        }
    }
}

fn map_call_args_for_query(cx: &Typeck<'_>, args: &[hir::CallArg]) -> Vec<FnTyParam> {
    args.iter()
        .map(|a| FnTyParam { name: a.name.map(|w| w.name()), ty: cx.normalize(a.expr.ty) })
        .collect::<Vec<_>>()
}

fn lookup_fn_for_call(
    cx: &Typeck<'_>,
    env: &Env,
    in_module: ModuleId,
    word: Word,
    ty_args: Option<&[Ty]>,
    args: &[hir::CallArg],
    is_ufcs: IsUfcs,
) -> DiagnosticResult<DefId> {
    let args = map_call_args_for_query(cx, args);
    let query = FnQuery::new(word, ty_args, &args, is_ufcs);
    cx.lookup().with_env(env).query(env.module_id(), in_module, &Query::Fn(query))
}

fn check_call(
    cx: &mut Typeck<'_>,
    callee: hir::Expr,
    args: Vec<hir::CallArg>,
    span: Span,
    is_ufcs: IsUfcs,
) -> DiagnosticResult<hir::Expr> {
    let callee_ty = cx.normalize(callee.ty);

    match callee_ty.kind() {
        TyKind::Fn(fn_ty) => check_call_fn(cx, callee, args, fn_ty, span, is_ufcs),
        TyKind::Type(ty) => {
            if args.len() != 1 {
                return Err(errors::arg_mismatch(1, args.len(), span));
            }

            let arg = &args[0];

            if let Some(name) = arg.name {
                return Err(errors::named_param_not_found(name));
            }

            Ok(cx.expr(
                hir::ExprKind::Cast(hir::Cast { expr: Box::new(arg.expr.clone()), target: *ty }),
                *ty,
                span,
            ))
        }
        _ => {
            let ty = cx.normalize(callee.ty);
            let span = callee.span;

            Err(Diagnostic::error(format!("expected a function, found `{}`", ty.display(cx.db)))
                .with_label(Label::primary(span, "expected a function")))
        }
    }
}

fn check_call_fn(
    cx: &mut Typeck<'_>,
    callee: hir::Expr,
    mut args: Vec<hir::CallArg>,
    fn_ty: &FnTy,
    span: Span,
    is_ufcs: IsUfcs,
) -> DiagnosticResult<hir::Expr> {
    #[derive(Debug)]
    struct PassedArg {
        is_named: bool,
        span: Span,
    }

    if !fn_ty.is_c_variadic() && args.len() != fn_ty.params.len() {
        return Err(errors::arg_mismatch(fn_ty.params.len(), args.len(), span));
    }

    let mut already_passed_args = UstrMap::<PassedArg>::default();

    // Resolve positional arg indices
    for (idx, arg) in args.iter_mut().enumerate() {
        if arg.name.is_none() {
            arg.index = Some(idx);

            if let Some(param_name) = fn_ty.params.get(idx).and_then(|p| p.name) {
                already_passed_args
                    .insert(param_name, PassedArg { is_named: false, span: arg.expr.span });
            }
        }

        let arg_ty = cx.normalize(arg.expr.ty);

        if arg_ty.is_type() {
            return Err(errors::generic_expected_found(
                "a value",
                &format!("type `{}`", arg_ty.display(cx.db)),
                arg.expr.span,
            ));
        }
    }

    // Resolve named arg indices
    for arg in &mut args {
        if let Some(arg_name) = &arg.name {
            let name = arg_name.name();

            let idx = fn_ty
                .params
                .iter()
                .enumerate()
                .find_map(|(i, p)| if p.name == Some(name) { Some(i) } else { None })
                .ok_or_else(|| errors::named_param_not_found(*arg_name))?;

            // Report named arguments that are passed twice
            if let Some(passed_arg) = already_passed_args
                .insert(arg_name.name(), PassedArg { is_named: true, span: arg_name.span() })
            {
                let name = arg_name.name();
                let prev = passed_arg.span;
                let dup = arg_name.span();
                let is_named = passed_arg.is_named;

                return Err(Diagnostic::error(if is_named {
                    format!("argument `{name}` is passed multiple times")
                } else {
                    format!("argument `{name}` is already passed positionally")
                })
                .with_label(Label::primary(dup, format!("`{name}` is passed again here")))
                .with_label(Label::secondary(prev, format!("`{name}` is already passed here"))));
            }

            arg.index = Some(idx);
        }
    }

    // Unify all args with their corresponding param type
    for (arg_idx, arg) in args.iter().enumerate() {
        let param_idx = arg.index.expect("arg index to be resolved");

        if let Some(param) = fn_ty.params.get(param_idx) {
            let coerce_options = CoerceOptions {
                allow_owned_to_ref: is_ufcs == IsUfcs::Yes && arg_idx == 0,
                ..CoerceOptions::default()
            };
            cx.at(Obligation::obvious(arg.expr.span)).eq(param.ty, arg.expr.ty).or_coerce_ex(
                cx,
                arg.expr.id,
                coerce_options,
            )?;
        }
    }

    Ok(cx.expr(hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args }), fn_ty.ret, span))
}

fn check_ref(
    cx: &mut Typeck<'_>,
    expr: hir::Expr,
    ty: Ty,
    op: UnOp,
    mutability: Mutability,
    span: Span,
) -> DiagnosticResult<hir::Expr> {
    // if ty.can_create_ref(cx.db) {
    Ok(cx.expr(
        hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op }),
        ty.create_ref(mutability),
        span,
    ))
    // } else {
    //     Err(Diagnostic::error()
    //         .with_message(format!(
    //             "cannot take a reference to value of type `{}`",
    //             ty.display(cx.db)
    //         ))
    //         .with_label(Label::primary(span, "cannot take
    // reference"))) }
}

fn check_call_args(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    args: &[ast::CallArg],
) -> DiagnosticResult<Vec<hir::CallArg>> {
    let mut new_args = vec![];

    for arg in args {
        new_args.push(match arg {
            ast::CallArg::Named(name, expr) => hir::CallArg {
                name: Some(*name),
                expr: check_expr(cx, env, expr, None)?,
                index: None,
            },
            ast::CallArg::Positional(expr) => {
                hir::CallArg { name: None, expr: check_expr(cx, env, expr, None)?, index: None }
            }
        });
    }

    Ok(new_args)
}

fn check_bin_op(
    cx: &mut Typeck<'_>,
    lhs: &hir::Expr,
    rhs: &hir::Expr,
    op: BinOp,
    span: Span,
) -> DiagnosticResult<()> {
    fn can_use_eq(ty: Ty) -> bool {
        match ty.kind() {
            TyKind::Ref(ty, _) => can_use_eq(*ty),
            TyKind::RawPtr(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Bool
            | TyKind::Infer(InferTy::Int(_) | InferTy::Float(_)) => true,
            _ => false,
        }
    }

    match op {
        BinOp::And | BinOp::Or => {
            cx.eq_obvious_expr(cx.db.types.bool, lhs)?;
            cx.eq_obvious_expr(cx.db.types.bool, rhs)?;
        }
        BinOp::Cmp(CmpOp::Eq | CmpOp::Ne) => {
            cx.at(Obligation::exprs(span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(cx, rhs.id)?;

            let ty = cx.normalize(lhs.ty);
            if !can_use_eq(ty) {
                return Err(errors::invalid_bin_op(cx.db, op, ty, span));
            }
        }
        BinOp::Add => {
            if cx.normalize(lhs.ty).is_raw_ptr() {
                cx.eq_obvious_expr(cx.db.types.uint, rhs)?;
                return Ok(());
            }

            cx.at(Obligation::exprs(span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(cx, rhs.id)?;

            let ty = cx.normalize(lhs.ty);
            if !ty.is_any_int() && !ty.is_any_float() {
                return Err(errors::invalid_bin_op(cx.db, op, ty, span));
            }
        }
        BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Cmp(_) => {
            cx.at(Obligation::exprs(span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(cx, rhs.id)?;

            let ty = cx.normalize(lhs.ty);
            if !ty.is_any_int() && !ty.is_any_float() {
                return Err(errors::invalid_bin_op(cx.db, op, ty, span));
            }
        }
        BinOp::Rem | BinOp::Shl | BinOp::Shr | BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
            cx.at(Obligation::exprs(span, lhs.span, rhs.span))
                .eq(lhs.ty, rhs.ty)
                .or_coerce(cx, rhs.id)?;

            let ty = cx.normalize(lhs.ty);
            if !ty.is_any_int() {
                return Err(errors::invalid_bin_op(cx.db, op, ty, span));
            }
        }
    }

    Ok(())
}

fn check_assign_lhs(expr: &hir::Expr) -> DiagnosticResult<()> {
    check_assign_lhs_aux(expr).then_some(()).ok_or_else(|| {
        Diagnostic::error("invalid left-hand side of assignment")
            .with_label(Label::primary(expr.span, "expression is not assignable"))
    })
}

fn check_swap_lhs(expr: &hir::Expr) -> DiagnosticResult<()> {
    check_assign_lhs_aux(expr).then_some(()).ok_or_else(|| {
        Diagnostic::error("invalid left-hand side of swap")
            .with_label(Label::primary(expr.span, "expression is not swappable"))
    })
}

fn check_assign_lhs_aux(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Unsafe(uns) => check_assign_lhs_aux(&uns.expr),
        hir::ExprKind::Deref(_)
        | hir::ExprKind::Field(_)
        | hir::ExprKind::Index(_)
        | hir::ExprKind::Name(_) => true,
        hir::ExprKind::Slice(_)
        | hir::ExprKind::Match(_)
        | hir::ExprKind::Loop(_)
        | hir::ExprKind::Break
        | hir::ExprKind::Block(_)
        | hir::ExprKind::Return(_)
        | hir::ExprKind::Call(_)
        | hir::ExprKind::Unary(_)
        | hir::ExprKind::Binary(_)
        | hir::ExprKind::Cast(_)
        | hir::ExprKind::Transmute(_)
        | hir::ExprKind::Let(_)
        | hir::ExprKind::Assign(_)
        | hir::ExprKind::Swap(_)
        | hir::ExprKind::Variant(_)
        | hir::ExprKind::SliceLit(_)
        | hir::ExprKind::BoolLit(_)
        | hir::ExprKind::IntLit(_)
        | hir::ExprKind::FloatLit(_)
        | hir::ExprKind::StrLit(_) => false,
    }
}
