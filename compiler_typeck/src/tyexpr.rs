use std::rc::Rc;

use compiler_core::db::DefId;
use compiler_core::{
    db::DefKind,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::TyExpr,
    span::{Span, Spanned},
    ty::{FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    word::Word,
};
use compiler_helpers::create_bool_enum;
use ustr::Ustr;

use crate::ns::ScopeKind;
use crate::{errors, lookup::PathLookup, ns::Env, Typeck};

pub(crate) fn check(
    cx: &mut Typeck,
    env: &Env,
    ty: &TyExpr,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    match ty {
        TyExpr::Fn(fn_ty) => {
            let mut params = vec![];
            for tyexpr in &fn_ty.params {
                let ty =
                    check(cx, env, tyexpr, allow_hole).map(|ty| FnTyParam { name: None, ty })?;
                params.push(ty);
            }

            let ret = fn_ty
                .ret
                .as_ref()
                .map(|ret| check(cx, env, ret, allow_hole))
                .transpose()?
                .unwrap_or(cx.db.types.unit);

            let mut flags = FnTyFlags::empty();

            if fn_ty.is_extern {
                flags.insert(FnTyFlags::EXTERN);
            }

            if fn_ty.is_c_variadic {
                flags.insert(FnTyFlags::C_VARIADIC);
            }

            Ok(Ty::new(TyKind::Fn(FnTy { params, ret, callconv: fn_ty.callconv, flags })))
        }
        TyExpr::Slice(inner, _) => {
            let inner = check(cx, env, inner, allow_hole)?;
            Ok(Ty::new(TyKind::Slice(inner)))
        }
        TyExpr::Ref(inner, mutability, _) => {
            let inner_ty = check(cx, env, inner, allow_hole)?;

            match inner_ty.kind() {
                TyKind::Adt(..)
                | TyKind::Param(_)
                | TyKind::Fn(_)
                | TyKind::Slice(_)
                | TyKind::RawPtr(_)
                | TyKind::Int(_)
                | TyKind::Uint(_)
                | TyKind::Float(_)
                | TyKind::Str
                | TyKind::Char
                | TyKind::Bool
                | TyKind::Unit => Ok(inner_ty.create_ref(*mutability)),
                TyKind::Ref(..)
                | TyKind::Never
                | TyKind::Infer(_)
                | TyKind::Type(_)
                | TyKind::Module(_)
                | TyKind::Unknown => Err(Diagnostic::error(format!(
                    "type `{}` cannot be referenced",
                    inner_ty.display(cx.db)
                ))
                .with_label(Label::primary(inner.span(), "invalid referenced type"))),
            }
        }
        TyExpr::RawPtr(pointee, _) => {
            let pointee = check(cx, env, pointee, allow_hole)?;
            Ok(Ty::new(TyKind::RawPtr(pointee)))
        }
        TyExpr::Path(path, targs, span) => {
            check_path(cx, env, path, targs.as_deref(), *span, allow_hole)
        }
        TyExpr::Unit(_) => Ok(cx.db.types.unit),
        TyExpr::Hole(span) => {
            if allow_hole == AllowTyHole::Yes {
                Ok(cx.fresh_ty_var())
            } else {
                Err(Diagnostic::error("the _ type is invalid in this context")
                    .with_label(Label::primary(*span, "invalid type hole")))
            }
        }
    }
}

fn check_path(
    cx: &mut Typeck,
    env: &Env,
    path: &[Word],
    targs: Option<&[TyExpr]>,
    span: Span,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    let result = cx.lookup().with_env(env).path(env.module_id(), path)?;

    match result {
        PathLookup::Def(id) => {
            let def = &cx.db[id];

            match def.kind.as_ref() {
                DefKind::BuiltinTy(ty) => {
                    if targs.is_some() {
                        Err(Diagnostic::error(format!(
                            "type `{}` doesn't expect any type arguments",
                            ty.display(cx.db)
                        ))
                        .with_label(Label::primary(span, "unexpected type arguments")))
                    } else {
                        Ok(*ty)
                    }
                }
                &DefKind::Adt(adt_id) => {
                    let ty_params = &cx.db[adt_id].ty_params;
                    let targs = check_optional_targs_exact(
                        cx,
                        env,
                        cx.db[adt_id].name.name(),
                        targs,
                        ty_params.len(),
                        allow_hole,
                        span,
                    )?;
                    Ok(Ty::new(TyKind::Adt(adt_id, targs.unwrap_or_default())))
                }
                DefKind::TyAlias => {
                    let ty = check_ty_alias(cx, id, allow_hole)?;
                    let targs = check_optional_targs_exact(
                        cx,
                        env,
                        cx.db[id].name,
                        targs,
                        cx.ty_aliases[&id].ty_params.len(),
                        allow_hole,
                        span,
                    )?;
                    let instantiation =
                        cx.ty_aliases[&id].instantiation(&targs.unwrap_or_default());
                    Ok(instantiation.fold(ty))
                }
                _ => Err(Diagnostic::error(format!(
                    "expected a type, found value of type `{}`",
                    cx.def_ty(id).display(cx.db)
                ))
                .with_label(Label::primary(span, "expected a type"))),
            }
        }
        PathLookup::Variant(variant_id) => {
            let variant = &cx.db[variant_id];

            Err(Diagnostic::error(format!(
                "expected a type, found variant `{}` of type `{}`",
                variant.name, cx.db[variant.adt_id].name
            ))
            .with_label(Label::primary(span, "expected type, found variant")))
        }
    }
}

pub(crate) fn check_optional(
    cx: &mut Typeck,
    env: &Env,
    ty: Option<&TyExpr>,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    if let Some(ty) = ty {
        check(cx, env, ty, allow_hole)
    } else {
        Ok(cx.fresh_ty_var())
    }
}

pub(crate) fn check_optional_targs(
    cx: &mut Typeck,
    env: &Env,
    targs: Option<&[TyExpr]>,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Option<Vec<Ty>>> {
    if let Some(targs) = targs {
        let mut new_targs = vec![];
        for arg in targs {
            new_targs.push(check(cx, env, arg, allow_hole)?);
        }
        Ok(Some(new_targs))
    } else {
        Ok(None)
    }
}

pub(crate) fn check_optional_targs_exact(
    cx: &mut Typeck,
    env: &Env,
    ty_name: Ustr,
    targs: Option<&[TyExpr]>,
    ty_params_len: usize,
    allow_hole: AllowTyHole,
    span: Span,
) -> DiagnosticResult<Option<Vec<Ty>>> {
    let targs = check_optional_targs(cx, env, targs, allow_hole)?;
    let targs_len = targs.as_ref().map_or(0, Vec::len);

    if targs_len != ty_params_len {
        return Err(errors::adt_ty_arg_mismatch(&ty_name, targs_len, ty_params_len, span));
    }

    Ok(targs)
}

pub(crate) fn check_ty_alias(
    cx: &mut Typeck,
    id: DefId,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    if let Some(ty) = cx.ty_aliases[&id].ty {
        return Ok(ty);
    }

    let mut env = Env::new(cx.db[id].scope.module_id);

    env.with_anon_scope(ScopeKind::TyDef, |env| {
        for tp in &cx.ty_aliases[&id].ty_params {
            env.insert(tp.word.name(), tp.id);
        }

        let tyexpr = Rc::clone(
            cx.ty_aliases[&id]
                .tyexpr
                .as_ref()
                .expect("unresolved type aliases must have a type expression to resolve"),
        );
        self::check(cx, env, &tyexpr, allow_hole)
    })
}

create_bool_enum!(AllowTyHole);
