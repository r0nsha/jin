use crate::{
    db::{DefKind, ModuleId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    macros::create_bool_enum,
    middle::TyExpr,
    span::{Span, Spanned},
    ty::{FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    typeck2::{errors, lookup::PathLookup, Typeck},
    word::Word,
};

pub(super) fn check(
    cx: &Typeck,
    in_module: ModuleId,
    ty: &TyExpr,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    match ty {
        TyExpr::Fn(fn_ty) => {
            let params = fn_ty
                .params
                .iter()
                .map(|ty| {
                    check(cx, in_module, ty, allow_hole).map(|ty| FnTyParam { name: None, ty })
                })
                .try_collect()?;

            let ret = fn_ty
                .ret
                .as_ref()
                .map(|ret| check(cx, in_module, ret, allow_hole))
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
            let inner = check(cx, in_module, inner, allow_hole)?;
            Ok(Ty::new(TyKind::Slice(inner)))
        }
        TyExpr::Ref(inner, mutability, _) => {
            let inner_ty = check(cx, in_module, inner, allow_hole)?;

            match inner_ty.kind() {
                TyKind::Adt(..) => Ok(inner_ty.create_ref(*mutability)),
                TyKind::Param(_)
                | TyKind::Fn(_)
                | TyKind::Slice(_)
                | TyKind::RawPtr(_)
                | TyKind::Int(_)
                | TyKind::Uint(_)
                | TyKind::Float(_)
                | TyKind::Str
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
            let pointee = check(cx, in_module, pointee, allow_hole)?;
            Ok(Ty::new(TyKind::RawPtr(pointee)))
        }
        TyExpr::Path(path, targs, span) => {
            check_path(cx, in_module, path, targs.as_deref(), *span, allow_hole)
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
    cx: &Typeck,
    in_module: ModuleId,
    path: &[Word],
    targs: Option<&[TyExpr]>,
    span: Span,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Ty> {
    let result = cx.lookup().path(in_module, path)?;
    match result {
        PathLookup::Def(id) => {
            let def = &cx.db[id];

            match def.kind.as_ref() {
                DefKind::Ty(ty) => {
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
                    let targs = check_optional_targs(cx, in_module, targs, allow_hole)?;

                    let ty_params = &cx.db[adt_id].ty_params;
                    let targs_len = targs.as_ref().map_or(0, Vec::len);

                    if targs_len == ty_params.len() {
                        Ok(Ty::new(TyKind::Adt(adt_id, targs.unwrap_or_default())))
                    } else {
                        Err(errors::adt_ty_arg_mismatch(cx.db, adt_id, targs_len, span))
                    }
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

pub(super) fn check_optional_targs(
    cx: &Typeck,
    in_module: ModuleId,
    targs: Option<&[TyExpr]>,
    allow_hole: AllowTyHole,
) -> DiagnosticResult<Option<Vec<Ty>>> {
    targs
        .map(|targs| targs.iter().map(|arg| check(cx, in_module, arg, allow_hole)).try_collect())
        .transpose()
}

create_bool_enum!(AllowTyHole);
