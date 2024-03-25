use compiler_core::diagnostics::{Diagnostic, Label};
use compiler_core::hir;
use compiler_core::span::Spanned;
use compiler_core::{
    db::{DefId, Hook},
    diagnostics::DiagnosticResult,
    middle::Mutability,
    ty::TyKind,
};

use crate::Typeck;

pub(crate) fn check(cx: &mut Typeck, sig: &hir::FnSig, id: DefId) -> DiagnosticResult<()> {
    let Ok(hook) = Hook::try_from(sig.word.as_str()) else {
        return Ok(());
    };

    match hook {
        Hook::Drop => check_drop_hook(cx, sig, id),
    }
}

fn check_drop_hook(cx: &mut Typeck, sig: &hir::FnSig, id: DefId) -> DiagnosticResult<()> {
    let fn_ty = sig.ty.as_fn().unwrap();

    let param = if fn_ty.params.len() == 1 {
        &fn_ty.params[0]
    } else {
        return Err(Diagnostic::error("drop hook must have exactly 1 parameter").with_label(
            Label::primary(sig.word.span(), format!("has {} parameters", fn_ty.params.len())),
        ));
    };

    let (adt_id, targs) = match param.ty.kind() {
        TyKind::Ref(ty, Mutability::Mut) => match ty.kind() {
            TyKind::Adt(adt_id, targs) => (*adt_id, targs),
            ty => {
                return Err(Diagnostic::error(format!(
                    "cannot define drop hook for type `{}`",
                    ty.display(cx.db)
                ))
                .with_label(Label::primary(sig.params[0].pat.span(), "invalid parameter type")))
            }
        },
        _ => {
            return Err(Diagnostic::error("parameter must be a &mut reference")
                .with_label(Label::primary(sig.params[0].pat.span(), "invalid parameter type")))
        }
    };

    if !cx.db[adt_id].is_ref() {
        return Err(Diagnostic::error(format!(
            "cannot define drop hook for value type `{}`",
            cx.db[adt_id].name
        ))
        .with_label(Label::primary(sig.params[0].pat.span(), "parameter is a value type")));
    }

    let any_non_poly_targs =
        targs.iter().any(|ty| ty.walk_short(|ty| !matches!(ty.kind(), TyKind::Param(_))));
    if any_non_poly_targs {
        return Err(Diagnostic::error("drop hook cannot be specialized")
            .with_label(Label::primary(
                sig.params[0].pat.span(),
                "has non-polymorphic type arguments",
            ))
            .with_note("change parameter type to only contain polymorphic type arguments"));
    }

    if !fn_ty.ret.is_unit() {
        return Err(Diagnostic::error("drop hook must return `()`")
            .with_label(Label::primary(sig.ret_span, "invalid return type")));
    }

    let hook_pkg = cx.db[cx.db[id].scope.module_id].package;
    let adt_pkg = cx.db[cx.db[cx.db[adt_id].def_id].scope.module_id].package;

    if hook_pkg != adt_pkg {
        return Err(Diagnostic::error(format!(
            "cannot define drop hook for type `{}` that is defined outside this package",
            cx.db[adt_id].name,
        ))
        .with_label(Label::primary(sig.word.span(), "cannot define hook for foreign type"))
        .with_note(format!(
            "type `{}` is defined outside this package, in package `{}`",
            cx.db[adt_id].name, adt_pkg
        )));
    }

    if let Some(prev_id) = cx.db.hooks.insert((adt_id, Hook::Drop), id) {
        return Err(Diagnostic::error(format!(
            "drop hook is already defined for type `{}`",
            cx.db[adt_id].name,
        ))
        .with_label(Label::primary(sig.word.span(), "defined here"))
        .with_label(Label::secondary(cx.db[prev_id].span, "also defined here")));
    }

    Ok(())
}
