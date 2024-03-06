use compiler_ast::{self as ast};
use compiler_core::db::Hook;
use compiler_core::middle::Mutability;
use compiler_core::{
    db::DefId,
    diagnostics::DiagnosticResult,
    hir,
    middle::{CallConv, Pat, TyExpr},
    span::{Span, Spanned},
    ty::{FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    word::{Word, WordMap},
};
use compiler_data_structures::index_vec::Key as _;
use ustr::ustr;

use crate::{
    coerce::CoerceExt as _,
    errors, exprs,
    ns::{Env, ScopeKind},
    tyexpr,
    tyexpr::AllowTyHole,
    types,
    unify::Obligation,
    Typeck,
};

pub(crate) fn check(cx: &mut Typeck, id: DefId) -> DiagnosticResult<()> {
    let Ok(hook) = Hook::try_from(cx.db[id].name.as_str()) else {
        return Ok(());
    };

    match hook {
        Hook::Destroy => check_destroy_hook(cx, id),
    }
}

fn check_destroy_hook(cx: &mut Typeck, id: DefId) -> DiagnosticResult<()> {
    let ty = cx.def_ty(id);
    let fn_ty = ty.as_fn().unwrap();

    let param_ty =
        if fn_ty.params.len() == 1 { fn_ty.params[0].ty } else { todo!("error: one param") };

    let adt_id = match param_ty.kind() {
        TyKind::Ref(ty, Mutability::Mut) => match ty.kind() {
            TyKind::Adt(adt_id, _) => *adt_id,
            ty => todo!("error: must be ref ty"),
        },
        _ => todo!("error: must be &mut [ref ty]"),
    };

    if !cx.db[adt_id].is_ref() {
        todo!("error: cannot define on value type")
    }

    if !fn_ty.ret.is_unit() {
        todo!("error: must be unit")
    }

    if !fn_ty.ret.is_unit() {
        todo!("error: must be unit")
    }

    let hook_pkg = cx.db[cx.db[id].scope.module_id].package;
    let adt_pkg = cx.db[cx.db[cx.db[adt_id].def_id].scope.module_id].package;

    if hook_pkg != adt_pkg {
        todo!("error: cannot define hook for a type that is defined in another package")
    }

    if let Some(prev_id) = cx.db.hooks.insert((adt_id, Hook::Destroy), id) {
        todo!("error: cannot define hook twice for the same type")
    }

    Ok(())
}
