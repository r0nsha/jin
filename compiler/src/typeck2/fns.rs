use data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::ustr;

use crate::{
    ast,
    diagnostics::DiagnosticResult,
    hir,
    middle::{CallConv, Pat, TyExpr},
    span::{Span, Spanned},
    ty::{FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    typeck2::{errors, ns::Env, tyexpr, tyexpr::AllowTyHole, types, Typeck},
    word::{Word, WordMap},
};

pub(super) fn check_sig(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    sig: &ast::FnSig,
    callconv: CallConv,
    flags: FnTyFlags,
) -> DiagnosticResult<hir::FnSig> {
    let ty_params = types::define_ty_params(cx, env, &sig.ty_params)?;
    let (params, fnty_params) = check_fn_sig_params(cx, env, &sig.params)?;

    let ret = sig
        .ret
        .as_ref()
        .map(|ret| tyexpr::check(cx, env, ret, AllowTyHole::No))
        .transpose()?
        .unwrap_or(cx.db.types.unit);
    let ret_span = sig.ret.as_ref().map_or(sig.word.span(), Spanned::span);

    let ty = Ty::new(TyKind::Fn(FnTy { params: fnty_params, ret, callconv, flags }));
    Ok(hir::FnSig { word: sig.word, ty_params, params, ret, ret_span, ty })
}

pub(super) fn check_expr_sig(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    params: &[ast::FnParam],
    ret: Option<&TyExpr>,
    span: Span,
) -> DiagnosticResult<hir::FnSig> {
    let (params, fnty_params) = check_fn_sig_params(cx, env, params)?;
    let ret_span = ret.as_ref().map_or(span, |t| t.span());
    let ret = ret
        .map(|ret| tyexpr::check(cx, env, ret, AllowTyHole::Yes))
        .transpose()?
        .unwrap_or_else(|| cx.fresh_ty_var());

    let ty = Ty::new(TyKind::Fn(FnTy {
        params: fnty_params,
        ret,
        callconv: CallConv::default(),
        flags: FnTyFlags::empty(),
    }));

    let module_name = cx.db[env.module_id()].qpath.join_with("_");
    let name = ustr(&format!("closure_{}_{}", module_name, span.start()));
    let word = Word::new(name, span);

    Ok(hir::FnSig { word, ty_params: vec![], params, ret, ret_span, ty })
}

fn check_fn_sig_params(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    params: &[ast::FnParam],
) -> DiagnosticResult<(Vec<hir::FnParam>, Vec<FnTyParam>)> {
    let mut new_params: Vec<hir::FnParam> = vec![];
    let mut defined_params = WordMap::default();

    for p in params {
        let ty = p
            .ty_expr
            .as_ref()
            .map(|tex| tyexpr::check(cx, env, tex, AllowTyHole::No))
            .transpose()?
            .unwrap_or_else(|| cx.fresh_ty_var());

        let pat = cx.define().local_pat(env, &p.pat, ty);

        match &pat {
            Pat::Name(name) => {
                if let Some(prev_span) = defined_params.insert(name.word) {
                    return Err(errors::name_defined_twice("parameter", name.word, prev_span));
                }
            }
            Pat::Discard(_) => (),
        }

        new_params.push(hir::FnParam { pat, ty });
    }

    let fnty_params =
        new_params.iter().map(|p| FnTyParam { name: p.pat.name(), ty: p.ty }).collect();

    Ok((new_params, fnty_params))
}
