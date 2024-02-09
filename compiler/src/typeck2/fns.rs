use data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::ustr;

use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{
        AdtId, AdtKind, Def, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, StructDef, UnionDef,
        Variant, VariantId,
    },
    diagnostics::DiagnosticResult,
    hir,
    middle::{CallConv, Mutability, NamePat, Pat},
    qpath::QPath,
    span::Spanned,
    ty::{FnTy, FnTyFlags, FnTyParam, Ty, TyKind},
    typeck2::{
        attrs, errors,
        ns::{AssocTy, Env, ScopeKind},
        tyexpr,
        tyexpr::AllowTyHole,
        types, ResolutionMap, Typeck,
    },
    word::WordMap,
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
