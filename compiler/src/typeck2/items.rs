use data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::ustr;

use crate::{
    ast,
    ast::Ast,
    db::{
        AdtId, AdtKind, Def, DefId, DefKind, FnInfo, Intrinsic, ModuleId, ScopeInfo, ScopeLevel,
        StructDef, UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    hir,
    middle::{CallConv, Mutability, Pat},
    qpath::QPath,
    span::Spanned as _,
    ty::{FnTyFlags, Ty, TyKind},
    typeck2::{
        attrs, errors, fns,
        lookup::FnCandidate,
        ns::{AssocTy, Env, ScopeKind},
        tyexpr,
        tyexpr::AllowTyHole,
        types, ResolutionMap, Typeck,
    },
    word::WordMap,
};

pub(super) fn define(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        match item {
            ast::Item::Let(let_) => define_let(cx, res_map, module.id, id, let_)?,
            ast::Item::ExternLet(let_) => define_extern_let(cx, res_map, module.id, id, let_)?,
            ast::Item::Fn(fun) => define_fn(cx, res_map, module.id, id, fun, None)?,
            ast::Item::Type(tydef) => define_tydef(cx, res_map, module.id, id, tydef)?,
            ast::Item::ExternImport(import) => {
                attrs::validate(&import.attrs, attrs::Placement::ExternImport)?;
                cx.db.extern_libs.insert(import.lib.clone());
            }
            _ => (),
        }
    }

    Ok(())
}

fn define_let(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::Let,
) -> DiagnosticResult<()> {
    attrs::validate(&let_.attrs, attrs::Placement::Let)?;
    let unknown = cx.db.types.unknown;
    let pat = cx.define().global_pat(module_id, &let_.pat, unknown)?;
    res_map.item_to_pat.insert(item_id, pat);
    Ok(())
}

fn define_extern_let(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::ExternLet,
) -> DiagnosticResult<()> {
    attrs::validate(&let_.attrs, attrs::Placement::ExternLet)?;

    let id = cx.define().new_global(
        module_id,
        let_.vis,
        DefKind::ExternGlobal,
        let_.word,
        let_.mutability,
    )?;

    res_map.item_to_def.insert(item_id, id);

    Ok(())
}

fn define_fn(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
    assoc_ty: Option<AssocTy>,
) -> DiagnosticResult<()> {
    attrs::validate(
        &fun.attrs,
        match &fun.kind {
            ast::FnKind::Bare { .. } => attrs::Placement::Fn,
            ast::FnKind::Extern { .. } => attrs::Placement::ExternFn,
        },
    )?;

    let name = fun.sig.word.name();

    let id = match &fun.kind {
        ast::FnKind::Bare { .. } => {
            let base_qpath = if let Some(assoc_ty) = assoc_ty {
                match assoc_ty {
                    AssocTy::Adt(adt_id) => cx.db[cx.db[adt_id].def_id].qpath.clone(),
                    AssocTy::BuiltinTy(ty) => QPath::from(ustr(&ty.display(cx.db).to_string())),
                }
            } else {
                cx.db[module_id].qpath.clone()
            };

            let qpath = base_qpath.child(name);

            let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis: fun.vis };

            if let Some(def) = cx.global_env.module(module_id).ns.defs.get(&name) {
                return Err(errors::multiple_item_def_err(def.span, fun.sig.word));
            }

            let id = Def::alloc(
                cx.db,
                qpath,
                scope,
                DefKind::Fn(FnInfo::Bare),
                Mutability::Imm,
                fun.sig.word.span(),
            );

            cx.global_env.module_mut(module_id).ns.defined_fns.entry(name).or_default().push(id);

            id
        }
        ast::FnKind::Extern { .. } => cx.define().new_global(
            module_id,
            fun.vis,
            DefKind::Fn(FnInfo::Extern),
            fun.sig.word,
            Mutability::Imm,
        )?,
    };

    res_map.item_to_def.insert(item_id, id);

    Ok(())
}

fn define_tydef(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tydef: &ast::TyDef,
) -> DiagnosticResult<()> {
    let mut env = Env::new(module_id);

    let adt_id = env.with_anon_scope(ScopeKind::TyDef, |env| -> DiagnosticResult<AdtId> {
        let (adt_id, _) = match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => {
                attrs::validate(&tydef.attrs, attrs::Placement::Struct)?;
                let unknown = cx.db.types.unknown;
                cx.define().adt(module_id, tydef, |id| {
                    AdtKind::Struct(StructDef::new(
                        id,
                        vec![],
                        struct_def.kind,
                        unknown, // Will be filled later
                    ))
                })?
            }
            ast::TyDefKind::Union(union_def) => {
                attrs::validate(&tydef.attrs, attrs::Placement::Union)?;
                let variants = define_variants(cx, union_def)?;
                cx.define().adt(module_id, tydef, |id| {
                    AdtKind::Union(UnionDef::new(id, union_def.kind, variants))
                })?
            }
        };

        check_adt_ty_params(cx, env, tydef, adt_id)?;

        Ok(adt_id)
    })?;

    res_map.item_to_adt.insert(item_id, adt_id);

    Ok(())
}

fn define_variants(
    cx: &mut Typeck<'_>,
    union_def: &ast::UnionTyDef,
) -> DiagnosticResult<Vec<VariantId>> {
    let mut variants = vec![];
    let mut defined_variants = WordMap::default();

    for (index, variant) in union_def.variants.iter().enumerate() {
        if let Some(prev_span) = defined_variants.insert(variant.name) {
            return Err(errors::name_defined_twice("variant", variant.name, prev_span));
        }

        let unknown = cx.db.types.unknown;
        let id = cx.db.variants.push_with_key(|id| Variant {
            id,
            adt_id: AdtId::null(),
            index,
            name: variant.name,
            fields: vec![],
            ctor_ty: unknown,
        });

        variants.push(id);
    }

    Ok(variants)
}

fn check_adt_ty_params(
    cx: &mut Typeck,
    env: &mut Env,
    tydef: &ast::TyDef,
    adt_id: AdtId,
) -> DiagnosticResult<()> {
    let ty_params = types::define_ty_params(cx, env, &tydef.ty_params)?;
    cx.db[adt_id].ty_params = ty_params;
    let adt = &cx.db[adt_id];
    cx.def_to_ty.insert(adt.def_id, TyKind::Type(adt.ty()).into());
    Ok(())
}

pub(super) fn check_sigs(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        match item {
            ast::Item::Let(let_) => check_let_item(cx, res_map, module.id, id, let_)?,
            ast::Item::ExternLet(let_) => check_extern_let(cx, res_map, module.id, id, let_)?,
            ast::Item::Fn(fun) => check_fn(cx, res_map, module.id, id, fun, None)?,
            // TODO: ast::Item::Assoc(word, item) => todo!(),
            _ => (),
        }
    }

    Ok(())
}

pub(super) fn check_let_item(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::Let,
) -> DiagnosticResult<()> {
    let env = Env::new(module_id);
    let pat = res_map.item_to_pat.get_mut(&item_id).expect("to be defined");

    debug_assert!(let_.ty_expr.is_some());
    let ty = tyexpr::check_optional(cx, &env, let_.ty_expr.as_ref(), AllowTyHole::No)?;
    assign_pat_ty(cx, pat, ty);

    Ok(())
}

fn check_extern_let(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::ExternLet,
) -> DiagnosticResult<()> {
    let env = Env::new(module_id);
    let id = res_map.item_to_def.remove(&item_id).expect("to be defined");

    let ty = tyexpr::check(cx, &env, &let_.ty_expr, AllowTyHole::No)?;
    cx.hir.extern_lets.push(hir::ExternLet { module_id, id, word: let_.word, ty, span: let_.span });

    Ok(())
}

fn check_fn(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
    assoc_ty: Option<AssocTy>,
) -> DiagnosticResult<()> {
    let mut env = Env::new(module_id);
    let &id = res_map.item_to_def.get(&item_id).expect("to be defined");
    let sig = env.with_named_scope(fun.sig.word.name(), ScopeKind::Fn(DefId::null()), |env| {
        check_fn_item_helper(cx, env, fun)
    })?;

    match &fun.kind {
        ast::FnKind::Bare { .. } => {
            let candidate =
                FnCandidate { id, word: sig.word, ty: sig.ty.as_fn().cloned().unwrap() };

            if let Some(ty) = assoc_ty {
                todo!()
                // self.insert_fn_candidate_in_ty(ty, sig.word.name(),
                // symbol.module_id, candidate)?;
            } else {
                cx.define().fn_candidate(candidate)?;
            }
        }
        ast::FnKind::Extern { .. } => {
            check_intrinsic_fn(cx, &env, fun, &sig, id)?;
        }
    }

    cx.def_to_ty.insert(id, sig.ty);
    res_map.item_to_sig.insert(item_id, sig);

    Ok(())
}

fn check_fn_item_helper(
    cx: &mut Typeck,
    env: &mut Env,
    fun: &ast::Fn,
) -> DiagnosticResult<hir::FnSig> {
    let mut flags = FnTyFlags::empty();

    let callconv = match &fun.kind {
        ast::FnKind::Bare { .. } => CallConv::default(),
        ast::FnKind::Extern { callconv, is_c_variadic } => {
            flags.insert(FnTyFlags::EXTERN);

            if *is_c_variadic {
                flags.insert(FnTyFlags::C_VARIADIC);
            }

            *callconv
        }
    };

    fns::check_sig(cx, env, &fun.sig, callconv, flags)
}

fn check_intrinsic_fn(
    cx: &mut Typeck<'_>,
    env: &Env,
    fun: &ast::Fn,
    sig: &hir::FnSig,
    id: DefId,
) -> DiagnosticResult<()> {
    let fnty = sig.ty.as_fn().unwrap();

    if let Some(attr) = fun.attrs.find(ast::AttrId::Intrinsic) {
        let ast::AttrArgs::Intrinsic(name) = attr.args else { unreachable!() };

        let intrinsic = Intrinsic::try_from(name.as_str()).map_err(|()| {
            Diagnostic::error(format!("unknown intrinsic `{name}`"))
                .with_label(Label::primary(name.span(), "unknown intrinsic"))
        })?;

        if fnty.callconv != CallConv::Jin {
            return Err(Diagnostic::error("intrinsic calling convention must be \"jin\"")
                .with_label(Label::primary(fun.sig.word.span(), "invalid calling convention")));
        }

        if !env.in_std(cx.db) {
            return Err(Diagnostic::error("intrinsic cannot be defined outside the `std` package")
                .with_label(Label::primary(
                    fun.sig.word.span(),
                    "cannot be defined outside `std`",
                )));
        }

        cx.db.intrinsics.insert(id, intrinsic);
    } else if fnty.is_extern() && !sig.ty_params.is_empty() {
        return Err(Diagnostic::error("type parameters are not allowed on extern functions")
            .with_label(Label::primary(sig.word.span(), "type parameters not allowed")));
    }

    Ok(())
}

fn assign_pat_ty(cx: &mut Typeck<'_>, pat: &mut Pat, ty: Ty) {
    match pat {
        Pat::Name(name) => {
            debug_assert!(!name.id.is_null());
            cx.def_to_ty.insert(name.id, ty);
        }
        Pat::Discard(_) => (),
    }
}
