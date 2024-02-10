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
        lookup::{FnCandidate, Query},
        ns,
        ns::{AssocTy, Env, ScopeKind},
        tyexpr,
        tyexpr::AllowTyHole,
        types, ResolutionMap, Typeck,
    },
    word::{Word, WordMap},
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
            ast::Item::Fn(fun) => define_fn(cx, res_map, module.id, id, fun, None).map(|_| ())?,
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
) -> DiagnosticResult<DefId> {
    attrs::validate(&fun.attrs, attrs::Placement::from(&fun.kind))?;

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

    Ok(id)
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
            ast::Item::Fn(fun) => check_fn(cx, res_map, module.id, id, fun)?,
            ast::Item::Assoc(tyname, item) => {
                check_assoc_item(cx, res_map, module.id, id, *tyname, item)?
            }
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
) -> DiagnosticResult<()> {
    let &id = res_map.item_to_def.get(&item_id).expect("to be defined");
    check_fn_helper(cx, res_map, module_id, item_id, id, fun, None)
}

fn check_assoc_fn(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
    assoc_ty: AssocTy,
) -> DiagnosticResult<()> {
    check_assoc_name_overlap(cx, assoc_ty, fun.sig.word)?;
    let id = define_fn(cx, res_map, module_id, item_id, fun, Some(assoc_ty))?;
    check_fn_helper(cx, res_map, module_id, item_id, id, fun, Some(assoc_ty))
}

// Checks that a to-be-defined associated name doesn't overlap
// with an existing name/definition
fn check_assoc_name_overlap(
    cx: &Typeck<'_>,
    assoc_ty: AssocTy,
    name: Word,
) -> DiagnosticResult<()> {
    match assoc_ty {
        AssocTy::Adt(adt_id) => {
            let adt = &cx.db[adt_id];
            if let AdtKind::Union(union_def) = &adt.kind {
                if let Some(variant) =
                    union_def.variants(cx.db).find(|v| v.name.name() == name.name())
                {
                    return Err(Diagnostic::error(format!(
                        "cannot define associated name `{}` on type `{}`",
                        name, adt.name
                    ))
                    .with_label(Label::primary(name.span(), "defined again here"))
                    .with_label(Label::secondary(
                        variant.name.span(),
                        "variant already defined here",
                    )));
                }
            }

            Ok(())
        }
        AssocTy::BuiltinTy(_) => Ok(()),
    }
}

fn check_fn_helper(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    id: DefId,
    fun: &ast::Fn,
    assoc_ty: Option<AssocTy>,
) -> DiagnosticResult<()> {
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

    let mut env = Env::new(module_id);
    let sig = env.with_named_scope(fun.sig.word.name(), ScopeKind::Fn(DefId::null()), |env| {
        fns::check_sig(cx, env, &fun.sig, callconv, flags)
    })?;

    match &fun.kind {
        ast::FnKind::Bare { .. } => {
            let candidate =
                FnCandidate { id, word: sig.word, ty: sig.ty.as_fn().cloned().unwrap() };

            if let Some(assoc_ty) = assoc_ty {
                cx.define().assoc_fn_candidate(assoc_ty, candidate)?;
            } else {
                cx.define().fn_candidate(candidate)?;
            }
        }
        ast::FnKind::Extern { .. } => {
            check_intrinsic_fn(cx, module_id, fun, &sig, id)?;
        }
    }

    cx.def_to_ty.insert(id, sig.ty);
    res_map.item_to_sig.insert(item_id, sig);

    Ok(())
}

fn check_intrinsic_fn(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
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

        if !ns::in_std(cx.db, module_id) {
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

fn check_assoc_item(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tyname: Word,
    item: &ast::Item,
) -> DiagnosticResult<()> {
    let assoc_ty = check_assoc_item_ty(cx, module_id, tyname)?;

    match item {
        ast::Item::Fn(fun) => check_assoc_fn(cx, res_map, module_id, item_id, fun, assoc_ty),
        ast::Item::Let(_)
        | ast::Item::Type(_)
        | ast::Item::Import(_)
        | ast::Item::ExternLet(_)
        | ast::Item::ExternImport(_)
        | ast::Item::Assoc(_, _) => unreachable!(),
    }
}

fn check_assoc_item_ty(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    tyname: Word,
) -> DiagnosticResult<AssocTy> {
    let id = cx.lookup().query(module_id, module_id, &Query::Name(tyname))?;

    let Some(assoc_ty) = try_extract_assoc_ty(cx, id) else {
        return Err(Diagnostic::error(format!(
            "expected a type, found value of type `{}`",
            cx.def_ty(id).display(cx.db)
        ))
        .with_label(Label::primary(tyname.span(), "expected a type")));
    };

    let env_package = cx.db[module_id].package;

    match assoc_ty {
        AssocTy::Adt(adt_id) => {
            let ty_def = &cx.db[cx.db[adt_id].def_id];
            let ty_package = cx.db[ty_def.scope.module_id].package;

            if env_package != ty_package {
                return Err(Diagnostic::error(format!(
                    "cannot define associated name for foreign type `{}`",
                    cx.db[adt_id].name
                ))
                .with_label(Label::primary(
                    tyname.span(),
                    format!("type is defined in package `{ty_package}`"),
                ))
                .with_label(Label::secondary(ty_def.span, "defined here")));
            }
        }
        AssocTy::BuiltinTy(ty) => {
            if !ns::in_std(cx.db, module_id) {
                return Err(Diagnostic::error(format!(
                    "cannot define associated name for builtin type `{}`",
                    ty.display(cx.db)
                ))
                .with_label(Label::primary(tyname.span(), "builtin type")));
            }
        }
    }

    Ok(assoc_ty)
}

fn try_extract_assoc_ty(cx: &Typeck<'_>, id: DefId) -> Option<AssocTy> {
    let def = &cx.db[id];

    match def.kind.as_ref() {
        DefKind::Adt(adt_id) => Some(AssocTy::Adt(*adt_id)),
        DefKind::Ty(ty) => Some(AssocTy::BuiltinTy(*ty)),
        _ => None,
    }
}

pub(super) fn check_bodies(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (_, item, id) in ast.items_with_id() {
        match item {
            ast::Item::Let(let_) => {
                let pat = res_map.item_to_pat.remove(&id).expect("to be defined");
                let mut let_ = check_let_body(cx, pat, let_)?;
                cx.hir.lets.push_with_key(|id| {
                    let_.id = id;
                    let_
                });
            }
            ast::Item::Fn(fun) => {
                // TODO:
                // cx.hir.fns.push_with_key(|id| {
                //     fun.id = id;
                //     fun
                // });
            }
            ast::Item::Assoc(_, item) => {
                // TODO:
                // define_fn(cx, res_map, module.id, id, fun, None).map(|_| ())?
            }
            _ => (),
        }
    }

    Ok(())
}

pub(super) fn check_let_body(
    cx: &mut Typeck<'_>,
    pat: Pat,
    let_: &ast::Let,
) -> DiagnosticResult<hir::Let> {
    todo!()
    // let value = self.check_expr(env, &let_.value, Some(ty))?;
    // self.eq_obvious_expr(ty, &value)?;

    // if self.normalize(ty).is_module() {
    //     return Err(Diagnostic::error("cannot store a module as a value")
    //         .with_label(Label::primary(value.span, "expected a value")));
    // }

    // let value = if env.in_global_scope() {
    //     // We do this so that global variable initialization always includes
    // a block     // (required for destroys)
    //     self.expr_or_block(value)
    // } else {
    //     value
    // };

    // Ok(hir::Let {
    //     id: hir::LetId::null(),
    //     module_id: env.module_id(),
    //     pat,
    //     value: Box::new(value),
    //     ty,
    //     span: let_.span,
    // })
}
