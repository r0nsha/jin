use std::rc::Rc;

use compiler_ast::{self as ast, Ast};
use compiler_core::{
    db::{
        AdtId, AdtKind, Builtin, Def, DefId, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel,
        StructDef, UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    hir,
    middle::{CallConv, Mutability, NamePat, Pat},
    qpath::QPath,
    span::Spanned as _,
    ty::{FnTyFlags, Ty, TyKind},
    word::{Word, WordMap},
};
use compiler_data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::ustr;

use crate::{
    attrs, errors, exprs, fns,
    lookup::{FnCandidate, Query},
    ns,
    ns::{AssocTy, Env, ScopeKind},
    tyexpr,
    tyexpr::AllowTyHole,
    types, TyAlias, Typeck,
};
use crate::{helpers, hooks};

pub(crate) fn define(cx: &mut Typeck, ast: &Ast) {
    for (module, item, id) in ast.items_with_id() {
        match item {
            ast::Item::Let(let_) => define_let(cx, module.id, id, let_),
            ast::Item::ExternLet(let_) => define_extern_let(cx, module.id, id, let_),
            ast::Item::Fn(fun) => {
                if let Err(diagnostic) = define_fn(cx, module.id, id, fun, None) {
                    cx.db.diagnostics.add(diagnostic);
                }
            }
            ast::Item::Type(tydef) => define_tydef(cx, module.id, id, tydef),
            ast::Item::ExternImport(import) => {
                attrs::validate(cx, &import.attrs, attrs::Placement::ExternImport);
                cx.db.extern_libs.insert(import.lib.clone());
            }
            _ => (),
        }
    }
}

fn define_let(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::Let,
) {
    attrs::validate(cx, &let_.attrs, attrs::Placement::Let);
    let unknown = cx.db.types.unknown;
    let pat = cx.define().global_pat(
        module_id,
        &let_.pat,
        match &let_.kind {
            ast::LetKind::Let => DefKind::Global,
            ast::LetKind::Const => DefKind::Const,
        },
        unknown,
    );
    cx.res_map.item_to_pat.insert(item_id, pat);
}

fn define_extern_let(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::ExternLet,
) {
    attrs::validate(cx, &let_.attrs, attrs::Placement::ExternLet);

    let id = cx.define().new_global(
        module_id,
        let_.vis,
        DefKind::ExternGlobal,
        let_.word,
        let_.mutability,
    );

    cx.res_map.item_to_def.insert(item_id, id);
}

fn define_fn(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
    assoc_ty: Option<AssocTy>,
) -> DiagnosticResult<DefId> {
    attrs::validate(cx, &fun.attrs, attrs::Placement::from(&fun.kind));

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

            if let Some(prev) = cx.global_env.module(module_id).ns.defs.get(&name) {
                return Err(errors::multiple_item_def_err(prev.span(), fun.sig.word));
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
        ),
    };

    cx.res_map.item_to_def.insert(item_id, id);

    Ok(id)
}

fn define_tydef(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tydef: &ast::TyDef,
) {
    attrs::validate(
        cx,
        &tydef.attrs,
        match &tydef.kind {
            ast::TyDefKind::Struct(_) => attrs::Placement::Struct,
            ast::TyDefKind::Union(_) => attrs::Placement::Union,
            ast::TyDefKind::Alias(_) => attrs::Placement::TyAlias,
        },
    );

    let mut env = Env::new(module_id);

    env.with_anon_scope(ScopeKind::TyDef, |env| {
        match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => {
                let unknown = cx.db.types.unknown;
                let adt_id = cx.define().adt(module_id, tydef, |id| {
                    AdtKind::Struct(StructDef::new(
                        id,
                        vec![],
                        struct_def.kind,
                        unknown, // Will be filled later
                    ))
                });
                check_adt_tparams(cx, env, tydef, adt_id);
                cx.res_map.item_to_adt.insert(item_id, adt_id);
            }
            ast::TyDefKind::Union(union_def) => {
                let adt_id = cx
                    .define()
                    .adt(module_id, tydef, |id| AdtKind::Union(UnionDef::new(id, union_def.kind)));
                let variants = define_variants(cx, union_def, adt_id);
                cx.db[adt_id].as_union_mut().unwrap().variants = variants;
                check_adt_tparams(cx, env, tydef, adt_id);
                cx.res_map.item_to_adt.insert(item_id, adt_id);
            }
            ast::TyDefKind::Alias(alias) => {
                let def_id = cx.define().new_global(
                    module_id,
                    tydef.vis,
                    DefKind::TyAlias,
                    tydef.word,
                    Mutability::Imm,
                );

                let tparams = types::define_tparams(cx, env, &tydef.tparams);
                cx.ty_aliases.insert(
                    def_id,
                    TyAlias { tyexpr: Some(Rc::clone(&alias.ty)), ty: None, tparams },
                );
            }
        }
    });
}

fn define_variants(
    cx: &mut Typeck<'_>,
    union_def: &ast::UnionTyDef,
    adt_id: AdtId,
) -> Vec<VariantId> {
    let mut variants = vec![];
    let mut defined_variants = WordMap::default();

    for (index, variant) in union_def.variants.iter().enumerate() {
        if let Some(prev_span) = defined_variants.insert(variant.name) {
            cx.db.diagnostics.add(errors::name_defined_twice("variant", variant.name, prev_span));
        }

        let unknown = cx.db.types.unknown;
        let id = cx.db.variants.push_with_key(|id| Variant {
            id,
            adt_id,
            index,
            name: variant.name,
            fields: vec![],
            ctor_ty: unknown,
        });

        variants.push(id);
    }

    // TODO: We store the union tag in a u8, so the variant count must be under 255.
    // What we should be doing is make the tag type accommodate the variant count.
    if variants.len() > 255 {
        cx.db.diagnostics.add(
            Diagnostic::error("cannot define more than 255 variants")
                .with_label(Label::primary(cx.db[adt_id].name.span(), "exceeds variant limit"))
                .with_note("this limitation will be lifted in the future"),
        )
    }

    variants
}

fn check_adt_tparams(cx: &mut Typeck, env: &mut Env, tydef: &ast::TyDef, adt_id: AdtId) {
    let tparams = types::define_tparams(cx, env, &tydef.tparams);
    cx.db[adt_id].tparams = tparams;
    let adt = &cx.db[adt_id];
    cx.def_to_ty.insert(adt.def_id, TyKind::Type(adt.ty()).into());
}

pub(crate) fn check_sigs(cx: &mut Typeck, ast: &Ast) {
    for (module, item, id) in ast.items_with_id() {
        let result = match item {
            ast::Item::Let(let_) => check_let_item(cx, module.id, id, let_),
            ast::Item::ExternLet(let_) => check_extern_let(cx, module.id, id, let_),
            ast::Item::Fn(fun) => check_fn(cx, module.id, id, fun),
            ast::Item::Assoc(tyname, item) => check_assoc_item(cx, module.id, id, *tyname, item),
            _ => Ok(()),
        };

        if let Err(diagnostic) = result {
            cx.db.diagnostics.add(diagnostic);
        }
    }
}

pub(crate) fn check_let_item(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::Let,
) -> DiagnosticResult<()> {
    let env = Env::new(module_id);

    debug_assert!(let_.ty_expr.is_some());
    let ty = tyexpr::check_optional(cx, &env, let_.ty_expr.as_ref(), AllowTyHole::No)?;

    let pat = cx.res_map.item_to_pat.get(&item_id).cloned().expect("to be defined");
    let typed_pat = map_typed_pat(cx, pat, ty);
    cx.res_map.item_to_pat.insert(item_id, typed_pat);
    cx.res_map.item_to_ty.insert(item_id, ty);

    Ok(())
}

fn map_typed_pat(cx: &mut Typeck<'_>, pat: Pat, ty: Ty) -> Pat {
    match pat {
        Pat::Name(name) => {
            debug_assert!(!name.id.is_null());
            cx.def_to_ty.insert(name.id, ty);
            Pat::Name(NamePat { ty, ..name })
        }
        Pat::Discard(span) => Pat::Discard(span),
    }
}

fn check_extern_let(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    let_: &ast::ExternLet,
) -> DiagnosticResult<()> {
    let env = Env::new(module_id);
    let id = cx.res_map.item_to_def.remove(&item_id).expect("to be defined");
    let ty = tyexpr::check(cx, &env, &let_.ty_expr, AllowTyHole::No)?;
    cx.def_to_ty.insert(id, ty);
    cx.hir.extern_lets.push(hir::ExternLet { module_id, id, word: let_.word, ty, span: let_.span });
    Ok(())
}

fn check_fn(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
) -> DiagnosticResult<()> {
    let &id = cx.res_map.item_to_def.get(&item_id).expect("to be defined");
    check_fn_helper(cx, module_id, item_id, id, fun, None)
}

fn check_assoc_fn(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
    assoc_ty: AssocTy,
) -> DiagnosticResult<()> {
    check_assoc_name_overlap(cx, assoc_ty, fun.sig.word)?;
    let id = define_fn(cx, module_id, item_id, fun, Some(assoc_ty))?;
    check_fn_helper(cx, module_id, item_id, id, fun, Some(assoc_ty))
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
            check_builtin_fn(cx, module_id, fun, &sig, id)?;
        }
    }

    if assoc_ty.is_none() && matches!(&fun.kind, ast::FnKind::Bare { .. }) {
        hooks::check(cx, &sig, id)?;
    }

    cx.def_to_ty.insert(id, sig.ty);
    cx.res_map.item_to_sig.insert(item_id, sig);

    Ok(())
}

fn check_builtin_fn(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    fun: &ast::Fn,
    sig: &hir::FnSig,
    id: DefId,
) -> DiagnosticResult<()> {
    let fnty = sig.ty.as_fn().unwrap();

    if let Some(attr) = fun.attrs.find(ast::AttrId::Builtin) {
        let ast::AttrArgs::Builtin(name) = attr.args else { unreachable!() };

        let builtin = Builtin::try_from(name.as_str()).map_err(|()| {
            Diagnostic::error(format!("unknown builtin `{name}`"))
                .with_label(Label::primary(name.span(), "unknown builtin"))
        })?;

        if fnty.callconv != CallConv::Jin {
            return Err(Diagnostic::error("builtin calling convention must be \"jin\"")
                .with_label(Label::primary(fun.sig.word.span(), "invalid calling convention")));
        }

        if !ns::in_std(cx.db, module_id) {
            return Err(Diagnostic::error("builtin cannot be defined outside the `std` package")
                .with_label(Label::primary(
                    fun.sig.word.span(),
                    "cannot be defined outside `std`",
                )));
        }

        cx.db.builtins.insert(id, builtin);
    } else if fnty.is_extern() && !sig.tparams.is_empty() {
        return Err(Diagnostic::error("type parameters are not allowed on extern functions")
            .with_label(Label::primary(sig.word.span(), "type parameters not allowed")));
    }

    Ok(())
}

fn check_assoc_item(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tyname: Word,
    item: &ast::Item,
) -> DiagnosticResult<()> {
    let assoc_ty = check_assoc_item_ty(cx, module_id, tyname)?;

    match item {
        ast::Item::Fn(fun) => check_assoc_fn(cx, module_id, item_id, fun, assoc_ty),
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

    let Some(assoc_ty) = types::try_extract_assoc_ty(cx, id) else {
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

pub(crate) fn check_bodies(cx: &mut Typeck<'_>, ast: &Ast) {
    for (module, item, id) in ast.items_with_id() {
        if let Err(diagnostic) = check_item_body(cx, module.id, item, id) {
            cx.db.diagnostics.add(diagnostic);
        }
    }
}

fn check_item_body(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item: &ast::Item,
    id: ast::GlobalItemId,
) -> DiagnosticResult<()> {
    match item {
        ast::Item::Let(let_) => {
            let (pat, value, ty) = check_let_item_body(cx, module_id, let_, id)?;
            cx.hir.lets.push_with_key(|id| hir::Let {
                id,
                module_id,
                kind: helpers::trans_let_kind(&let_.kind),
                pat,
                value: Box::new(value),
                ty,
                span: let_.span,
            });
            Ok(())
        }
        ast::Item::Fn(fun) => check_fn_item_body(cx, id, fun),
        ast::Item::Assoc(_, item) => match item.as_ref() {
            ast::Item::Fn(fun) => check_fn_item_body(cx, id, fun),
            ast::Item::Let(_)
            | ast::Item::Type(_)
            | ast::Item::Import(_)
            | ast::Item::ExternLet(_)
            | ast::Item::ExternImport(_)
            | ast::Item::Assoc(_, _) => unreachable!(),
        },
        _ => Ok(()),
    }
}

fn check_let_item_body(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    let_: &ast::Let,
    id: ast::GlobalItemId,
) -> DiagnosticResult<(Pat, hir::Expr, Ty)> {
    let mut env = Env::new(module_id);
    let pat = cx.res_map.item_to_pat.remove(&id).expect("to be defined");
    let ty = cx.res_map.item_to_ty.remove(&id).expect("to be defined");
    let value = check_let_body(cx, &mut env, ty, let_)?;
    Ok((pat, value, ty))
}

pub(crate) fn check_let_body(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    ty: Ty,
    let_: &ast::Let,
) -> DiagnosticResult<hir::Expr> {
    let value = exprs::check_expr(cx, env, &let_.value, Some(ty))?;
    cx.eq_obvious_expr(ty, &value)?;

    if cx.normalize(ty).is_module() {
        return Err(Diagnostic::error("cannot store a module as a value")
            .with_label(Label::primary(value.span, "expected a value")));
    }

    let value = if env.in_global_scope() {
        // We do this so that global variable initialization always
        // includes a block (required for drops)
        cx.expr_or_block(value)
    } else {
        value
    };

    Ok(value)
}

pub(crate) fn check_fn_item_body(
    cx: &mut Typeck<'_>,
    item_id: ast::GlobalItemId,
    fun: &ast::Fn,
) -> DiagnosticResult<()> {
    let def_id = cx.res_map.item_to_def.remove(&item_id).expect("to be defined");
    let sig = cx.res_map.item_to_sig.remove(&item_id).expect("to be defined");

    let mut fun = match &fun.kind {
        ast::FnKind::Bare { body } => fns::check_fn_body(cx, sig, def_id, body, fun.span)?,
        ast::FnKind::Extern { is_c_variadic, .. } => hir::Fn {
            id: hir::FnId::null(),
            module_id: cx.db[def_id].scope.module_id,
            def_id,
            sig,
            kind: hir::FnKind::Extern { is_c_variadic: *is_c_variadic },
            span: fun.span,
        },
    };

    cx.hir.fns.push_with_key(|id| {
        fun.id = id;
        fun
    });

    Ok(())
}
