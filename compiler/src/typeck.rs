mod attrs;
mod coerce;
mod collect;
mod env;
mod errors;
mod late;
mod normalize;
mod pmatch;
mod resolution_state;
mod subst;
mod unify;

use std::cell::RefCell;

use data_structures::index_vec::{IndexVecExt, Key};
use ena::unify::{InPlace, InPlaceUnificationTable, Snapshot};
use itertools::{Itertools, Position};
use ustr::UstrMap;

use crate::{
    ast::{self, Ast},
    counter::Counter,
    db::{
        Adt, AdtField, AdtId, AdtKind, Db, DefId, DefKind, ModuleId, StructDef,
        UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{ExprId, FnParam, Hir},
    macros::create_bool_enum,
    middle::{
        BinOp, CmpOp, IsUfcs, Mutability, Pat, TyExpr, TyParam, UnOp, Vis,
    },
    span::{Span, Spanned},
    sym,
    ty::{
        FloatVar, FnTy, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty,
        TyKind, TyVar,
    },
    typeck::{
        attrs::AttrsPlacement,
        coerce::CoerceExt,
        env::{
            BuiltinTys, Env, FnQuery, GlobalScope, LookupResult, PathLookup,
            Query, ScopeKind, Symbol, TyLookup,
        },
        resolution_state::{ModuleStatus, ResolutionState, ResolvedFnSig},
        unify::Obligation,
    },
    word::{Word, WordMap},
};

pub type TypeckResult<T> = Result<T, Diagnostic>;

pub fn typeck(db: &mut Db, ast: &Ast) -> TypeckResult<Hir> {
    Typeck::new(db, ast).run()
}

pub struct Typeck<'db> {
    db: &'db mut Db,
    ast: &'db Ast,
    hir: Hir,
    global_scope: GlobalScope,
    builtin_tys: BuiltinTys,
    resolution_state: ResolutionState,
    storage: RefCell<TyStorage>,
    expr_id: Counter<ExprId>,
    checking_items: bool,
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty: InPlaceUnificationTable<TyVar>,
    pub int: InPlaceUnificationTable<IntVar>,
    pub float: InPlaceUnificationTable<FloatVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty: InPlaceUnificationTable::new(),
            int: InPlaceUnificationTable::new(),
            float: InPlaceUnificationTable::new(),
        }
    }

    pub fn snapshot(&mut self) -> TyStorageSnapshot {
        TyStorageSnapshot {
            ty: self.ty.snapshot(),
            int: self.int.snapshot(),
            float: self.float.snapshot(),
        }
    }

    pub fn rollback_to(&mut self, to: TyStorageSnapshot) {
        self.ty.rollback_to(to.ty);
        self.int.rollback_to(to.int);
        self.float.rollback_to(to.float);
    }
}

pub struct TyStorageSnapshot {
    ty: Snapshot<InPlace<TyVar>>,
    int: Snapshot<InPlace<IntVar>>,
    float: Snapshot<InPlace<FloatVar>>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db, ast: &'db Ast) -> Self {
        Self {
            ast,
            hir: Hir::new(),
            global_scope: GlobalScope::new(),
            builtin_tys: BuiltinTys::new(db),
            db,
            resolution_state: ResolutionState::new(),
            storage: RefCell::new(TyStorage::new()),
            expr_id: Counter::new(),
            checking_items: true,
        }
    }

    fn run(mut self) -> TypeckResult<Hir> {
        self.create_module_states();
        self.collect_items();
        self.check_items()?;
        self.checking_items = false;
        self.check_fn_bodies()?;

        self.subst();

        late::check_bodies(self.db, &self.hir);
        late::leaky_items(self.db, &self.hir);
        late::check_main(self.db, &self.hir);

        Ok(self.hir)
    }

    fn create_module_states(&mut self) {
        for module in &self.ast.modules {
            self.resolution_state.create_module_state(module.id);
        }
    }

    fn check_items(&mut self) -> TypeckResult<()> {
        for module in &self.ast.modules {
            self.check_module(module)?;
        }

        Ok(())
    }

    fn check_module(&mut self, module: &ast::Module) -> TypeckResult<()> {
        if self.resolution_state.module_state(module.id).status.is_resolved() {
            return Ok(());
        }

        self.resolution_state.module_state_mut(module.id).status =
            ModuleStatus::InProgress;

        let mut env = Env::new(module.id);

        for (item_id, item) in module.items.iter_enumerated() {
            let item_id = ast::GlobalItemId::new(module.id, item_id);
            if self.resolution_state.get_item_status(&item_id).is_unresolved() {
                self.check_item(&mut env, item, item_id)?;
            }
        }

        self.resolution_state.module_state_mut(module.id).status =
            ModuleStatus::Resolved;

        Ok(())
    }

    fn check_fn_bodies(&mut self) -> TypeckResult<()> {
        for module in &self.ast.modules {
            let mut env = Env::new(module.id);

            for (item_id, item) in module.items.iter_enumerated() {
                if let ast::Item::Fn(fun) = item {
                    let ResolvedFnSig { id, sig } = self
                        .resolution_state
                        .take_resolved_fn_sig(ast::GlobalItemId::new(
                            module.id, item_id,
                        ))
                        .unwrap_or_else(|| {
                            panic!(
                                "fn `{}` in module `{}` to be resolved",
                                fun.sig.word, self.db[module.id].qpath
                            )
                        });

                    let mut f = self.check_fn_body(&mut env, fun, sig, id)?;
                    self.hir.fns.push_with_key(|id| {
                        f.id = id;
                        f
                    });
                }
            }
        }

        Ok(())
    }

    fn check_item(
        &mut self,
        env: &mut Env,
        item: &ast::Item,
        item_id: ast::GlobalItemId,
    ) -> TypeckResult<()> {
        if self.resolution_state.get_item_status(&item_id).is_resolved() {
            return Ok(());
        }

        self.resolution_state.mark_in_progress_item(item_id).map_err(
            |err| {
                let origin_span = item.span();
                let reference_span = self
                    .ast
                    .find_item(err.causee)
                    .expect("item to exist")
                    .span();

                Diagnostic::error()
                    .with_message("cycle detected while checking definition")
                    .with_label(
                        Label::primary(origin_span)
                            .with_message("definition here"),
                    )
                    .with_label(
                        Label::secondary(reference_span)
                            .with_message("cyclic reference here"),
                    )
            },
        )?;

        match item {
            ast::Item::Fn(fun) => {
                self.check_fn_item(env, fun, item_id)?;
                // Fn will be added to Hir after `check_fn_bodies`
            }
            ast::Item::Let(let_) => {
                let mut let_ = self.check_let(env, let_)?;
                self.hir.lets.push_with_key(|id| {
                    let_.id = id;
                    let_
                });
            }
            ast::Item::Type(tydef) => {
                self.check_tydef(env, tydef)?;
            }
            ast::Item::Import(import) => {
                self.check_import(env, import)?;
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.check_extern_let(env, let_)?;
                self.hir.extern_lets.push(let_);
            }
            ast::Item::ExternImport(import) => {
                self.db.extern_libs.insert(import.lib.clone());
            }
        }

        self.resolution_state.mark_resolved_item(item_id);

        Ok(())
    }

    fn check_fn_body(
        &mut self,
        env: &mut Env,
        fun: &ast::Fn,
        mut sig: hir::FnSig,
        id: DefId,
    ) -> TypeckResult<hir::Fn> {
        let kind = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::Fn(id),
            |env| -> TypeckResult<_> {
                for tp in &sig.ty_params {
                    env.insert(tp.word.name(), tp.id);
                }

                for p in &mut sig.params {
                    self.insert_pat(env, &p.pat)?;
                }

                match &fun.kind {
                    ast::FnKind::Bare { body } => {
                        let ret_ty = sig.ty.as_fn().unwrap().ret;

                        let body = self.check_expr(env, body, Some(ret_ty))?;

                        self.at(Obligation::return_ty(
                            body.span,
                            fun.sig
                                .ret
                                .as_ref()
                                .map_or(self.db[id].span, Spanned::span),
                        ))
                        .eq(ret_ty, body.ty)
                        .or_coerce(self, body.id)?;

                        Ok(hir::FnKind::Bare { body: self.expr_or_block(body) })
                    }
                    ast::FnKind::Extern { is_c_variadic } => {
                        Ok(hir::FnKind::Extern {
                            is_c_variadic: *is_c_variadic,
                        })
                    }
                }
            },
        )?;

        Ok(hir::Fn {
            id: hir::FnId::null(),
            module_id: env.module_id(),
            def_id: id,
            sig,
            kind,
            span: fun.span,
        })
    }

    fn check_fn_item(
        &mut self,
        env: &mut Env,
        fun: &ast::Fn,
        item_id: ast::GlobalItemId,
    ) -> TypeckResult<()> {
        self.check_attrs(
            env.module_id(),
            &fun.attrs,
            match &fun.kind {
                ast::FnKind::Bare { .. } => AttrsPlacement::Fn,
                ast::FnKind::Extern { .. } => AttrsPlacement::ExternFn,
            },
        )?;

        let is_c_variadic = match &fun.kind {
            ast::FnKind::Bare { .. } => false,
            ast::FnKind::Extern { is_c_variadic } => *is_c_variadic,
        };

        let sig = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::Fn(DefId::null()),
            |env| self.check_fn_sig(env, &fun.sig, is_c_variadic),
        )?;

        let id = self.define_fn(env.module_id(), fun, &sig)?;

        self.resolution_state
            .insert_resolved_fn_sig(item_id, ResolvedFnSig { id, sig });

        Ok(())
    }

    fn check_fn_sig(
        &mut self,
        env: &mut Env,
        sig: &ast::FnSig,
        is_c_variadic: bool,
    ) -> TypeckResult<hir::FnSig> {
        let ty_params = self.check_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = WordMap::default();

        for p in &sig.params {
            let ty = self.check_ty_expr(env, &p.ty_expr, AllowTyHole::No)?;
            let pat = self.define_pat(env, DefKind::Variable, &p.pat, ty)?;

            match &pat {
                Pat::Name(name) => {
                    if let Some(prev_span) = defined_params.insert(name.word) {
                        let name = name.word;
                        let dup_span = name.span();

                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "the name `{name}` is already used as a \
                                 parameter name"
                            ))
                            .with_label(Label::primary(dup_span).with_message(
                                format!("`{name}` used again here"),
                            ))
                            .with_label(
                                Label::secondary(prev_span).with_message(
                                    format!("first use of `{name}`"),
                                ),
                            ));
                    }
                }
                Pat::Discard(_) => (),
            }

            params.push(hir::FnParam { pat, ty });
        }

        let (ret, ret_span) = if let Some(ret) = &sig.ret {
            (self.check_ty_expr(env, ret, AllowTyHole::No)?, ret.span())
        } else {
            (self.fresh_ty_var(), sig.word.span())
        };

        let ty = Ty::new(TyKind::Fn(FnTy {
            params: params
                .iter()
                .map(|p: &FnParam| FnTyParam { name: p.pat.name(), ty: p.ty })
                .collect(),
            ret,
            is_c_variadic,
        }));

        Ok(hir::FnSig { word: sig.word, ty_params, params, ret, ret_span, ty })
    }

    fn check_let(
        &mut self,
        env: &mut Env,
        let_: &ast::Let,
    ) -> TypeckResult<hir::Let> {
        self.check_attrs(env.module_id(), &let_.attrs, AttrsPlacement::Let)?;

        let ty = if let Some(ty_expr) = &let_.ty_expr {
            self.check_ty_expr(env, ty_expr, AllowTyHole::Yes)?
        } else {
            self.fresh_ty_var()
        };

        let value = self.check_expr(env, &let_.value, Some(ty))?;

        self.at(Obligation::obvious(value.span))
            .eq(ty, value.ty)
            .or_coerce(self, value.id)?;

        if self.normalize(ty).is_module() {
            return Err(Diagnostic::error()
                .with_message("cannot store a module as a value")
                .with_label(
                    Label::primary(value.span).with_message("expected a value"),
                ));
        }

        let def_kind = if env.in_global_scope() {
            DefKind::Global
        } else {
            DefKind::Variable
        };

        let pat = self.define_pat(env, def_kind, &let_.pat, ty)?;

        let value = if env.in_global_scope() {
            // We do this so that global variable initialization always includes a block (required for
            // destroys)
            self.expr_or_block(value)
        } else {
            value
        };

        Ok(hir::Let {
            id: hir::LetId::null(),
            module_id: env.module_id(),
            pat,
            value: Box::new(value),
            ty,
            span: let_.span,
        })
    }

    fn check_tydef(
        &mut self,
        env: &mut Env,
        tydef: &ast::TyDef,
    ) -> TypeckResult<()> {
        self.check_attrs(
            env.module_id(),
            &tydef.attrs,
            AttrsPlacement::ExternLet,
        )?;

        match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => {
                self.check_tydef_struct(env, tydef, struct_def)
            }
            ast::TyDefKind::Union(union_def) => {
                self.check_tydef_union(env, tydef, union_def)
            }
        }
    }

    fn check_tydef_struct(
        &mut self,
        env: &mut Env,
        tydef: &ast::TyDef,
        struct_def: &ast::StructTyDef,
    ) -> TypeckResult<()> {
        let mut fields = vec![];
        let mut defined_fields = WordMap::default();

        for field in &struct_def.fields {
            if let Some(prev_span) = defined_fields.insert(field.name) {
                let name = field.name.name();
                let dup_span = field.name.span();

                return Err(Diagnostic::error()
                    .with_message(format!(
                        "the name `{name}` is already used as a field name"
                    ))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("first use of `{name}`")),
                    ));
            }

            fields.push(AdtField {
                name: field.name,
                vis: field.vis,
                ty: self.db.types.unknown,
            });
        }

        let unknown = self.db.types.unknown;
        let (adt_id, def_id) = self.define_adt(env, tydef, |id| {
            AdtKind::Struct(StructDef::new(
                id,
                fields,
                struct_def.kind,
                unknown, // Will be filled later
            ))
        })?;

        env.with_anon_scope(ScopeKind::TyDef, |env| -> TypeckResult<()> {
            self.check_adt_ty_params(env, tydef, adt_id, def_id)?;

            for (idx, field) in struct_def.fields.iter().enumerate() {
                let ty =
                    self.check_ty_expr(env, &field.ty_expr, AllowTyHole::No)?;
                self.db[adt_id].as_struct_mut().unwrap().fields[idx].ty = ty;
            }

            Ok(())
        })?;

        let adt_ty = self.db[adt_id].ty();
        self.db[adt_id].as_struct_mut().unwrap().fill_ctor_ty(adt_ty);

        let adt = &self.db[adt_id];

        if let Some(field) = adt.is_infinitely_sized() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "type `{}` is infinitely sized",
                    adt.name
                ))
                .with_label(
                    Label::primary(adt.name.span())
                        .with_message("defined here"),
                )
                .with_label(
                    Label::secondary(field.name.span()).with_message(format!(
                        "field has type `{}` without indirection",
                        adt.name
                    )),
                ));
        }

        Ok(())
    }

    fn check_tydef_union(
        &mut self,
        env: &mut Env,
        tydef: &ast::TyDef,
        union_def: &ast::UnionTyDef,
    ) -> TypeckResult<()> {
        let mut variants: Vec<VariantId> = vec![];

        for (idx, variant) in union_def.variants.iter().enumerate() {
            variants.push(self.check_tydef_union_variant(variant, idx)?);
        }

        let (adt_id, def_id) = self.define_adt(env, tydef, |id| {
            AdtKind::Union(UnionDef::new(id, variants.clone()))
        })?;

        env.with_anon_scope(ScopeKind::TyDef, |env| -> TypeckResult<()> {
            self.check_adt_ty_params(env, tydef, adt_id, def_id)?;

            let adt_ty = self.db[adt_id].ty();

            for (&variant_id, variant) in
                variants.iter().zip(&union_def.variants)
            {
                for (field_idx, field) in variant.fields.iter().enumerate() {
                    let ty = self.check_ty_expr(
                        env,
                        &field.ty_expr,
                        AllowTyHole::No,
                    )?;
                    self.db[variant_id].fields[field_idx].ty = ty;
                }

                self.db[variant_id].fill_ctor_ty(adt_ty);
                self.db[variant_id].adt_id = adt_id;
            }

            Ok(())
        })?;

        let adt = &self.db[adt_id];

        if let Some(field) = adt.is_infinitely_sized() {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "type `{}` is infinitely sized",
                    adt.name
                ))
                .with_label(
                    Label::primary(adt.name.span())
                        .with_message("defined here"),
                )
                .with_label(
                    Label::secondary(field.name.span()).with_message(format!(
                        "field has type `{}` without indirection",
                        adt.name
                    )),
                ));
        }

        Ok(())
    }

    fn check_tydef_union_variant(
        &mut self,
        variant: &ast::UnionVariant,
        index: usize,
    ) -> TypeckResult<VariantId> {
        let mut fields = vec![];
        let mut defined_fields = WordMap::default();

        for field in &variant.fields {
            if let Some(prev_span) = defined_fields.insert(field.name) {
                let name = field.name.name();
                let dup_span = field.name.span();

                return Err(Diagnostic::error()
                    .with_message(format!(
                        "the name `{name}` is already used as a field name"
                    ))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("first use of `{name}`")),
                    ));
            }

            fields.push(AdtField {
                name: field.name,
                vis: Vis::Public,
                ty: self.db.types.unknown,
            });
        }

        let unknown = self.db.types.unknown;
        let id = self.db.variants.push_with_key(|id| Variant {
            id,
            adt_id: AdtId::null(),
            index,
            name: variant.name,
            fields,
            ctor_ty: unknown,
        });

        Ok(id)
    }

    fn define_adt(
        &mut self,
        env: &mut Env,
        tydef: &ast::TyDef,
        kind: impl FnOnce(AdtId) -> AdtKind,
    ) -> TypeckResult<(AdtId, DefId)> {
        let adt_id = self.db.adts.push_with_key(|id| Adt {
            id,
            def_id: DefId::null(),
            name: tydef.word,
            ty_params: vec![],
            kind: kind(id),
        });

        let def_id = self.define_def(
            env,
            tydef.vis,
            DefKind::Adt(adt_id),
            tydef.word,
            Mutability::Imm,
            self.db.types.unknown, // Will be filled later
        )?;
        self.db[adt_id].def_id = def_id;

        Ok((adt_id, def_id))
    }

    fn check_adt_ty_params(
        &mut self,
        env: &mut Env,
        tydef: &ast::TyDef,
        adt_id: AdtId,
        def_id: DefId,
    ) -> TypeckResult<()> {
        let ty_params = self.check_ty_params(env, &tydef.ty_params)?;
        self.db[adt_id].ty_params = ty_params;
        self.db[def_id].ty = TyKind::Type(self.db[adt_id].ty()).into();
        Ok(())
    }

    fn check_import(
        &mut self,
        env: &mut Env,
        import: &ast::Import,
    ) -> TypeckResult<()> {
        let module_info = self
            .db
            .find_module_by_path(&import.path)
            .expect("import to use an existing module");

        let module_id = module_info.id;
        self.check_import_root(env, module_id, &import.root)?;

        Ok(())
    }

    fn check_import_root(
        &mut self,
        env: &mut Env,
        module_id: ModuleId,
        root: &ast::ImportName,
    ) -> TypeckResult<()> {
        match &root.node {
            Some(node) => {
                self.check_import_node(env, module_id, node)?;
            }
            None => {
                self.define_def(
                    env,
                    root.vis,
                    DefKind::Variable,
                    root.name(),
                    Mutability::Imm,
                    Ty::new(TyKind::Module(module_id)),
                )?;
            }
        }

        Ok(())
    }

    fn check_import_node(
        &mut self,
        env: &mut Env,
        module_id: ModuleId,
        node: &ast::ImportNode,
    ) -> TypeckResult<()> {
        match node {
            ast::ImportNode::Name(name) => {
                self.check_import_name(env, module_id, name)?;
            }
            ast::ImportNode::Group(nodes) => {
                for node in nodes {
                    self.check_import_node(env, module_id, node)?;
                }
            }
            ast::ImportNode::Glob(is_ufcs, _) => {
                self.check_import_glob(env, module_id, *is_ufcs);
            }
        }

        Ok(())
    }

    fn check_import_name(
        &mut self,
        env: &mut Env,
        module_id: ModuleId,
        name: &ast::ImportName,
    ) -> TypeckResult<()> {
        let results =
            self.import_lookup(env.module_id(), module_id, name.word)?;

        match (&name.node, results.len()) {
            (Some(node), 1) => {
                // We are traversing a nested module definition
                let id = results[0].id();
                let module_id = self.is_module_def(id, name.word.span())?;
                self.check_import_node(env, module_id, node)
            }
            (Some(_), _) => {
                // We tried to traverse a nested import, but there are multiple results
                Err(Diagnostic::error()
                    .with_message(format!(
                        "expected a module, but `{}` isn't one",
                        name.word
                    ))
                    .with_label(
                        Label::primary(name.word.span())
                            .with_message("not a module"),
                    ))
            }
            _ => {
                for res in results {
                    match res {
                        LookupResult::Def(id) => {
                            self.insert_def(env, name.name(), id, name.vis)?;
                        }
                        LookupResult::Fn(candidate) => {
                            self.insert_fn_candidate(
                                Symbol::new(
                                    env.module_id(),
                                    name.name().name(),
                                ),
                                candidate,
                            )?;
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn check_import_glob(
        &mut self,
        env: &Env,
        module_id: ModuleId,
        is_ufcs: IsUfcs,
    ) {
        self.resolution_state
            .module_state_mut(env.module_id())
            .globs
            .insert(module_id, is_ufcs);
    }

    fn is_module_def(
        &self,
        def_id: DefId,
        span: Span,
    ) -> TypeckResult<ModuleId> {
        match self.normalize(self.db[def_id].ty).kind() {
            TyKind::Module(module_id) => Ok(*module_id),
            ty => Err(errors::ty_mismatch(
                &TyKind::Module(ModuleId::null()).to_string(self.db),
                &ty.to_string(self.db),
                span,
            )),
        }
    }

    fn check_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> TypeckResult<hir::ExternLet> {
        self.check_attrs(
            env.module_id(),
            &let_.attrs,
            AttrsPlacement::ExternLet,
        )?;

        let ty = self.check_ty_expr(env, &let_.ty_expr, AllowTyHole::No)?;
        let id = self.define_def(
            env,
            let_.vis,
            DefKind::ExternGlobal,
            let_.word,
            let_.mutability,
            ty,
        )?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id,
            word: let_.word,
            span: let_.span,
        })
    }

    #[allow(clippy::too_many_lines)]
    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        expected_ty: Option<Ty>,
    ) -> TypeckResult<hir::Expr> {
        match expr {
            ast::Expr::Let(let_) => {
                let span = let_.span;
                let let_ = self.check_let(env, let_)?;
                Ok(self.expr(
                    hir::ExprKind::Let(let_),
                    self.db.types.unit,
                    span,
                ))
            }
            ast::Expr::Assign { lhs, rhs, op, span } => {
                let lhs = self.check_expr(env, lhs, None)?;
                let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

                if let Some(op) = op {
                    self.check_bin_op(&lhs, &rhs, *op, *span)?;
                } else {
                    self.at(Obligation::exprs(*span, lhs.span, rhs.span))
                        .eq(lhs.ty, rhs.ty)
                        .or_coerce(self, rhs.id)?;
                }

                Ok(self.expr(
                    hir::ExprKind::Assign(hir::Assign {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    self.db.types.unit,
                    *span,
                ))
            }
            ast::Expr::Return { expr, span } => {
                if let Some(fn_id) = env.fn_id() {
                    let ret_ty = self.db[fn_id].ty.as_fn().unwrap().ret;

                    let expr = if let Some(expr) = expr {
                        self.check_expr(env, expr, Some(ret_ty))?
                    } else {
                        self.unit_expr(*span)
                    };

                    self.at(Obligation::return_ty(
                        expr.span,
                        self.db[fn_id].span,
                    ))
                    .eq(ret_ty, expr.ty)
                    .or_coerce(self, expr.id)?;

                    Ok(self.expr(
                        hir::ExprKind::Return(hir::Return {
                            expr: Box::new(expr),
                        }),
                        self.db.types.never,
                        *span,
                    ))
                } else {
                    Err(Diagnostic::error()
                        .with_message("cannot return outside of function scope")
                        .with_label(Label::primary(*span)))
                }
            }
            ast::Expr::If { cond, then, otherwise, span } => self.check_if(
                env,
                cond.as_ref(),
                then.as_ref(),
                otherwise.as_deref(),
                *span,
                expected_ty,
            ),
            ast::Expr::Match { expr, arms, span } => {
                self.check_match(env, expr, arms, *span, expected_ty)
            }
            ast::Expr::Loop { cond, expr, span } => {
                let cond = if let Some(cond) = cond.as_ref() {
                    let cond =
                        self.check_expr(env, cond, Some(self.db.types.bool))?;

                    self.at(Obligation::obvious(cond.span))
                        .eq(self.db.types.bool, cond.ty)
                        .or_coerce(self, cond.id)?;

                    Some(Box::new(cond))
                } else {
                    None
                };

                let expr = env.with_anon_scope(ScopeKind::Loop, |env| {
                    self.check_expr(env, expr, Some(self.db.types.never))
                })?;

                // NOTE: expected & actual types are flipped here so that all types are accepted
                // (since all types coerce to `never`)
                self.at(Obligation::obvious(expr.span))
                    .eq(expr.ty, self.db.types.never)
                    .or_coerce(self, expr.id)?;

                Ok(self.expr(
                    hir::ExprKind::Loop(hir::Loop {
                        cond,
                        expr: Box::new(expr),
                    }),
                    self.db.types.never,
                    *span,
                ))
            }
            ast::Expr::Break { span } => {
                if env.in_scope_kind(&ScopeKind::Loop) {
                    Ok(self.expr(
                        hir::ExprKind::Break,
                        self.db.types.never,
                        *span,
                    ))
                } else {
                    Err(Diagnostic::error()
                        .with_message("cannot break outside of a loop")
                        .with_label(
                            Label::primary(*span)
                                .with_message("break outside of loop"),
                        ))
                }
            }
            ast::Expr::Block { exprs, span } => env.with_anon_scope(
                ScopeKind::Block,
                |env| -> TypeckResult<hir::Expr> {
                    let (exprs, ty) = if exprs.is_empty() {
                        (vec![], self.db.types.unit)
                    } else {
                        let mut new_exprs = vec![];

                        for (pos, expr) in exprs.iter().with_position() {
                            let expected_ty = match pos {
                                Position::Last => expected_ty,
                                _ => Some(self.db.types.unit),
                            };

                            new_exprs.push(self.check_expr(
                                env,
                                expr,
                                expected_ty,
                            )?);
                        }

                        let ty = new_exprs.last().unwrap().ty;

                        (new_exprs, ty)
                    };

                    Ok(self.expr(
                        hir::ExprKind::Block(hir::Block { exprs }),
                        ty,
                        *span,
                    ))
                },
            ),
            ast::Expr::MethodCall { expr, method, ty_args, args, span } => {
                let targs = self.check_optional_ty_args(
                    env,
                    ty_args.as_deref(),
                    AllowTyHole::Yes,
                )?;
                let mut args = self.check_call_args(env, args)?;

                let expr = self.check_expr(env, expr, expected_ty)?;

                let lookup_in_module = match self.normalize(expr.ty).kind() {
                    TyKind::Module(in_module) => *in_module,
                    TyKind::Type(ty) => {
                        let (callee, _) = self
                            .check_name_in_ty(*ty, *method, *span, expr.span)?;
                        return self.check_call(callee, args, *span);
                    }
                    _ => {
                        // This is a UFCS call: add `expr` as the first argument of the call
                        args.insert(
                            0,
                            hir::CallArg { name: None, expr, index: None },
                        );

                        env.module_id()
                    }
                };

                let id = self.lookup_fn_for_call(
                    env,
                    lookup_in_module,
                    *method,
                    targs.as_deref(),
                    &args,
                    IsUfcs::Yes,
                )?;

                let callee =
                    self.check_name(env, id, *method, *span, targs.as_deref())?;

                self.check_call(callee, args, *span)
            }
            ast::Expr::Call { callee, args, span } => {
                let args = self.check_call_args(env, args)?;

                let callee = match callee.as_ref() {
                    ast::Expr::Name { word, targs, span } => {
                        let targs = self.check_optional_ty_args(
                            env,
                            targs.as_deref(),
                            AllowTyHole::Yes,
                        )?;

                        let id = self.lookup_fn_for_call(
                            env,
                            env.module_id(),
                            *word,
                            targs.as_deref(),
                            &args,
                            IsUfcs::No,
                        )?;

                        self.check_name(
                            env,
                            id,
                            *word,
                            *span,
                            targs.as_deref(),
                        )?
                    }
                    _ => self.check_expr(env, callee, None)?,
                };

                self.check_call(callee, args, *span)
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.check_expr(env, expr, None)?;

                let ty = self.normalize(expr.ty);

                match op {
                    UnOp::Neg => {
                        if ty.is_any_int() || ty.is_any_float() {
                            Ok(self.expr(
                                hir::ExprKind::Unary(hir::Unary {
                                    expr: Box::new(expr),
                                    op: *op,
                                }),
                                ty,
                                *span,
                            ))
                        } else {
                            Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(*span).with_message(
                                        format!("invalid use of `{op}`"),
                                    ),
                                ))
                        }
                    }
                    UnOp::Not => {
                        if ty.is_any_int() || ty.is_bool() {
                            Ok(self.expr(
                                hir::ExprKind::Unary(hir::Unary {
                                    expr: Box::new(expr),
                                    op: *op,
                                }),
                                ty,
                                *span,
                            ))
                        } else {
                            Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(*span).with_message(
                                        format!("invalid use of `{op}`"),
                                    ),
                                ))
                        }
                    }
                    UnOp::Ref(mutability) => {
                        self.check_ref(expr, ty, *op, *mutability, *span)
                    }
                }
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.check_expr(env, lhs, None)?;
                let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

                self.check_bin_op(&lhs, &rhs, *op, *span)?;

                let result_ty = match op {
                    BinOp::Cmp(..) => self.db.types.bool,
                    _ => lhs.ty,
                };

                Ok(self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    result_ty,
                    *span,
                ))
            }
            ast::Expr::Cast { expr, ty_expr: ty, span } => {
                let expr = self.check_expr(env, expr, None)?;
                let target = self.check_ty_expr(env, ty, AllowTyHole::Yes)?;

                Ok(self.expr(
                    hir::ExprKind::Cast(hir::Cast {
                        expr: Box::new(expr),
                        target,
                    }),
                    target,
                    *span,
                ))
            }
            ast::Expr::Field { expr, field, span } => {
                let expr = self.check_expr(env, expr, expected_ty)?;
                self.check_field(env, expr, *field, *span)
            }
            ast::Expr::Name { word, targs, span } => {
                let id =
                    self.lookup(env, env.module_id(), &Query::Name(*word))?;
                let targs = self.check_optional_ty_args(
                    env,
                    targs.as_deref(),
                    AllowTyHole::Yes,
                )?;
                self.check_name(env, id, *word, *span, targs.as_deref())
            }
            ast::Expr::Lit { kind, span } => {
                let (kind, ty) = match kind {
                    ast::LitKind::Str(v) => {
                        (hir::Lit::Str(*v), self.db.types.str)
                    }
                    ast::LitKind::Int(v) => {
                        (hir::Lit::Int(*v), self.fresh_int_var())
                    }
                    ast::LitKind::Float(v) => {
                        (hir::Lit::Float(*v), self.fresh_float_var())
                    }
                    ast::LitKind::Bool(v) => {
                        (hir::Lit::Bool(*v), self.db.types.bool)
                    }
                };

                Ok(self.expr(hir::ExprKind::Lit(kind), ty, *span))
            }
        }
    }

    fn check_if(
        &mut self,
        env: &mut Env,
        cond: &ast::Expr,
        then: &ast::Expr,
        otherwise: Option<&ast::Expr>,
        span: Span,
        expected_ty: Option<Ty>,
    ) -> TypeckResult<hir::Expr> {
        let cond = self.check_expr(env, cond, Some(self.db.types.bool))?;

        self.at(Obligation::obvious(cond.span))
            .eq(self.db.types.bool, cond.ty)
            .or_coerce(self, cond.id)?;

        let then = self.check_expr(env, then, expected_ty)?;

        let otherwise = if let Some(otherwise) = otherwise.as_ref() {
            let otherwise = self.check_expr(env, otherwise, Some(then.ty))?;

            self.at(Obligation::exprs(span, then.span, otherwise.span))
                .eq(then.ty, otherwise.ty)
                .or_coerce(self, otherwise.id)?;

            otherwise
        } else {
            self.unit_expr(span)
        };

        let ty = then.ty;
        let cond_span = cond.span;

        Ok(self.expr(
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
        &mut self,
        env: &Env,
        id: DefId,
        word: Word,
        span: Span,
        targs: Option<&[Ty]>,
    ) -> TypeckResult<hir::Expr> {
        if let DefKind::Adt(adt_id) = self.db[id].kind.as_ref() {
            match &self.db[*adt_id].kind {
                AdtKind::Struct(_) => {
                    return self.check_name_struct(
                        env, id, word, span, targs, *adt_id,
                    );
                }
                AdtKind::Union(_) => (),
            }
        }

        let def_ty = self.normalize(self.db[id].ty);
        let (ty, instantiation) =
            self.apply_ty_args_to_ty(env, def_ty, targs, span)?;

        Ok(self.expr(
            hir::ExprKind::Name(hir::Name { id, word, instantiation }),
            ty,
            span,
        ))
    }

    fn check_name_struct(
        &mut self,
        env: &Env,
        id: DefId,
        word: Word,
        span: Span,
        targs: Option<&[Ty]>,
        adt_id: AdtId,
    ) -> TypeckResult<hir::Expr> {
        let adt = &self.db[adt_id];
        let struct_def = adt.as_struct().unwrap();

        // NOTE: if the named definition is a struct, we want to return its
        // constructor function's type
        if struct_def.ctor_vis == Vis::Private
            && self.db[adt.def_id].scope.module_id != env.module_id()
        {
            let private_field = struct_def
                .fields
                .iter()
                .find(|f| f.vis == Vis::Private)
                .expect("to have at least one private field");

            return Err(Diagnostic::error()
                .with_message(format!(
                    "constructor of type `{}` is private because `{}` is \
                     private",
                    adt.name, private_field.name
                ))
                .with_label(
                    Label::primary(span)
                        .with_message("private type constructor"),
                )
                .with_label(
                    Label::secondary(private_field.name.span()).with_message(
                        format!("`{}` is private", private_field.name),
                    ),
                ));
        }

        let (ty, instantiation) =
            self.apply_ty_args_to_ty(env, struct_def.ctor_ty, targs, span)?;

        Ok(self.expr(
            hir::ExprKind::Name(hir::Name { id, word, instantiation }),
            ty,
            span,
        ))
    }

    fn check_optional_ty_args(
        &mut self,
        env: &Env,
        targs: Option<&[TyExpr]>,
        allow_hole: AllowTyHole,
    ) -> TypeckResult<Option<Vec<Ty>>> {
        targs
            .map(|targs| {
                targs
                    .iter()
                    .map(|arg| self.check_ty_expr(env, arg, allow_hole))
                    .try_collect()
            })
            .transpose()
    }

    // Applies type arguments to the given type.
    // Returns (instantiated type, instantiation)
    fn apply_ty_args_to_ty(
        &mut self,
        env: &Env,
        ty: Ty,
        targs: Option<&[Ty]>,
        span: Span,
    ) -> TypeckResult<(Ty, Instantiation)> {
        let mut ty_params = ty.collect_params();

        // NOTE: map type params that are part of the current polymorphic function to themselves, so
        // that we don't instantiate them. that's quite ugly though.
        if let Some(fn_id) = env.fn_id() {
            let fn_ty_params = self.db[fn_id].ty.collect_params();
            for ftp in fn_ty_params {
                if let Some(tp) =
                    ty_params.iter_mut().find(|p| p.var == ftp.var)
                {
                    *tp = ftp.clone();
                }
            }
        }

        let instantiation: Instantiation = match &targs {
            Some(args) if args.len() == ty_params.len() => ty_params
                .into_iter()
                .zip(args.iter())
                .map(|(param, arg)| (param.var, *arg))
                .collect(),
            Some(args) => {
                return Err(errors::ty_arg_mismatch(
                    ty_params.len(),
                    args.len(),
                    span,
                ));
            }
            _ => self.fresh_instantiation(env, ty_params),
        };

        Ok((instantiation.fold(ty), instantiation))
    }

    fn fresh_instantiation(
        &mut self,
        env: &Env,
        ty_params: Vec<ParamTy>,
    ) -> Instantiation {
        let env_fn_ty_params =
            env.fn_id().map_or(vec![], |id| self.db[id].ty.collect_params());

        ty_params
            .into_iter()
            .map(|param| {
                (
                    param.var,
                    // If the type param is one of the current function's type
                    // params, we don't want to instantiate it
                    if env_fn_ty_params.iter().any(|p| p.var == param.var) {
                        Ty::new(TyKind::Param(param))
                    } else {
                        self.fresh_ty_var()
                    },
                )
            })
            .collect()
    }

    fn check_field(
        &mut self,
        env: &Env,
        expr: hir::Expr,
        field: Word,
        span: Span,
    ) -> TypeckResult<hir::Expr> {
        let ty = self.normalize(expr.ty).auto_deref();

        let res_ty = match ty.kind() {
            TyKind::Module(module_id) => {
                let id = self.lookup(env, *module_id, &Query::Name(field))?;
                return self.check_name(env, id, field, span, None);
            }
            TyKind::Adt(adt_id, targs) => {
                let adt = &self.db[*adt_id];

                match &adt.kind {
                    AdtKind::Struct(struct_def) => {
                        if let Some(field) =
                            struct_def.field_by_name(field.name().as_str())
                        {
                            self.check_field_access(env, adt, field, span)?;
                            Some(adt.instantiation(targs).fold(field.ty))
                        } else {
                            None
                        }
                    }
                    AdtKind::Union(_) => None,
                }
            }
            TyKind::Type(ty) => {
                let (expr, can_implicitly_call) =
                    self.check_name_in_ty(*ty, field, span, expr.span)?;

                return if can_implicitly_call {
                    self.check_call(expr, vec![], span)
                } else {
                    Ok(expr)
                };
            }
            TyKind::Str if field.name() == sym::PTR => {
                Some(self.db.types.u8.raw_ptr())
            }
            TyKind::Str if field.name() == sym::LEN => Some(self.db.types.uint),
            _ => None,
        };

        if let Some(res_ty) = res_ty {
            Ok(self.expr(
                hir::ExprKind::Field(hir::Field {
                    expr: Box::new(expr),
                    field,
                }),
                res_ty,
                span,
            ))
        } else {
            Err(errors::field_not_found(self.db, ty, expr.span, field))
        }
    }

    fn check_field_access(
        &self,
        env: &Env,
        adt: &Adt,
        field: &AdtField,
        span: Span,
    ) -> TypeckResult<()> {
        if field.vis == Vis::Private
            && self.db[adt.def_id].scope.module_id != env.module_id()
        {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "field `{}` of type `{}` is private",
                    field.name, adt.name
                ))
                .with_label(
                    Label::primary(span).with_message("private field"),
                ));
        }

        Ok(())
    }

    /// Tries to look up `name` in the namespace of `ty`.
    /// Returns the evaluated expression, and whether it can be implicitly called.
    fn check_name_in_ty(
        &mut self,
        ty: Ty,
        name: Word,
        span: Span,
        ty_span: Span,
    ) -> TypeckResult<(hir::Expr, bool)> {
        match self.lookup_name_in_ty(ty, name, ty_span)? {
            TyLookup::Variant(variant_id) => {
                let TyKind::Adt(adt_id, targs) = ty.kind() else {
                    unreachable!()
                };

                let variant = &self.db[variant_id];
                let adt = &self.db[*adt_id];

                let instantiation = adt.instantiation(targs);
                let ctor_ty = instantiation.fold(variant.ctor_ty);

                let can_implicitly_call = variant.fields.is_empty();
                let expr = self.expr(
                    hir::ExprKind::Variant(hir::Variant {
                        id: variant.id,
                        instantiation,
                    }),
                    ctor_ty,
                    span,
                );

                Ok((expr, can_implicitly_call))
            }
        }
    }

    fn lookup_fn_for_call(
        &mut self,
        env: &Env,
        in_module: ModuleId,
        word: Word,
        ty_args: Option<&[Ty]>,
        args: &[hir::CallArg],
        is_ufcs: IsUfcs,
    ) -> TypeckResult<DefId> {
        let args = args
            .iter()
            .map(|a| FnTyParam {
                name: a.name.map(|w| w.name()),
                ty: self.normalize(a.expr.ty),
            })
            .collect::<Vec<_>>();
        let query = FnQuery::new(word, ty_args, &args, is_ufcs);
        self.lookup(env, in_module, &Query::Fn(query))
    }

    fn check_call(
        &mut self,
        callee: hir::Expr,
        args: Vec<hir::CallArg>,
        span: Span,
    ) -> TypeckResult<hir::Expr> {
        let callee_ty = self.normalize(callee.ty);

        match callee_ty.kind() {
            TyKind::Fn(fn_ty) => self.check_call_fn(callee, args, fn_ty, span),
            TyKind::Type(ty) => {
                if args.len() != 1 {
                    return Err(errors::arg_mismatch(1, args.len(), span));
                }

                let arg = &args[0];

                if let Some(name) = arg.name {
                    return Err(errors::named_param_not_found(name));
                }

                Ok(self.expr(
                    hir::ExprKind::Cast(hir::Cast {
                        expr: Box::new(arg.expr.clone()),
                        target: *ty,
                    }),
                    *ty,
                    span,
                ))
            }
            _ => {
                let ty = self.normalize(callee.ty);
                let span = callee.span;

                Err(Diagnostic::error()
                    .with_message(format!(
                        "expected a function, found `{}`",
                        ty.display(self.db)
                    ))
                    .with_label(
                        Label::primary(span)
                            .with_message("expected a function"),
                    ))
            }
        }
    }

    fn check_call_fn(
        &mut self,
        callee: hir::Expr,
        mut args: Vec<hir::CallArg>,
        fn_ty: &FnTy,
        span: Span,
    ) -> TypeckResult<hir::Expr> {
        #[derive(Debug)]
        struct PassedArg {
            is_named: bool,
            span: Span,
        }

        if !fn_ty.is_c_variadic && args.len() != fn_ty.params.len() {
            return Err(errors::arg_mismatch(
                fn_ty.params.len(),
                args.len(),
                span,
            ));
        }

        let mut already_passed_args = UstrMap::<PassedArg>::default();

        // Resolve positional arg indices
        for (idx, arg) in args.iter_mut().enumerate() {
            if arg.name.is_none() {
                arg.index = Some(idx);

                if let Some(param_name) =
                    fn_ty.params.get(idx).and_then(|p| p.name)
                {
                    already_passed_args.insert(
                        param_name,
                        PassedArg { is_named: false, span: arg.expr.span },
                    );
                }
            }

            let arg_ty = self.normalize(arg.expr.ty);

            if arg_ty.is_type() {
                return Err(errors::generic_expected_found(
                    "a value",
                    &format!("type `{}`", arg_ty.display(self.db)),
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
                    .find_map(
                        |(i, p)| {
                            if p.name == Some(name) {
                                Some(i)
                            } else {
                                None
                            }
                        },
                    )
                    .ok_or_else(|| errors::named_param_not_found(*arg_name))?;

                // Report named arguments that are passed twice
                if let Some(passed_arg) = already_passed_args.insert(
                    arg_name.name(),
                    PassedArg { is_named: true, span: arg_name.span() },
                ) {
                    let name = arg_name.name();
                    let prev = passed_arg.span;
                    let dup = arg_name.span();
                    let is_named = passed_arg.is_named;

                    return Err(Diagnostic::error()
                        .with_message(if is_named {
                            format!(
                                "argument `{name}` is passed multiple times"
                            )
                        } else {
                            format!(
                                "argument `{name}` is already passed \
                                 positionally"
                            )
                        })
                        .with_label(Label::primary(dup).with_message(format!(
                            "`{name}` is passed again here"
                        )))
                        .with_label(Label::secondary(prev).with_message(
                            format!("`{name}` is already passed here"),
                        )));
                }

                arg.index = Some(idx);
            }
        }

        // Unify all args with their corresponding param type
        for arg in &args {
            let idx = arg.index.expect("arg index to be resolved");

            if let Some(param) = fn_ty.params.get(idx) {
                self.at(Obligation::obvious(arg.expr.span))
                    .eq(param.ty, arg.expr.ty)
                    .or_coerce(self, arg.expr.id)?;
            }
        }

        Ok(self.expr(
            hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args }),
            fn_ty.ret,
            span,
        ))
    }

    fn check_ref(
        &mut self,
        expr: hir::Expr,
        ty: Ty,
        op: UnOp,
        mutability: Mutability,
        span: Span,
    ) -> TypeckResult<hir::Expr> {
        if ty.can_create_ref(self.db) {
            Ok(self.expr(
                hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op }),
                ty.create_ref(mutability),
                span,
            ))
        } else {
            Err(Diagnostic::error()
                .with_message(format!(
                    "cannot take a reference to value of type `{}`",
                    ty.display(self.db)
                ))
                .with_label(
                    Label::primary(span).with_message("cannot take reference"),
                ))
        }
    }

    fn check_call_args(
        &mut self,
        env: &mut Env,
        args: &[ast::CallArg],
    ) -> TypeckResult<Vec<hir::CallArg>> {
        let mut new_args = vec![];

        for arg in args {
            new_args.push(match arg {
                ast::CallArg::Named(name, expr) => hir::CallArg {
                    name: Some(*name),
                    expr: self.check_expr(env, expr, None)?,
                    index: None,
                },
                ast::CallArg::Positional(expr) => hir::CallArg {
                    name: None,
                    expr: self.check_expr(env, expr, None)?,
                    index: None,
                },
            });
        }

        Ok(new_args)
    }

    fn check_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &[ast::TyParam],
    ) -> TypeckResult<Vec<TyParam>> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = WordMap::default();

        for tp in ty_params {
            let ty = Ty::new(TyKind::Param(ParamTy {
                name: tp.word.name(),
                var: self.fresh_var(),
            }));

            let id = self.define_local_def(
                env,
                DefKind::Ty(ty),
                tp.word,
                Mutability::Imm,
                TyKind::Type(ty).into(),
            );

            if let Some(prev_span) = defined_ty_params.insert(tp.word) {
                let name = tp.word.name();
                let dup_span = tp.word.span();

                return Err(Diagnostic::error()
                    .with_message(format!(
                        "the name `{name}` is already used as a type \
                         parameter name"
                    ))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("first use of `{name}`")),
                    ));
            }

            new_ty_params.push(TyParam { id, word: tp.word, ty });
        }

        Ok(new_ty_params)
    }

    fn check_ty_expr(
        &mut self,
        env: &Env,
        ty: &TyExpr,
        allow_hole: AllowTyHole,
    ) -> TypeckResult<Ty> {
        match ty {
            TyExpr::Fn(fn_ty) => {
                let params = fn_ty
                    .params
                    .iter()
                    .map(|ty| {
                        self.check_ty_expr(env, ty, allow_hole)
                            .map(|ty| FnTyParam { name: None, ty })
                    })
                    .try_collect()?;

                let ret = self.check_ty_expr(env, &fn_ty.ret, allow_hole)?;

                Ok(Ty::new(TyKind::Fn(FnTy {
                    params,
                    ret,
                    is_c_variadic: fn_ty.is_c_variadic,
                })))
            }
            TyExpr::Ref(inner, mutability, _) => {
                let inner_ty = self.check_ty_expr(env, inner, allow_hole)?;

                match inner_ty.kind() {
                    TyKind::Adt(adt_id, _) if self.db[*adt_id].is_ref() => {
                        Ok(inner_ty.create_ref(*mutability))
                    }
                    TyKind::Param(_) => Ok(inner_ty.create_ref(*mutability)),
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                            "type `{}` cannot be referenced",
                            inner_ty.display(self.db)
                        ))
                        .with_label(
                            Label::primary(inner.span())
                                .with_message("invalid referenced type"),
                        )),
                }
            }
            TyExpr::RawPtr(pointee, _) => {
                let pointee = self.check_ty_expr(env, pointee, allow_hole)?;
                Ok(Ty::new(TyKind::RawPtr(pointee)))
            }
            TyExpr::Path(path, targs, span) => self.check_ty_expr_path(
                env,
                path,
                targs.as_deref(),
                *span,
                allow_hole,
            ),
            TyExpr::Hole(span) => {
                if allow_hole == AllowTyHole::Yes {
                    Ok(self.fresh_ty_var())
                } else {
                    Err(Diagnostic::error()
                        .with_message(
                            "cannot use a _ type in a function's signature",
                        )
                        .with_label(Label::primary(*span)))
                }
            }
        }
    }

    fn check_ty_expr_path(
        &mut self,
        env: &Env,
        path: &[Word],
        targs: Option<&[TyExpr]>,
        span: Span,
        allow_hole: AllowTyHole,
    ) -> TypeckResult<Ty> {
        match self.path_lookup(env, path)? {
            PathLookup::Def(id) => {
                let def = &self.db[id];

                match def.kind.as_ref() {
                    DefKind::Ty(ty) => {
                        if targs.is_some() {
                            Err(Diagnostic::error()
                                .with_message(format!(
                                    "type `{}` doesn't expect any type \
                                     arguments",
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(span).with_message(
                                        "unexpected type arguments",
                                    ),
                                ))
                        } else {
                            Ok(*ty)
                        }
                    }
                    &DefKind::Adt(adt_id) => {
                        let targs = self
                            .check_optional_ty_args(env, targs, allow_hole)?;

                        let ty_params = &self.db[adt_id].ty_params;
                        let targs_len = targs.as_ref().map_or(0, Vec::len);

                        if targs_len == ty_params.len() {
                            Ok(Ty::new(TyKind::Adt(
                                adt_id,
                                targs.unwrap_or_default(),
                            )))
                        } else {
                            Err(errors::adt_ty_arg_mismatch(
                                self.db, adt_id, targs_len, span,
                            ))
                        }
                    }
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                            "expected a type, found value of type `{}`",
                            def.ty.display(self.db)
                        ))
                        .with_label(
                            Label::primary(span)
                                .with_message("expected a type"),
                        )),
                }
            }
            PathLookup::Variant(variant_id) => {
                let variant = &self.db[variant_id];

                Err(Diagnostic::error()
                    .with_message(format!(
                        "expected a type, found variant `{}` of type `{}`",
                        variant.name, self.db[variant.adt_id].name
                    ))
                    .with_label(
                        Label::primary(span)
                            .with_message("expected type, found variant"),
                    ))
            }
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, ty: Ty, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.increment(), kind, ty, span }
    }

    fn expr_or_block(&mut self, expr: hir::Expr) -> hir::Expr {
        if let hir::ExprKind::Block(_) = &expr.kind {
            return expr;
        }

        let ty = expr.ty;
        let span = expr.span;

        self.expr(
            hir::ExprKind::Block(hir::Block { exprs: vec![expr] }),
            ty,
            span,
        )
    }

    fn unit_expr(&mut self, span: Span) -> hir::Expr {
        self.expr(
            hir::ExprKind::Block(hir::Block { exprs: vec![] }),
            self.db.types.unit,
            span,
        )
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Ty(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Int(
            self.storage.borrow_mut().int.new_key(None),
        )))
    }

    #[inline]
    pub fn fresh_float_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Float(
            self.storage.borrow_mut().float.new_key(None),
        )))
    }

    fn check_bin_op(
        &mut self,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
        op: BinOp,
        span: Span,
    ) -> TypeckResult<()> {
        self.at(Obligation::exprs(span, lhs.span, rhs.span))
            .eq(lhs.ty, rhs.ty)
            .or_coerce(self, rhs.id)?;

        match op {
            BinOp::And | BinOp::Or => {
                self.at(Obligation::obvious(lhs.span))
                    .eq(self.db.types.bool, lhs.ty)
                    .or_coerce(self, lhs.id)?;

                self.at(Obligation::obvious(rhs.span))
                    .eq(self.db.types.bool, rhs.ty)
                    .or_coerce(self, rhs.id)?;
            }
            BinOp::Cmp(CmpOp::Eq | CmpOp::Ne) => {
                let ty = self.normalize(lhs.ty);

                if !can_use_eq(ty) {
                    return Err(errors::invalid_bin_op(self.db, op, ty, span));
                }
            }
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::Cmp(_) => {
                let ty = self.normalize(lhs.ty);

                if !ty.is_any_int() && !ty.is_any_float() {
                    return Err(errors::invalid_bin_op(self.db, op, ty, span));
                }
            }
            BinOp::Shl
            | BinOp::Shr
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::BitXor => {
                let ty = self.normalize(lhs.ty);

                if !ty.is_any_int() {
                    return Err(errors::invalid_bin_op(self.db, op, ty, span));
                }
            }
        }

        Ok(())
    }
}

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

create_bool_enum!(AllowTyHole);
