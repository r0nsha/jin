use std::{iter, mem};

use itertools::Itertools;
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast,
    db::{
        AdtId, AdtKind, Db, Def, DefId, DefKind, FnInfo, ModuleId, ScopeInfo,
        ScopeLevel, VariantId,
    },
    diagnostics::{Diagnostic, Label},
    hir,
    macros::create_bool_enum,
    middle::{IsUfcs, Mutability, NamePat, Pat, Vis},
    qpath::QPath,
    span::{Span, Spanned},
    sym,
    ty::{printer::FnTyPrinter, FnTy, FnTyParam, Ty, TyKind},
    typeck::{
        coerce::{Coerce, CoerceOptions},
        errors,
        unify::UnifyOptions,
        Typeck, TypeckResult,
    },
    word::Word,
};

#[derive(Debug, Clone)]
pub enum LookupResult {
    Def(DefId),
    Fn(FnCandidate),
}

impl LookupResult {
    pub fn id(&self) -> DefId {
        match self {
            LookupResult::Def(id)
            | LookupResult::Fn(FnCandidate { id, .. }) => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PathLookup {
    Def(DefId),
    Variant(VariantId),
}

#[derive(Debug, Clone)]
pub enum TyLookup {
    AssocFn(DefId),
    Variant(VariantId),
}

impl<'db> Typeck<'db> {
    pub fn define_global_def(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> TypeckResult<DefId> {
        let qpath = self.db[module_id].qpath.clone().child(name.name());
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let id = Def::alloc(
            self.db,
            qpath,
            scope,
            kind,
            mutability,
            ty,
            name.span(),
        );
        self.insert_global_def(module_id, name, id, vis)
    }

    pub fn insert_global_def(
        &mut self,
        module_id: ModuleId,
        name: Word,
        id: DefId,
        vis: Vis,
    ) -> TypeckResult<DefId> {
        let symbol = Symbol::new(module_id, name.name());

        if let Some(candidates) = self.global_scope.fns.get(&symbol) {
            let last_candidate = candidates.iter().last().unwrap();
            return Err(errors::multiple_item_def_err(
                last_candidate.word.span(),
                name,
            ));
        }

        let new_def = GlobalScopeDef::new(id, vis, name.span());

        if let Some(prev) = self.global_scope.insert_def(symbol, new_def) {
            return Err(errors::multiple_item_def_err(prev.span, name));
        }

        Ok(id)
    }

    pub fn define_local_def(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> DefId {
        let id = Def::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo {
                module_id: env.module_id(),
                level: env.scope_level(),
                vis: Vis::Private,
            },
            kind,
            mutability,
            ty,
            name.span(),
        );

        env.insert(name.name(), id);
        id
    }

    pub fn define_def(
        &mut self,
        env: &mut Env,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> TypeckResult<DefId> {
        if env.in_global_scope() {
            self.define_global_def(
                env.module_id(),
                vis,
                kind,
                name,
                mutability,
                ty,
            )
        } else {
            Ok(self.define_local_def(env, kind, name, mutability, ty))
        }
    }

    pub(super) fn define_fn(
        &mut self,
        module_id: ModuleId,
        fun: &ast::Fn,
        sig: &hir::FnSig,
        assoc_ty: Option<AssocTy>,
    ) -> TypeckResult<DefId> {
        match fun.kind {
            ast::FnKind::Bare { .. } => {
                let symbol = Symbol::new(module_id, sig.word.name());

                let base_qpath = if let Some(assoc_ty) = assoc_ty {
                    match assoc_ty {
                        AssocTy::Adt(adt_id) => {
                            self.db[self.db[adt_id].def_id].qpath.clone()
                        }
                        AssocTy::BuiltinTy(ty) => {
                            QPath::from(ustr(&ty.display(self.db).to_string()))
                        }
                    }
                } else {
                    self.db[module_id].qpath.clone()
                };

                let qpath = base_qpath.child(sig.word.name());

                let scope = ScopeInfo {
                    module_id,
                    level: ScopeLevel::Global,
                    vis: fun.vis,
                };

                if let Some(def) = self.global_scope.defs.get(&symbol) {
                    return Err(errors::multiple_item_def_err(
                        def.span, sig.word,
                    ));
                }

                let id = {
                    Def::alloc(
                        self.db,
                        qpath,
                        scope,
                        DefKind::Fn(FnInfo::Bare),
                        Mutability::Imm,
                        sig.ty,
                        sig.word.span(),
                    )
                };

                let candidate = FnCandidate {
                    id,
                    word: sig.word,
                    ty: sig.ty.as_fn().cloned().unwrap(),
                };

                if let Some(ty) = assoc_ty {
                    self.insert_fn_candidate_in_ty(
                        ty,
                        sig.word.name(),
                        symbol.module_id,
                        candidate,
                    )?;
                } else {
                    self.insert_fn_candidate(symbol, candidate)?;
                }

                Ok(id)
            }
            ast::FnKind::Extern { .. } => self.define_global_def(
                module_id,
                fun.vis,
                DefKind::Fn(FnInfo::Extern),
                sig.word,
                Mutability::Imm,
                sig.ty,
            ),
        }
    }

    pub(super) fn insert_fn_candidate_in_ty(
        &mut self,
        assoc_ty: AssocTy,
        name: Ustr,
        module_id: ModuleId,
        candidate: FnCandidate,
    ) -> TypeckResult<()> {
        let set = self
            .global_scope
            .assoc_scopes
            .entry(assoc_ty)
            .or_default()
            .fns
            .entry(name)
            .or_default();
        Self::insert_fn_candidate_in(self.db, set, module_id, candidate)
    }

    pub fn insert_fn_candidate(
        &mut self,
        symbol: Symbol,
        candidate: FnCandidate,
    ) -> TypeckResult<()> {
        let set = self.global_scope.fns.entry(symbol).or_default();
        Self::insert_fn_candidate_in(self.db, set, symbol.module_id, candidate)
    }

    pub fn insert_fn_candidate_in(
        db: &Db,
        set: &mut FnCandidateSet,
        module_id: ModuleId,
        candidate: FnCandidate,
    ) -> TypeckResult<()> {
        set.try_insert(candidate).map_err(|err| match err {
            FnCandidateInsertError::AlreadyExists { prev, curr } => {
                errors::multiple_fn_def_err(
                    db,
                    module_id,
                    prev.word.span(),
                    &curr,
                )
            }
        })
    }

    pub fn define_pat(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        pat: &Pat,
        ty: Ty,
    ) -> TypeckResult<Pat> {
        match pat {
            Pat::Name(name) => {
                let id = self.define_def(
                    env,
                    name.vis,
                    kind,
                    name.word,
                    name.mutability,
                    ty,
                )?;

                Ok(Pat::Name(NamePat { id, ty, ..name.clone() }))
            }
            Pat::Discard(span) => Ok(Pat::Discard(*span)),
        }
    }

    pub fn insert_pat(&mut self, env: &mut Env, pat: &Pat) -> TypeckResult<()> {
        match pat {
            Pat::Name(name) => {
                self.insert_def(
                    env,
                    name.word,
                    name.id,
                    self.db[name.id].scope.vis,
                )?;
            }
            Pat::Discard(_) => (),
        }

        Ok(())
    }

    pub fn insert_def(
        &mut self,
        env: &mut Env,
        name: Word,
        id: DefId,
        vis: Vis,
    ) -> TypeckResult<()> {
        if env.in_global_scope() {
            self.insert_global_def(env.module_id(), name, id, vis)?;
        } else {
            env.insert(name.name(), id);
        }

        Ok(())
    }

    pub fn path_lookup(
        &mut self,
        env: &Env,
        path: &[Word],
    ) -> TypeckResult<PathLookup> {
        let (&last, path) =
            path.split_last().expect("to have at least one element");

        let mut target_module = env.module_id();

        for (idx, &part) in path.iter().enumerate() {
            let part_id =
                self.lookup(env, target_module, &Query::Name(part))?;
            let part_ty = self.normalize(self.db[part_id].ty);

            match part_ty.kind() {
                TyKind::Module(module_id) => {
                    target_module = *module_id;
                }
                TyKind::Type(ty) => {
                    // The type could be a union, and the next part could be a variant, so we explicitly look it up
                    let next_part = path.get(idx + 1).copied().unwrap_or(last);

                    match self.lookup_in_ty(
                        env.module_id(),
                        *ty,
                        part.span(),
                        &Query::Name(next_part),
                    )? {
                        TyLookup::Variant(variant_id) => {
                            // If there are more parts after this variant, it's an error, since we there
                            // are no symbols under variants
                            if path.get(idx + 2).is_some() {
                                return Err(Diagnostic::error()
                                    .with_message(format!(
                                        "`{}` is a variant, not a module",
                                        self.db[variant_id].name,
                                    ))
                                    .with_label(
                                        Label::primary(next_part.span())
                                            .with_message("not a module"),
                                    ));
                            }

                            return Ok(PathLookup::Variant(variant_id));
                        }
                        TyLookup::AssocFn(id) => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "`{}` is an associated function, not a \
                                     module",
                                    self.db[id].name,
                                ))
                                .with_label(
                                    Label::primary(next_part.span())
                                        .with_message("not a module"),
                                ));
                        }
                    }
                }
                ty => {
                    return Err(errors::expected_module(
                        ty.display(self.db),
                        part.span(),
                    ))
                }
            }
        }

        let id = self.lookup(env, target_module, &Query::Name(last))?;
        Ok(PathLookup::Def(id))
    }

    /// Looks up a `query` in the associated namespace of `ty`.
    pub(super) fn lookup_in_ty(
        &mut self,
        from_module: ModuleId,
        ty: Ty,
        ty_span: Span,
        query: &Query,
    ) -> TypeckResult<TyLookup> {
        if self.checking_items {
            let symbol = Symbol::new(from_module, query.name());
            self.find_and_check_items(&symbol, IsUfcs::No)?;
        }

        if let Query::Fn(fn_query) = query {
            if let Some(id) =
                self.lookup_assoc_fn(from_module, AssocTy::from(ty), fn_query)?
            {
                return Ok(TyLookup::AssocFn(id));
            }
        }

        if let TyKind::Adt(adt_id, _) = ty.kind() {
            self.lookup_name_in_adt(*adt_id, query, ty_span)
        } else {
            Err(errors::assoc_name_not_found(self.db, ty, query))
        }
    }

    fn lookup_assoc_fn(
        &mut self,
        from_module: ModuleId,
        assoc_ty: AssocTy,
        query: &FnQuery,
    ) -> TypeckResult<Option<DefId>> {
        let Some(set) = self
            .global_scope
            .assoc_scopes
            .get(&assoc_ty)
            .and_then(|scope| scope.fns.get(&query.word.name()))
        else {
            return Ok(None);
        };

        let candidates = set.find(self, query);
        self.check_and_filter_fn_candidates(query, candidates, from_module)
    }

    pub(super) fn lookup_name_in_adt(
        &mut self,
        adt_id: AdtId,
        query: &Query,
        ty_span: Span,
    ) -> TypeckResult<TyLookup> {
        let name = query.word();
        let adt = &self.db[adt_id];

        match &adt.kind {
            AdtKind::Union(union_def) => {
                return union_def
                    .variants(self.db)
                    .find(|v| v.name.name() == name.name())
                    .map(|v| TyLookup::Variant(v.id))
                    .ok_or_else(|| {
                        errors::variant_not_found(
                            self.db,
                            adt.ty(),
                            ty_span,
                            name,
                        )
                    });
            }
            AdtKind::Struct(_) => {
                Err(errors::assoc_name_not_found(self.db, adt.ty(), query))
            }
        }
    }

    pub fn import_lookup(
        &mut self,
        from_module: ModuleId,
        in_module: ModuleId,
        word: Word,
    ) -> TypeckResult<Vec<LookupResult>> {
        let symbol = Symbol::new(in_module, word.name());

        if self.checking_items {
            self.find_and_check_items(&symbol, IsUfcs::No)?;
        }

        let results = self.lookup_global_many(
            in_module,
            &symbol,
            ShouldLookupFns::Yes,
            IsUfcs::No,
            AllowBuiltinTys::No,
        );

        if results.is_empty() {
            return Err(errors::name_not_found(
                self.db,
                from_module,
                in_module,
                word,
            ));
        }

        if from_module != in_module {
            for res in &results {
                self.check_def_access(from_module, res.id(), word.span())?;
            }
        }

        Ok(results)
    }

    pub fn lookup(
        &mut self,
        env: &Env,
        in_module: ModuleId,
        query: &Query,
    ) -> TypeckResult<DefId> {
        let id = self.lookup_inner(env, in_module, query)?;
        self.check_def_access(env.module_id(), id, query.span())?;
        Ok(id)
    }

    fn lookup_inner(
        &mut self,
        env: &Env,
        in_module: ModuleId,
        query: &Query,
    ) -> TypeckResult<DefId> {
        let name = query.name();

        if let Some(id) = env.lookup(name).copied() {
            return Ok(id);
        }

        let symbol = Symbol::new(in_module, name);

        if self.checking_items {
            self.find_and_check_items(&symbol, query.is_ufcs())?;
        }

        let from_module = env.module_id();

        if let Query::Fn(fn_query) = query {
            if let Some(id) =
                self.lookup_fn_candidate(from_module, in_module, fn_query)?
            {
                return Ok(id);
            }
        }

        // We should only lookup functions if we didn't already query for a function
        let should_lookup_fns =
            ShouldLookupFns::from(!matches!(query, Query::Fn(_)));

        // Allow looking up builtin types only when looking up a symbol in the same module as its
        // environment's module.
        let allow_builtin_tys =
            AllowBuiltinTys::from(env.module_id() == in_module);

        self.lookup_global_one(
            &symbol,
            query.span(),
            should_lookup_fns,
            allow_builtin_tys,
        )?
        .ok_or_else(|| match query {
            Query::Name(word) => {
                errors::name_not_found(self.db, from_module, in_module, *word)
            }
            Query::Fn(fn_query) => errors::fn_not_found(self.db, fn_query),
        })
    }

    #[inline]
    fn find_and_check_items(
        &mut self,
        symbol: &Symbol,
        is_ufcs: IsUfcs,
    ) -> TypeckResult<()> {
        let lookup_modules = self
            .get_lookup_modules(symbol.module_id, is_ufcs)
            .collect::<Vec<_>>();

        for module_id in lookup_modules {
            if let Some(item_ids) =
                self.global_scope.symbol_to_item.get(symbol).cloned()
            {
                let mut env = Env::new(module_id);

                for item_id in item_ids {
                    let item = &self.ast.modules[module_id].items[item_id];
                    let gid = ast::GlobalItemId::new(module_id, item_id);

                    if self
                        .resolution_state
                        .get_item_status(&gid)
                        .is_unresolved()
                    {
                        self.check_item(&mut env, item, gid)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn lookup_fn_candidate(
        &self,
        from_module: ModuleId,
        in_module: ModuleId,
        query: &FnQuery,
    ) -> TypeckResult<Option<DefId>> {
        let candidates = self
            .get_lookup_modules(in_module, query.is_ufcs)
            .filter_map(|module_id| {
                self.global_scope
                    .fns
                    .get(&Symbol::new(module_id, query.word.name()))
            })
            .flat_map(|set| set.find(self, query))
            .unique_by(|candidate| candidate.id)
            .collect::<Vec<_>>();

        self.check_and_filter_fn_candidates(query, candidates, from_module)
    }

    pub(super) fn check_and_filter_fn_candidates(
        &self,
        query: &FnQuery,
        mut candidates: Vec<&FnCandidate>,
        from_module: ModuleId,
    ) -> TypeckResult<Option<DefId>> {
        if !candidates.is_empty()
            && candidates.iter().all(|c| !self.can_access(from_module, c.id))
        {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "all functions which apply to `{}` are private to their \
                     module",
                    query.display(self.db)
                ))
                .with_label(
                    Label::primary(query.word.span())
                        .with_message("no accessible function found"),
                ));
        }

        // Filter fn candidates by their visibility if there's more than one,
        // otherwise, we want to emit a privacy error for better ux
        candidates.retain(|c| {
            let def = &self.db[c.id];
            def.scope.vis == Vis::Public || from_module == def.scope.module_id
        });

        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.first().unwrap().id)),
            _ => Err(Diagnostic::error()
                .with_message(format!(
                    "ambiguous call to `{}`",
                    query.display(self.db)
                ))
                .with_label(
                    Label::primary(query.word.span()).with_message("call here"),
                )
                .with_note("these functions apply:")
                .with_notes(
                    candidates
                        .into_iter()
                        .map(|c| c.display(self.db).to_string()),
                )),
        }
    }

    pub fn lookup_global_one(
        &mut self,
        symbol: &Symbol,
        span: Span,
        should_lookup_fns: ShouldLookupFns,
        allow_builtin_tys: AllowBuiltinTys,
    ) -> TypeckResult<Option<DefId>> {
        let results = self.lookup_global_many(
            symbol.module_id,
            symbol,
            should_lookup_fns,
            IsUfcs::No,
            allow_builtin_tys,
        );

        match results.len() {
            0 => Ok(None),
            1 => Ok(results.first().map(LookupResult::id)),
            _ => Err(Diagnostic::error()
                .with_message(format!(
                    "ambiguous use of item `{}`",
                    symbol.name
                ))
                .with_label(Label::primary(span).with_message("used here"))
                .with_labels(results.iter().map(|res| {
                    let def = &self.db[res.id()];
                    Label::secondary(def.span)
                        .with_message(format!("`{}` is defined here", def.name))
                }))),
        }
    }

    fn lookup_global_many(
        &self,
        in_module: ModuleId,
        symbol: &Symbol,
        should_lookup_fns: ShouldLookupFns,
        is_ufcs: IsUfcs,
        allow_builtin_tys: AllowBuiltinTys,
    ) -> Vec<LookupResult> {
        let lookup_modules = self.get_lookup_modules(in_module, is_ufcs);
        let mut results = vec![];

        for module_id in lookup_modules {
            let symbol = symbol.with_module_id(module_id);

            if let Some(id) = self.global_scope.get_def(in_module, &symbol) {
                results.push(LookupResult::Def(id));
            } else if should_lookup_fns == ShouldLookupFns::Yes {
                if let Some(candidates) = self.global_scope.fns.get(&symbol) {
                    results.extend(
                        candidates.iter().cloned().map(LookupResult::Fn),
                    );
                }
            }
        }

        if results.is_empty() && allow_builtin_tys == AllowBuiltinTys::Yes {
            return self
                .builtin_tys
                .get(symbol.name)
                .into_iter()
                .map(LookupResult::Def)
                .collect();
        }

        results
    }

    fn get_lookup_modules(
        &self,
        in_module: ModuleId,
        is_ufcs: IsUfcs,
    ) -> impl Iterator<Item = ModuleId> + '_ {
        // When `is_ufcs` is IsUfcs::No, we only include glob imports which are IsUfcs::No
        iter::once(in_module).chain(
            self.resolution_state
                .module_state(in_module)
                .globs
                .iter()
                .filter_map(move |(id, v)| match (is_ufcs, v) {
                    (IsUfcs::Yes, _) | (IsUfcs::No, IsUfcs::No) => Some(id),
                    (IsUfcs::No, IsUfcs::Yes) => None,
                })
                .copied(),
        )
    }

    fn check_def_access(
        &self,
        from_module: ModuleId,
        accessed: DefId,
        span: Span,
    ) -> TypeckResult<()> {
        if self.can_access(from_module, accessed) {
            Ok(())
        } else {
            Err(errors::private_access_violation(self.db, accessed, span))
        }
    }

    fn can_access(&self, from_module: ModuleId, accessed: DefId) -> bool {
        let def = &self.db[accessed];
        def.scope.vis == Vis::Public || from_module == def.scope.module_id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub module_id: ModuleId,
    pub name: Ustr,
}

impl Symbol {
    pub fn new(module_id: ModuleId, name: Ustr) -> Self {
        Self { module_id, name }
    }

    pub fn with_module_id(&self, module_id: ModuleId) -> Self {
        Self { module_id, name: self.name }
    }
}

#[derive(Debug)]
pub struct GlobalScope {
    pub defs: FxHashMap<Symbol, GlobalScopeDef>,
    pub fns: FxHashMap<Symbol, FnCandidateSet>,
    pub assoc_scopes: FxHashMap<AssocTy, AssocScope>,
    pub symbol_to_item: FxHashMap<Symbol, Vec<ast::ItemId>>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            defs: FxHashMap::default(),
            fns: FxHashMap::default(),
            assoc_scopes: FxHashMap::default(),
            symbol_to_item: FxHashMap::default(),
        }
    }

    pub fn get_def(
        &self,
        from_module: ModuleId,
        symbol: &Symbol,
    ) -> Option<DefId> {
        if let Some(def) = self.defs.get(symbol) {
            if def.vis == Vis::Public || from_module == symbol.module_id {
                return Some(def.id);
            }
        }

        None
    }

    fn insert_def(
        &mut self,
        symbol: Symbol,
        def: GlobalScopeDef,
    ) -> Option<GlobalScopeDef> {
        self.defs.insert(symbol, def)
    }
}

#[derive(Debug)]
pub struct GlobalScopeDef {
    pub id: DefId,
    pub vis: Vis,
    pub span: Span,
}

impl GlobalScopeDef {
    fn new(id: DefId, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}

#[derive(Debug)]
pub struct BuiltinTys {
    inner: UstrMap<DefId>,
}

impl BuiltinTys {
    pub fn new(db: &mut Db) -> Self {
        let mut this = Self { inner: UstrMap::default() };

        this.define_all(
            db,
            &[
                (sym::I8, db.types.i8),
                (sym::I16, db.types.i16),
                (sym::I32, db.types.i32),
                (sym::I64, db.types.i64),
                (sym::INT, db.types.int),
                (sym::U8, db.types.u8),
                (sym::U16, db.types.u16),
                (sym::U32, db.types.u32),
                (sym::U64, db.types.u64),
                (sym::UINT, db.types.uint),
                (sym::F32, db.types.f32),
                (sym::F64, db.types.f64),
                (sym::STR, db.types.str),
                (sym::BOOL, db.types.bool),
                (sym::NEVER, db.types.never),
                (sym::UNIT, db.types.unit),
            ],
        );

        this
    }

    fn define_all(&mut self, db: &mut Db, pairs: &[(&str, Ty)]) {
        for (name, ty) in pairs {
            self.define(db, name, *ty);
        }
    }

    fn define(&mut self, db: &mut Db, name: &str, ty: Ty) -> Option<DefId> {
        let name = ustr(name);
        let scope_info = ScopeInfo {
            module_id: db.main_module.unwrap(),
            level: ScopeLevel::Global,
            vis: Vis::Public,
        };

        self.inner.insert(
            name,
            Def::alloc(
                db,
                QPath::from(name),
                scope_info,
                DefKind::Ty(ty),
                Mutability::Imm,
                TyKind::Type(ty).into(),
                Span::unknown(),
            ),
        )
    }

    pub fn get(&self, name: Ustr) -> Option<DefId> {
        self.inner.get(&name).copied()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(super) enum AssocTy {
    Adt(AdtId),
    BuiltinTy(Ty),
}

impl AssocTy {
    pub(super) fn ty(self, db: &Db) -> Ty {
        match self {
            Self::Adt(adt_id) => db[adt_id].ty(),
            Self::BuiltinTy(ty) => ty,
        }
    }
}

impl From<Ty> for AssocTy {
    fn from(value: Ty) -> Self {
        match value.kind() {
            &TyKind::Adt(adt_id, _) => Self::Adt(adt_id),
            _ => Self::BuiltinTy(value),
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct AssocScope {
    pub(super) fns: FxHashMap<Ustr, FnCandidateSet>,
}

#[derive(Debug)]
pub struct Env {
    module_id: ModuleId,
    scopes: Vec<Scope>,
}

impl Env {
    const ANON_SCOPE: &'static str = "_";

    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, scopes: vec![] }
    }

    pub fn with_scope<R>(
        &mut self,
        name: Ustr,
        kind: ScopeKind,
        mut f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.scopes.push(Scope { kind, name, defs: UstrMap::default() });
        let res = f(self);
        self.scopes.pop();
        res
    }

    pub fn with_anon_scope<R>(
        &mut self,
        kind: ScopeKind,
        f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.with_scope(ustr(Self::ANON_SCOPE), kind, f)
    }

    #[allow(unused)]
    pub fn current(&self) -> &Scope {
        self.scopes.last().expect("to have a scope")
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("to have a scope")
    }

    pub fn insert(&mut self, k: Ustr, v: DefId) {
        self.scopes.last_mut().unwrap().defs.insert(k, v);
    }

    pub fn lookup(&self, k: Ustr) -> Option<&DefId> {
        self.lookup_depth(k).map(|r| r.1)
    }

    pub fn lookup_depth(&self, k: Ustr) -> Option<(usize, &DefId)> {
        if self.scopes.is_empty() {
            return None;
        }

        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(value) = scope.defs.get(&k) {
                return Some((depth + 1, value));
            }
        }

        None
    }

    pub fn scope_level(&self) -> ScopeLevel {
        match self.depth() {
            0 => unreachable!("expected to have at least one scope"),
            n => ScopeLevel::Local(n),
        }
    }

    pub fn scope_path(&self, db: &Db) -> QPath {
        let mut qpath = db[self.module_id].qpath.clone();
        qpath.extend(self.scopes.iter().map(|s| s.name));
        qpath
    }

    pub fn fn_id(&self) -> Option<DefId> {
        self.scopes.iter().find_map(|s| match s.kind {
            ScopeKind::Fn(id) => Some(id),
            ScopeKind::Loop | ScopeKind::Block | ScopeKind::TyDef => None,
        })
    }

    pub fn in_scope_kind(&self, kind: &ScopeKind) -> bool {
        let discriminant = mem::discriminant(kind);
        self.scopes.iter().any(|s| mem::discriminant(&s.kind) == discriminant)
    }

    #[inline]
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    #[inline]
    pub fn in_global_scope(&self) -> bool {
        self.depth() == 0
    }

    #[inline]
    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub name: Ustr,
    pub defs: UstrMap<DefId>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ScopeKind {
    Fn(DefId),
    TyDef,
    Loop,
    Block,
}

#[derive(Debug)]
pub struct FnCandidateSet(Vec<FnCandidate>);

impl FnCandidateSet {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn iter(&self) -> std::slice::Iter<'_, FnCandidate> {
        self.0.iter()
    }

    pub fn try_insert(
        &mut self,
        candidate: FnCandidate,
    ) -> Result<(), FnCandidateInsertError> {
        if let Some(prev) = self.0.iter().find(|c| *c == &candidate) {
            return Err(FnCandidateInsertError::AlreadyExists {
                prev: prev.clone(),
                curr: candidate,
            });
        }

        self.0.push(candidate);
        Ok(())
    }

    pub fn find(&self, cx: &Typeck, query: &FnQuery) -> Vec<&FnCandidate> {
        let scores = self.scores(cx, query);

        let Some(&min_score) = scores.iter().map(|(_, s)| s).min() else {
            return vec![];
        };
        scores
            .into_iter()
            .filter_map(|(c, s)| (s == min_score).then_some(c))
            .collect()
    }

    fn scores(&self, cx: &Typeck, query: &FnQuery) -> Vec<(&FnCandidate, u32)> {
        let mut scores = vec![];

        for c in &self.0 {
            if let Some(score) = c.test(cx, query) {
                scores.push((c, score));
            }
        }

        scores
    }
}

impl Default for FnCandidateSet {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum FnCandidateInsertError {
    AlreadyExists { prev: FnCandidate, curr: FnCandidate },
}

#[derive(Debug, Clone)]
pub struct FnCandidate {
    pub id: DefId,
    pub word: Word,
    pub ty: FnTy,
}

impl FnCandidate {
    // Tests the given query against the function candidate, returning
    // a Some(score) if there's a match, or a None if there isn't.
    // See `distance` for how parameter scoring works.
    fn test(&self, cx: &Typeck, query: &FnQuery) -> Option<u32> {
        if self.ty.params.len() != query.args.len() {
            return None;
        }

        // Check that the amount of given type arguments == the amount of type
        // parameters in this candidate
        match query.ty_args {
            Some(ty_args)
                if ty_args.len() != self.ty.collect_params().len() =>
            {
                return None;
            }
            _ => (),
        }

        // Make sure that all passed named arguments exist in this candidate
        if !query.args.iter().all(|arg| {
            arg.name.map_or(true, |name| {
                self.ty.params.iter().any(|p| Some(name) == p.name)
            })
        }) {
            return None;
        }

        let mut total_score = 0;

        for (arg, param) in query.args.iter().zip(self.ty.params.iter()) {
            let score = Self::distance(cx, arg.ty, param.ty)?;
            total_score += score as u32;
        }

        Some(total_score)
    }

    // Calculates the distance between an argument and the parameter it is applied to.
    // The actual distance is calculated by the amount of "steps"
    // required to convert the argument to the parameter.
    fn distance(cx: &Typeck, arg: Ty, param: Ty) -> Option<FnCandidateScore> {
        if arg.can_unify(param, cx, UnifyOptions::default()).is_ok() {
            return Some(FnCandidateScore::Eq);
        }

        if arg.can_coerce(
            &param,
            cx,
            CoerceOptions {
                unify_options: UnifyOptions::default(),
                rollback_unifications: true,
            },
        ) {
            return Some(FnCandidateScore::Coerce);
        }

        if arg.can_coerce(
            &param,
            cx,
            CoerceOptions {
                unify_options: UnifyOptions { unify_param_tys: true },
                rollback_unifications: true,
            },
        ) {
            return Some(FnCandidateScore::Polymorphic);
        }

        None
    }

    pub fn display<'a>(&'a self, db: &'a Db) -> FnTyPrinter {
        self.ty.display(db, Some(db[self.id].name))
    }
}

impl PartialEq for FnCandidate {
    fn eq(&self, other: &Self) -> bool {
        if self.word.name() != other.word.name()
            || self.ty.params.len() != other.ty.params.len()
        {
            return false;
        }

        // Both function parameters are the same, order is insignificant
        if !self.ty.params.iter().all(|p1| {
            if let Some(name) = p1.name {
                other.ty.params.iter().any(|p2| Some(name) == p2.name)
            } else {
                false
            }
        }) {
            return false;
        }

        // Both function parameters are the same, in order
        if self
            .ty
            .params
            .iter()
            .zip(other.ty.params.iter())
            .any(|(p1, p2)| !fn_candidate_tys_eq(p1.ty, p2.ty))
        {
            return false;
        }

        true
    }
}

impl Eq for FnCandidate {}

fn fn_candidate_tys_eq(a: Ty, b: Ty) -> bool {
    match (a.kind(), b.kind()) {
        (TyKind::Ref(a, _), TyKind::Ref(b, _)) => {
            // Consider two references as equal candidates, regardless of their mutability
            a == b
        }
        _ => a == b,
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum FnCandidateScore {
    Eq = 0,
    Coerce = 1,
    Polymorphic = 2,
}

#[derive(Debug, Clone)]
pub enum Query<'a> {
    Name(Word),
    Fn(FnQuery<'a>),
}

impl<'a> Query<'a> {
    #[inline]
    pub fn word(&self) -> Word {
        match self {
            Query::Name(word) | Query::Fn(FnQuery { word, .. }) => *word,
        }
    }

    #[inline]
    pub fn name(&self) -> Ustr {
        self.word().name()
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.word().span()
    }

    pub fn is_ufcs(&self) -> IsUfcs {
        match self {
            Query::Name(_) => IsUfcs::No,
            Query::Fn(FnQuery { is_ufcs, .. }) => *is_ufcs,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnQuery<'a> {
    pub word: Word,
    pub ty_args: Option<&'a [Ty]>,
    pub args: &'a [FnTyParam],
    pub is_ufcs: IsUfcs,
}

impl<'a> FnQuery<'a> {
    pub fn new(
        word: Word,
        ty_args: Option<&'a [Ty]>,
        args: &'a [FnTyParam],
        is_ufcs: IsUfcs,
    ) -> Self {
        Self { word, ty_args, args, is_ufcs }
    }

    pub fn display<'db>(&'db self, db: &'db Db) -> FnTyPrinter {
        FnTyPrinter {
            db,
            name: Some(self.word.name()),
            params: self.args,
            ret: None,
        }
    }
}

create_bool_enum!(ShouldLookupFns);
create_bool_enum!(AllowBuiltinTys);
