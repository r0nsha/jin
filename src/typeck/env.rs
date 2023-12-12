use std::{iter, mem};

use itertools::Itertools;
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast,
    db::{
        Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel,
    },
    diagnostics::{Diagnostic, Label},
    hir,
    macros::create_bool_enum,
    middle::{Mutability, Vis},
    qpath::QPath,
    span::{Span, Spanned},
    sym,
    ty::{printer::FnTyPrinter, FnTy, FnTyParam, InferTy, Ty, TyKind},
    typeck::{coerce::Coerce, errors, Typeck, TypeckResult},
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
        let id = DefInfo::alloc(
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
        let id = DefInfo::alloc(
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

    pub fn define_fn(
        &mut self,
        module_id: ModuleId,
        fun: &ast::Fn,
        sig: &hir::FnSig,
    ) -> TypeckResult<DefId> {
        let vis = fun.vis;
        let word = fun.sig.word;
        let symbol = Symbol::new(module_id, word.name());

        match fun.kind {
            ast::FnKind::Bare { .. } => {
                let qpath = self.db[module_id].qpath.clone().child(word.name());
                let scope =
                    ScopeInfo { module_id, level: ScopeLevel::Global, vis };

                if let Some(def) = self.global_scope.defs.get(&symbol) {
                    return Err(errors::multiple_item_def_err(def.span, word));
                }

                let id = {
                    DefInfo::alloc(
                        self.db,
                        qpath,
                        scope,
                        DefKind::Fn(FnInfo::Bare),
                        Mutability::Imm,
                        sig.ty,
                        word.span(),
                    )
                };

                let candidate = FnCandidate {
                    id,
                    word,
                    ty: sig.ty.as_fn().cloned().unwrap(),
                };
                self.insert_fn_candidate(symbol, candidate)?;

                Ok(id)
            }
            ast::FnKind::Extern { .. } => self.define_global_def(
                module_id,
                vis,
                DefKind::Fn(FnInfo::Extern),
                sig.word,
                Mutability::Imm,
                sig.ty,
            ),
        }
    }

    pub fn insert_fn_candidate(
        &mut self,
        symbol: Symbol,
        candidate: FnCandidate,
    ) -> TypeckResult<()> {
        self.global_scope
            .fns
            .entry(symbol)
            .or_default()
            .try_insert(candidate)
            .map_err(|err| match err {
                FnCandidateInsertError::AlreadyExists { prev, curr } => {
                    errors::multiple_fn_def_err(
                        self.db,
                        symbol.module_id,
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
        pat: &ast::Pat,
        ty: Ty,
    ) -> TypeckResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => {
                let id = self.define_def(
                    env,
                    name.vis,
                    kind,
                    name.word,
                    name.mutability,
                    ty,
                )?;
                Ok(hir::Pat::Name(hir::NamePat { id, word: name.word }))
            }
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
    }

    pub fn insert_pat(
        &mut self,
        env: &mut Env,
        pat: &hir::Pat,
    ) -> TypeckResult<()> {
        match pat {
            hir::Pat::Name(name) => {
                self.insert_def(
                    env,
                    name.word,
                    name.id,
                    self.db[name.id].scope.vis,
                )?;
            }
            hir::Pat::Discard(_) => (),
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

    pub fn import_lookup(
        &mut self,
        from_module: ModuleId,
        in_module: ModuleId,
        word: Word,
    ) -> TypeckResult<Vec<LookupResult>> {
        let symbol = Symbol::new(in_module, word.name());

        if self.checking_items {
            self.find_and_check_items(&symbol)?;
        }

        let results =
            self.lookup_global_many(in_module, &symbol, ShouldLookupFns::Yes);

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
            self.find_and_check_items(&symbol)?;
        }

        let from_module = env.module_id();

        if let Query::Fn(fn_query) = query {
            if let Some(id) =
                self.lookup_fn_candidate(from_module, in_module, fn_query)?
            {
                return Ok(id);
            }
        }

        let lookup_fns = ShouldLookupFns::from(!matches!(query, Query::Fn(_)));

        self.lookup_global_one(&symbol, query.span(), lookup_fns)?.ok_or_else(
            || match query {
                Query::Name(word) => errors::name_not_found(
                    self.db,
                    from_module,
                    in_module,
                    *word,
                ),
                Query::Fn(fn_query) => errors::fn_not_found(self.db, fn_query),
            },
        )
    }

    #[inline]
    fn find_and_check_items(&mut self, symbol: &Symbol) -> TypeckResult<()> {
        let lookup_modules =
            self.get_lookup_modules(symbol.module_id).collect::<Vec<_>>();

        for module_id in lookup_modules {
            if let Some(item_ids) =
                self.global_scope.symbol_to_item.get(symbol).cloned()
            {
                let mut env = Env::new(module_id);

                for item_id in item_ids {
                    let item = &self.ast.modules[module_id].items[item_id];

                    if self
                        .resolution_state
                        .get_item_status(&ast::GlobalItemId::new(
                            module_id, item_id,
                        ))
                        .is_unresolved()
                    {
                        self.check_item(
                            &mut env,
                            item,
                            ast::GlobalItemId::new(module_id, item_id),
                        )?;
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
    ) -> Result<Option<DefId>, Diagnostic> {
        let mut candidates = self
            .get_lookup_modules(in_module)
            .filter_map(|module_id| {
                self.global_scope
                    .fns
                    .get(&Symbol::new(module_id, query.word.name()))
            })
            .flat_map(|set| set.find(self, query))
            .unique_by(|candidate| candidate.id)
            .collect::<Vec<_>>();

        if !candidates.is_empty()
            && candidates.iter().all(|c| !self.can_access(from_module, c.id))
        {
            return Err(Diagnostic::error()
                .with_message(format!(
                    "all functions which apply to `{}` are private to their module",
                    query.display(self.db)
                ))
                .with_label(
                    Label::primary(query.word.span()).with_message("no accessible function found"),
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
        lookup_fns: ShouldLookupFns,
    ) -> TypeckResult<Option<DefId>> {
        let results =
            self.lookup_global_many(symbol.module_id, symbol, lookup_fns);

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
        lookup_fns: ShouldLookupFns,
    ) -> Vec<LookupResult> {
        let lookup_modules = self.get_lookup_modules(in_module);
        let mut defs = vec![];

        for module_id in lookup_modules {
            let symbol = symbol.with_module_id(module_id);

            if let Some(id) = self.global_scope.get_def(in_module, &symbol) {
                defs.push(LookupResult::Def(id));
            } else if lookup_fns == ShouldLookupFns::Yes {
                if let Some(candidates) = self.global_scope.fns.get(&symbol) {
                    defs.extend(
                        candidates.iter().cloned().map(LookupResult::Fn),
                    );
                }
            }
        }

        if defs.is_empty() {
            return self
                .builtin_tys
                .get(symbol.name)
                .into_iter()
                .map(LookupResult::Def)
                .collect();
        }

        defs
    }

    fn get_lookup_modules(
        &self,
        in_module: ModuleId,
    ) -> impl Iterator<Item = ModuleId> + '_ {
        iter::once(in_module).chain(
            self.resolution_state.module_state(in_module).globs.iter().copied(),
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
    pub symbol_to_item: FxHashMap<Symbol, Vec<ast::ItemId>>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            defs: FxHashMap::default(),
            fns: FxHashMap::default(),
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

        this.define(db, sym::I8, db.types.i8);
        this.define(db, sym::I16, db.types.i16);
        this.define(db, sym::I32, db.types.i32);
        this.define(db, sym::I64, db.types.i64);
        this.define(db, sym::INT, db.types.int);

        this.define(db, sym::U8, db.types.u8);
        this.define(db, sym::U16, db.types.u16);
        this.define(db, sym::U32, db.types.u32);
        this.define(db, sym::U64, db.types.u64);
        this.define(db, sym::UINT, db.types.uint);

        this.define(db, sym::F32, db.types.f32);
        this.define(db, sym::F64, db.types.f64);

        this.define(db, sym::STR, db.types.str);
        this.define(db, sym::BOOL, db.types.bool);
        this.define(db, sym::NEVER, db.types.never);

        this
    }

    fn define(&mut self, db: &mut Db, name: &str, ty: Ty) -> Option<DefId> {
        let name = ustr(name);
        let scope_info = ScopeInfo {
            module_id: db.main_module_id().expect("to be resolved"),
            level: ScopeLevel::Global,
            vis: Vis::Public,
        };

        self.inner.insert(
            name,
            DefInfo::alloc(
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

#[derive(Debug)]
pub struct Env {
    module_id: ModuleId,
    scopes: Vec<Scope>,
}

impl Env {
    const ANON_SCOPE: &str = "_";

    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, scopes: vec![] }
    }

    pub fn push_scope(&mut self, name: Ustr, kind: ScopeKind) {
        self.scopes.push(Scope { kind, name, defs: UstrMap::default() });
    }

    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub fn with_scope<R>(
        &mut self,
        name: Ustr,
        kind: ScopeKind,
        mut f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.push_scope(name, kind);
        let res = f(self);
        self.pop_scope();
        res
    }

    pub fn with_anon_scope<R>(
        &mut self,
        kind: ScopeKind,
        mut f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.push_scope(ustr(Self::ANON_SCOPE), kind);
        let res = f(self);
        self.pop_scope();
        res
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
            ScopeKind::Loop | ScopeKind::Block => None,
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
        if !query.ty_args.is_empty()
            && query.ty_args.len() != self.ty.collect_params().len()
        {
            return None;
        }

        if self.ty.params.len() != query.args.len() {
            return None;
        }

        // Make sure that all passed named arguments exist in this candidate
        if !query.args.iter().all(|arg| {
            arg.name.map_or(true, |name| {
                self.ty.params.iter().any(|p| Some(name) == p.name)
            })
        }) {
            return None;
        }

        let mut score = 0;

        for (param, arg) in self.ty.params.iter().zip(query.args.iter()) {
            let dist = Self::distance(cx, param.ty, arg.ty)?;
            score += dist;
        }

        Some(score)
    }

    // Calculates the distances between an argument and the
    // parameter it is applied to. The actual distance is calculated by the amount
    // of "steps" required to convert the argument to the parameter.
    fn distance(cx: &Typeck, param: Ty, arg: Ty) -> Option<u32> {
        if param.unify(arg, cx).is_ok() {
            return Some(0);
        }

        if arg.can_coerce(&param, cx) {
            return Some(1);
        }

        if let (_, TyKind::Infer(InferTy::Ty(_)) | TyKind::Param(_)) =
            (arg.kind(), param.kind())
        {
            return Some(2);
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
            .any(|(p1, p2)| p1.ty != p2.ty)
        {
            return false;
        }

        true
    }
}

impl Eq for FnCandidate {}

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
}

#[derive(Debug, Clone)]
pub struct FnQuery<'a> {
    pub word: Word,
    pub ty_args: &'a [Ty],
    pub args: &'a [FnTyParam],
}

impl<'a> FnQuery<'a> {
    pub fn new(word: Word, ty_args: &'a [Ty], args: &'a [FnTyParam]) -> Self {
        Self { word, ty_args, args }
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
