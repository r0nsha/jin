use std::mem;

use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast,
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel},
    diagnostics::{Diagnostic, Label},
    hir,
    middle::{Mutability, Vis},
    qpath::QPath,
    span::{Span, Spanned},
    sym,
    ty::{Ty, TyKind},
    typeck::{errors, Typeck, TypeckResult},
    word::Word,
};

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
        let id = DefInfo::alloc(self.db, qpath, scope, kind, mutability, ty, name.span());
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

        if let Some(def) =
            self.global_scope.insert_def(symbol, GlobalScopeDef::new(id, vis, name.span()))
        {
            return Err(errors::multiple_item_def_err(def.span, name));
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
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private },
            kind,
            mutability,
            ty,
            name.span(),
        );

        env.current_mut().defs.insert(name.name(), id);
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
            self.define_global_def(env.module_id(), vis, kind, name, mutability, ty)
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
                let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };

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
                    vis,
                    word,
                    params: sig.params.iter().map(|p| p.ty).collect(),
                };

                self.global_scope.fns.entry(symbol).or_default().try_insert(candidate).map_err(
                    |err| match err {
                        FnCandidateInsertError::AlreadyExists { prev, curr } => {
                            errors::multiple_fn_def_err(prev.word.span(), curr.word)
                        }
                    },
                )?;

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

    pub fn define_pat(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        pat: &ast::Pat,
        ty: Ty,
    ) -> TypeckResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => {
                let id = self.define_def(env, name.vis, kind, name.word, name.mutability, ty)?;
                Ok(hir::Pat::Name(hir::NamePat { id, word: name.word }))
            }
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
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
            env.current_mut().defs.insert(name.name(), id);
        }

        Ok(())
    }

    pub fn lookup_fn(&mut self, env: &Env, word: Word, call_args: &[Ty]) -> TypeckResult<DefId> {
        let name = word.name();

        if let Some(id) = env.lookup(name).copied() {
            return Ok(id);
        }

        let symbol = Symbol::new(env.module_id(), name);

        if let Some(id) = self.lookup_fn_candidate(env.module_id(), &symbol, call_args)? {
            return Ok(id);
        }

        if self.checking_modules {
            // TODO: find from unresolved fn candidates
            // if let Some(id) = self.find_and_check_item(&symbol)? {
            //     return Ok(id);
            // }
        }

        self.lookup_global_def(&symbol)?.ok_or_else(|| {
            Diagnostic::error()
                .with_message(format!("cannot find `{}` in this scope", symbol.name))
                .with_label(Label::primary(word.span()).with_message("not found in this scope"))
        })
    }

    fn lookup_fn_candidate(
        &self,
        from_module: ModuleId,
        symbol: &Symbol,
        call_args: &[Ty],
    ) -> Result<Option<DefId>, Diagnostic> {
        // TODO: collect candidates from globs
        // TODO: take visibility into account
        // TODO: on multiple candidates: ambiguous call, these functions apply: ... (fully qualified function names)

        let Some(set) = self.global_scope.fns.get(symbol) else { return Ok(None) };
        let candidates = set.get(call_args);

        match candidates.len() {
            0 => Ok(None),
            1 => Ok(Some(candidates.first().unwrap().id)),
            _ => todo!("multiple candidates: error!"),
        }
    }

    pub fn lookup_def(&mut self, env: &Env, word: Word) -> TypeckResult<DefId> {
        let name = word.name();

        if let Some(id) = env.lookup(name).copied() {
            return Ok(id);
        }

        let symbol = Symbol::new(env.module_id(), name);

        self.lookup_global_def(&symbol)?.ok_or_else(|| {
            Diagnostic::error()
                .with_message(format!("cannot find `{}` in this scope", symbol.name))
                .with_label(Label::primary(word.span()).with_message("not found in this scope"))
        })
    }

    pub fn lookup_global_def(&mut self, symbol: &Symbol) -> TypeckResult<Option<DefId>> {
        if let Some(id) = self.lookup_def_in_global_scope(symbol.module_id, symbol) {
            return Ok(Some(id));
        }

        if self.checking_modules {
            if let Some(id) = self.find_and_check_item(symbol)? {
                return Ok(Some(id));
            }
        }

        if let Some(id) = self.builtin_tys.get(symbol.name) {
            return Ok(Some(id));
        }

        Ok(None)
    }

    pub fn lookup_def_in_module(
        &mut self,
        from_module: ModuleId,
        in_module: ModuleId,
        word: Word,
    ) -> TypeckResult<DefId> {
        let symbol = Symbol::new(in_module, word.name());

        let id = if let Some(id) = self.lookup_def_in_global_scope(from_module, &symbol) {
            id
        } else {
            self.find_and_check_item(&symbol)?.ok_or_else(|| {
                let module_name = self.db[in_module].qpath.join();

                Diagnostic::error()
                    .with_message(format!("cannot find `{word}` in module `{module_name}`",))
                    .with_label(
                        Label::primary(word.span())
                            .with_message(format!("not found in {module_name}")),
                    )
            })?
        };

        self.check_def_access(from_module, id, word.span())?;

        Ok(id)
    }

    fn lookup_def_in_global_scope(&self, from_module: ModuleId, symbol: &Symbol) -> Option<DefId> {
        if let Some(id) = self.global_scope.get_def(from_module, symbol) {
            Some(id)
        } else {
            for module_id in &self.resolution_state.module_state(from_module).globs {
                let new_symbol = symbol.with_module_id(*module_id);

                if let Some(id) = self.global_scope.get_def(from_module, &new_symbol) {
                    return Some(id);
                }
            }

            None
        }
    }

    fn check_def_access(
        &self,
        module_id: ModuleId,
        accessed: DefId,
        span: Span,
    ) -> TypeckResult<()> {
        let def = &self.db[accessed];

        match def.scope.vis {
            Vis::Private if module_id != def.scope.module_id => {
                let module_name = self.db[def.scope.module_id].qpath.join();

                Err(Diagnostic::error()
                    .with_message(format!("`{}` is private to module `{}`", def.name, module_name))
                    .with_label(
                        Label::primary(span).with_message(format!("private to `{module_name}`")),
                    ))
            }
            _ => Ok(()),
        }
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

    pub fn get_def(&self, from_module: ModuleId, symbol: &Symbol) -> Option<DefId> {
        if let Some(def) = self.defs.get(symbol) {
            if def.vis == Vis::Public || from_module == symbol.module_id {
                return Some(def.id);
            }
        }

        None
    }

    fn insert_def(&mut self, symbol: Symbol, def: GlobalScopeDef) -> Option<GlobalScopeDef> {
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

    pub fn with_anon_scope<R>(&mut self, kind: ScopeKind, mut f: impl FnMut(&mut Self) -> R) -> R {
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

    #[allow(unused)]
    pub fn lookup_mut(&mut self, k: Ustr) -> Option<&mut DefId> {
        self.lookup_depth_mut(k).map(|r| r.1)
    }

    #[allow(unused)]
    pub fn lookup_depth(&self, k: Ustr) -> Option<(usize, &DefId)> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(value) = scope.defs.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    pub fn lookup_depth_mut(&mut self, k: Ustr) -> Option<(usize, &mut DefId)> {
        for (depth, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(value) = scope.defs.get_mut(&k) {
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

    pub fn try_insert(&mut self, candidate: FnCandidate) -> Result<(), FnCandidateInsertError> {
        if let Some(prev) = self.0.iter().find(|c| *c == &candidate) {
            return Err(FnCandidateInsertError::AlreadyExists {
                prev: prev.clone(),
                curr: candidate,
            });
        }

        self.0.push(candidate);
        Ok(())
    }

    pub fn get(&self, args: &[Ty]) -> Vec<&FnCandidate> {
        let scores = self.scores(args);
        let Some(&min_score) = scores.iter().map(|(_, s)| s).min() else { return vec![] };
        scores.into_iter().filter_map(|(c, s)| (s == min_score).then_some(c)).collect()
    }

    // TODO: this can be an iterator
    fn scores(&self, args: &[Ty]) -> Vec<(&FnCandidate, u32)> {
        let mut scores = vec![];

        for c in &self.0 {
            if let Some(score) = c.score(args) {
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
    pub vis: Vis,
    pub word: Word,
    pub params: Vec<Ty>,
}

impl FnCandidate {
    fn score(&self, args: &[Ty]) -> Option<u32> {
        if self.params.len() != args.len() {
            return None;
        }

        let mut score = 0;

        for (param, arg) in self.params.iter().zip(args.iter()) {
            let dist = Self::distance(*param, *arg)?;
            score += dist;
        }

        Some(score)
    }

    fn distance(param: Ty, arg: Ty) -> Option<u32> {
        if param == arg {
            return Some(0);
        }

        None
    }
}

impl PartialEq for FnCandidate {
    fn eq(&self, other: &Self) -> bool {
        self.word.name() == other.word.name()
            && self.params.iter().zip(other.params.iter()).all(|(p1, p2)| p1.kind() == p2.kind())
    }
}

impl Eq for FnCandidate {}
