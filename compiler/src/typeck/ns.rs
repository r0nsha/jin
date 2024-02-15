use std::mem;

use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    db::{AdtId, Db, DefId, ModuleId, ScopeLevel},
    diagnostics::DiagnosticResult,
    middle::{IsUfcs, Pat, Vis},
    qpath::QPath,
    span::{Span, Spanned},
    ty::{Ty, TyKind},
    typeck::{builtins::BuiltinTys, errors, lookup::FnCandidateSet, Typeck},
    word::Word,
};

#[derive(Debug)]
pub(crate) struct GlobalEnv {
    pub(super) modules: FxHashMap<ModuleId, ModuleEnv>,
    pub(super) assoc_ns: FxHashMap<AssocTy, Ns>,
    pub(super) builtin_tys: BuiltinTys,
}

impl GlobalEnv {
    pub(super) fn new(builtin_tys: BuiltinTys) -> Self {
        Self { modules: FxHashMap::default(), assoc_ns: FxHashMap::default(), builtin_tys }
    }

    #[track_caller]
    pub(super) fn module(&self, id: ModuleId) -> &ModuleEnv {
        self.modules.get(&id).unwrap()
    }

    #[track_caller]
    pub(super) fn module_mut(&mut self, id: ModuleId) -> &mut ModuleEnv {
        self.modules.get_mut(&id).unwrap()
    }

    pub(super) fn insert_module(&mut self, id: ModuleId) {
        self.modules.insert(id, ModuleEnv::new());
    }
}

#[derive(Debug)]
pub(super) struct ModuleEnv {
    pub(super) ns: Ns,
    pub(super) globs: FxHashMap<ModuleId, GlobImport>,
}

impl ModuleEnv {
    pub(super) fn new() -> Self {
        Self { ns: Ns::new(), globs: FxHashMap::default() }
    }
}

impl Default for ModuleEnv {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(super) struct Ns {
    pub(super) defs: UstrMap<NsDef>,
    pub(super) fns: UstrMap<FnCandidateSet>,
    pub(super) defined_fns: UstrMap<Vec<DefId>>,
}

impl Ns {
    pub(super) fn new() -> Self {
        Self { defs: UstrMap::default(), fns: UstrMap::default(), defined_fns: UstrMap::default() }
    }
}

impl Default for Ns {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct NsDef {
    pub(super) id: DefId,
    pub(super) name: Word,
    pub(super) module_id: ModuleId,
    pub(super) vis: Vis,
}

impl NsDef {
    pub(super) fn from_def_id(db: &Db, id: DefId) -> Self {
        let def = &db[id];
        Self { id, name: def.word(), module_id: def.scope.module_id, vis: def.scope.vis }
    }

    pub(super) fn check_access(
        &self,
        cx: &Typeck,
        from_module: ModuleId,
        span: Span,
    ) -> DiagnosticResult<()> {
        self.can_access(cx, from_module).then_some(()).ok_or_else(|| {
            errors::private_access_violation(cx.db, self.module_id, self.name.name(), span)
        })
    }

    pub(super) fn can_access(&self, cx: &Typeck, from_module: ModuleId) -> bool {
        cx.can_access(from_module, self.module_id, self.vis)
    }

    pub(super) fn span(&self) -> Span {
        self.name.span()
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
            TyKind::Adt(adt_id, _) => Self::Adt(*adt_id),
            _ => Self::BuiltinTy(value),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct GlobImport {
    pub(super) is_ufcs: IsUfcs,
    pub(super) vis: Vis,
}

impl GlobImport {
    pub(super) fn merge_transitive(&self, other: &Self) -> Self {
        Self {
            is_ufcs: if other.is_ufcs == IsUfcs::Yes { IsUfcs::Yes } else { self.is_ufcs },
            vis: self.vis.min(other.vis),
        }
    }
}

#[derive(Debug)]
pub(super) struct Env {
    module_id: ModuleId,
    scopes: Vec<Scope>,
}

impl Env {
    pub(super) fn new(module_id: ModuleId) -> Self {
        Self { module_id, scopes: vec![] }
    }

    pub(super) fn with_anon_scope<R>(
        &mut self,
        kind: ScopeKind,
        f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.with_scope(None, kind, f)
    }

    pub(super) fn with_named_scope<R>(
        &mut self,
        name: Ustr,
        kind: ScopeKind,
        f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.with_scope(Some(name), kind, f)
    }

    pub(super) fn with_scope<R>(
        &mut self,
        name: Option<Ustr>,
        kind: ScopeKind,
        mut f: impl FnMut(&mut Self) -> R,
    ) -> R {
        self.scopes.push(Scope { kind, name, defs: UstrMap::default() });
        let res = f(self);
        self.scopes.pop();
        res
    }

    #[allow(unused)]
    pub(super) fn current(&self) -> &Scope {
        self.scopes.last().expect("to have a scope")
    }

    pub(super) fn insert(&mut self, k: Ustr, v: DefId) {
        self.scopes.last_mut().unwrap().defs.insert(k, v);
    }

    pub(super) fn insert_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Name(name) => self.insert(name.word.name(), name.id),
            Pat::Discard(_) => (),
        }
    }

    pub(super) fn lookup(&self, k: Ustr) -> Option<&DefId> {
        self.lookup_depth(k).map(|r| r.1)
    }

    pub(super) fn lookup_depth(&self, k: Ustr) -> Option<(usize, &DefId)> {
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

    pub(super) fn scope_level(&self) -> ScopeLevel {
        match self.depth() {
            0 => unreachable!("expected to have at least one scope"),
            n => ScopeLevel::Local(n),
        }
    }

    pub(super) fn scope_path(&self, db: &Db) -> QPath {
        let mut qpath = db[self.module_id].qpath.clone();
        qpath.extend(self.scopes.iter().flat_map(|s| s.name));
        qpath
    }

    pub(super) fn fn_id(&self) -> Option<DefId> {
        self.scopes.iter().find_map(|s| match s.kind {
            ScopeKind::Fn(id) => Some(id),
            ScopeKind::Loop | ScopeKind::Block | ScopeKind::TyDef => None,
        })
    }

    pub(super) fn in_scope_kind(&self, kind: &ScopeKind) -> bool {
        let discriminant = mem::discriminant(kind);
        self.scopes.iter().any(|s| mem::discriminant(&s.kind) == discriminant)
    }

    #[inline]
    pub(super) fn depth(&self) -> usize {
        self.scopes.len()
    }

    #[inline]
    pub(super) fn in_global_scope(&self) -> bool {
        self.depth() == 0
    }

    #[inline]
    pub(super) fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub(super) struct Scope {
    pub(super) kind: ScopeKind,
    pub(super) name: Option<Ustr>,
    pub(super) defs: UstrMap<DefId>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum ScopeKind {
    Fn(DefId),
    TyDef,
    Loop,
    Block,
}

pub(super) fn in_std(db: &Db, module_id: ModuleId) -> bool {
    db.package(db[module_id].package).is_std(db)
}
