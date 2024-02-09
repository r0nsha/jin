use std::mem;

use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    db::{AdtId, Db, DefId, ModuleId, ScopeLevel},
    middle::{IsUfcs, Vis},
    qpath::QPath,
    span::Span,
    ty::{Ty, TyKind},
    typeck2::builtins::BuiltinTys,
};

#[derive(Debug)]
pub(super) struct GlobalEnv {
    modules: FxHashMap<ModuleId, ModuleEnv>,
    pub(super) builtin_tys: BuiltinTys,
}

impl GlobalEnv {
    pub(super) fn new(builtin_tys: BuiltinTys) -> Self {
        Self { modules: FxHashMap::default(), builtin_tys }
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
        self.modules.insert(id, ModuleEnv::new(id));
    }
}

#[derive(Debug)]
pub(super) struct ModuleEnv {
    pub(super) module_id: ModuleId,
    pub(super) ns: Ns,
    pub(super) assoc_ns: FxHashMap<AssocTy, Ns>,
    pub(super) globs: FxHashMap<ModuleId, IsUfcs>,
}

impl ModuleEnv {
    pub(super) fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            ns: Ns::new(module_id),
            assoc_ns: FxHashMap::default(),
            globs: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub(super) struct Ns {
    #[allow(unused)]
    pub(super) module_id: ModuleId,
    pub(super) defs: UstrMap<NsDef>,
    // pub(super) fns: UstrMap<FnCandidateSet>,
    pub(super) defined_fns: UstrMap<Vec<DefId>>,
}

impl Ns {
    pub(super) fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            defs: UstrMap::default(),
            // fns: FxHashMap::default()
            defined_fns: UstrMap::default(),
        }
    }
}

#[derive(Debug)]
pub(super) struct NsDef {
    pub(super) id: DefId,
    pub(super) vis: Vis,
    pub(super) span: Span,
}

impl NsDef {
    pub(super) fn new(id: DefId, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
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

    pub(super) fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("to have a scope")
    }

    pub(super) fn insert(&mut self, k: Ustr, v: DefId) {
        self.scopes.last_mut().unwrap().defs.insert(k, v);
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

    #[inline]
    pub(super) fn in_std(&self, db: &Db) -> bool {
        db.package(db[self.module_id].package).is_std(db)
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
