use std::collections::HashMap;

use ustr::{ustr, Ustr, UstrMap};

use crate::{
    common::QPath,
    db::{Db, DefId, ModuleId, ScopeLevel, Vis},
    hir,
};

#[derive(Debug)]
pub struct GlobalScope {
    modules: HashMap<(ModuleId, Ustr), DefId>,
    pub resolved_global_pats: HashMap<(ModuleId, usize /* ItemId */), hir::Pat>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self { modules: HashMap::new(), resolved_global_pats: HashMap::new() }
    }

    pub fn lookup(&self, module_id: ModuleId, name: Ustr) -> Option<DefId> {
        self.modules.get(&(module_id, name)).copied()
    }

    pub fn insert(&mut self, module_id: ModuleId, name: Ustr, id: DefId) -> Option<DefId> {
        self.modules.insert((module_id, name), id)
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
        let mut qpath = db[self.module_id].name.clone();
        qpath.extend(self.scopes.iter().map(|s| s.name));
        qpath
    }

    #[inline]
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    #[inline]
    pub fn in_global_scope(&self) -> bool {
        self.depth() == 0
    }

    pub fn in_kind(&self, kind: ScopeKind) -> bool {
        self.scopes.iter().any(|s| s.kind == kind)
    }

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

impl Scope {
    #[allow(unused)]
    pub fn get(&mut self, name: Ustr) -> Option<DefId> {
        self.defs.get(&name).copied()
    }

    pub fn insert(&mut self, name: Ustr, id: DefId) {
        self.defs.insert(name, id);
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ScopeKind {
    Fn,
    Block,
    Initializer,
}

#[derive(Debug)]
pub enum EnvKind<'a> {
    Global(ModuleId, Vis),
    Local(&'a mut Env),
}
