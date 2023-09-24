use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    common::QPath,
    db::{Db, DefId, DefInfo, DefKind, ModuleId, ScopeInfo, ScopeLevel, Vis},
    hir,
    span::Span,
    sym,
    ty::Ty,
};

#[derive(Debug)]
pub struct GlobalScope {
    modules: FxHashMap<(ModuleId, Ustr), DefId>,
    pub resolved_pats: FxHashMap<(ModuleId, usize /* ItemId */), hir::Pat>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self { modules: FxHashMap::default(), resolved_pats: FxHashMap::default() }
    }

    pub fn lookup(&self, module_id: ModuleId, name: Ustr) -> Option<DefId> {
        self.modules.get(&(module_id, name)).copied()
    }

    pub fn insert(&mut self, module_id: ModuleId, name: Ustr, id: DefId) -> Option<DefId> {
        self.modules.insert((module_id, name), id)
    }
}

#[derive(Debug)]
pub struct BuiltinTys {
    inner: UstrMap<DefId>,
}

impl BuiltinTys {
    pub fn new(db: &mut Db) -> Self {
        let mut inner = UstrMap::default();
        let typ = db.types.typ;

        let mut mk = |name: &str, ty: &dyn std::ops::Fn(&Db) -> Ty| -> Option<DefId> {
            let name = ustr(name);
            let scope_info = ScopeInfo {
                module_id: db.main_module_id().expect("to be resolved"),
                level: ScopeLevel::Global,
                vis: Vis::Public,
            };

            inner.insert(
                name,
                DefInfo::alloc(
                    db,
                    QPath::from(name),
                    scope_info,
                    DefKind::Ty(ty(db)),
                    typ,
                    Span::unknown(),
                ),
            )
        };

        mk(sym::I8, &|db| db.types.i8);
        mk(sym::I16, &|db| db.types.i16);
        mk(sym::I32, &|db| db.types.i32);
        mk(sym::I64, &|db| db.types.i64);
        mk(sym::INT, &|db| db.types.int);

        mk(sym::U8, &|db| db.types.u8);
        mk(sym::U16, &|db| db.types.u16);
        mk(sym::U32, &|db| db.types.u32);
        mk(sym::U64, &|db| db.types.u64);
        mk(sym::UINT, &|db| db.types.uint);

        mk(sym::STR, &|db| db.types.str);
        mk(sym::BOOL, &|db| db.types.bool);
        mk(sym::NEVER, &|db| db.types.never);

        Self { inner }
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
