use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast,
    ast::Ast,
    common::QPath,
    db::{Db, DefId, DefInfo, DefKind, ModuleId, ScopeInfo, ScopeLevel, Vis},
    span::Span,
    sym,
    ty::Ty,
};

#[derive(Debug)]
pub struct GlobalScope {
    defs: FxHashMap<(ModuleId, Ustr), DefId>,
    items: FxHashMap<(ModuleId, Ustr), ast::ItemId>,
}

impl GlobalScope {
    pub fn new(ast: &Ast) -> Self {
        Self { defs: FxHashMap::default(), items: Self::init_items(ast) }
    }

    fn init_items(ast: &Ast) -> FxHashMap<(ModuleId, Ustr), ast::ItemId> {
        let mut items = FxHashMap::default();

        for module in &ast.modules {
            let module_id = module.id.expect("to be resolved");

            for (idx, item) in module.items.iter().enumerate() {
                let id = ast::ItemId::from(idx);

                item.walk_names(|word| {
                    items.insert((module_id, word.name()), id);
                });
            }
        }

        items
    }

    pub fn get_def(&self, module_id: ModuleId, name: Ustr) -> Option<DefId> {
        self.defs.get(&(module_id, name)).copied()
    }

    pub fn insert_def(&mut self, module_id: ModuleId, name: Ustr, id: DefId) -> Option<DefId> {
        self.defs.insert((module_id, name), id)
    }

    pub fn get_item(&self, module_id: ModuleId, name: Ustr) -> Option<ast::ItemId> {
        self.items.get(&(module_id, name)).copied()
    }
}

#[derive(Debug)]
pub struct BuiltinTys {
    inner: UstrMap<DefId>,
}

impl BuiltinTys {
    pub fn new(db: &mut Db) -> Self {
        let mut this = Self { inner: UstrMap::default() };

        this.define_ty(db, sym::I8, db.types.i8);
        this.define_ty(db, sym::I16, db.types.i16);
        this.define_ty(db, sym::I32, db.types.i32);
        this.define_ty(db, sym::I64, db.types.i64);
        this.define_ty(db, sym::INT, db.types.int);

        this.define_ty(db, sym::U8, db.types.u8);
        this.define_ty(db, sym::U16, db.types.u16);
        this.define_ty(db, sym::U32, db.types.u32);
        this.define_ty(db, sym::U64, db.types.u64);
        this.define_ty(db, sym::UINT, db.types.uint);

        this.define_ty(db, sym::STR, db.types.str);
        this.define_ty(db, sym::BOOL, db.types.bool);
        this.define_ty(db, sym::NEVER, db.types.never);

        this
    }

    fn define_ty(&mut self, db: &mut Db, name: &str, ty: Ty) -> Option<DefId> {
        let typ = db.types.typ;
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
                typ,
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
