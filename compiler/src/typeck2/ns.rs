use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    db::{AdtId, Db, DefId, ModuleId},
    middle::{IsUfcs, Vis},
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
    pub(super) module_id: ModuleId,
    defs: UstrMap<NsDef>,
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

    pub(super) fn insert_def(&mut self, name: Ustr, def: NsDef) -> Option<NsDef> {
        self.defs.insert(name, def)
    }

    pub(super) fn get_def(&self, from_module: ModuleId, name: Ustr) -> Option<DefId> {
        self.defs
            .get(&name)
            .filter(|def| def.vis == Vis::Public || from_module == self.module_id)
            .map(|def| def.id)
    }

    pub(super) fn contains_def(&self, name: Ustr) -> Option<&NsDef> {
        self.defs.get(&name)
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
