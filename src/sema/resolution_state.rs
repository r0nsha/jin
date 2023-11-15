use enum_as_inner::EnumAsInner;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ast,
    db::{DefId, ModuleId},
    hir,
};

pub struct ResolutionState {
    module_states: FxHashMap<ModuleId, ModuleState>,
    item_statuses: FxHashMap<ast::GlobalItemId, ItemStatus>,
    resolved_fn_sigs: ast::ItemMap<ResolvedFnSig>,
    check_stack: Vec<ast::GlobalItemId>,
}

impl ResolutionState {
    pub fn new() -> Self {
        Self {
            module_states: FxHashMap::default(),
            item_statuses: FxHashMap::default(),
            resolved_fn_sigs: FxHashMap::default(),
            check_stack: vec![],
        }
    }

    pub fn create_module_state(&mut self, id: ModuleId) -> &mut ModuleState {
        self.module_states.entry(id).or_default()
    }

    pub fn module_state(&self, id: ModuleId) -> &ModuleState {
        self.module_states.get(&id).unwrap()
    }

    pub fn module_state_mut(&mut self, id: ModuleId) -> &mut ModuleState {
        self.module_states.get_mut(&id).unwrap()
    }

    pub fn get_item_status(&self, id: &ast::GlobalItemId) -> ItemStatus {
        self.item_statuses.get(id).copied().unwrap_or(ItemStatus::Unresolved)
    }

    pub fn mark_in_progress_item(&mut self, id: ast::GlobalItemId) -> Result<(), CyclicItemErr> {
        match self.get_item_status(&id) {
            ItemStatus::Unresolved => {
                self.item_statuses.insert(id, ItemStatus::InProgress);
                self.check_stack.push(id);
                Ok(())
            }
            ItemStatus::InProgress => Err(CyclicItemErr {
                origin: id,
                causee: self.check_stack.last().copied().expect("to have an checked item"),
            }),
            ItemStatus::Resolved => Ok(()),
        }
    }

    pub fn mark_resolved_item(&mut self, id: ast::GlobalItemId) {
        self.item_statuses.insert(id, ItemStatus::Resolved);
        self.check_stack.pop();
    }

    pub fn get_resolved_fn_sig(&self, id: ast::GlobalItemId) -> Option<&ResolvedFnSig> {
        self.resolved_fn_sigs.get(&id)
    }

    pub fn insert_resolved_fn_sig(&mut self, id: ast::GlobalItemId, resolved_sig: ResolvedFnSig) {
        self.resolved_fn_sigs.insert(id, resolved_sig);
    }
}

#[derive(Debug, Clone)]
pub struct ModuleState {
    pub status: ModuleStatus,
    pub globs: FxHashSet<ModuleId>,
}

impl ModuleState {
    pub fn new() -> Self {
        Self { status: ModuleStatus::Unresolved, globs: FxHashSet::default() }
    }
}

impl Default for ModuleState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum ModuleStatus {
    Unresolved,
    InProgress,
    Resolved,
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum ItemStatus {
    Unresolved,
    InProgress,
    Resolved,
}

#[derive(Debug, Clone)]
pub struct ResolvedFnSig {
    pub id: DefId,
    pub sig: hir::FnSig,
}

#[derive(Debug, Clone, Copy)]
pub struct CyclicItemErr {
    /// The item that is actually cyclic and referenced by `causee`
    pub origin: ast::GlobalItemId,
    /// The item that caused the cycle to happen, and references `origin`
    pub causee: ast::GlobalItemId,
}
