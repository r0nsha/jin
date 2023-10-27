use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;

use crate::{ast, db::DefId, hir};

pub struct ItemState {
    statuses: ast::ItemMap<ItemStatus>,
    resolved_fn_sigs: ast::ItemMap<ResolvedFnSig>,
    check_stack: Vec<ast::GlobalItemId>,
}

impl ItemState {
    pub fn new() -> Self {
        Self {
            statuses: FxHashMap::default(),
            resolved_fn_sigs: FxHashMap::default(),
            check_stack: vec![],
        }
    }

    pub fn get_status(&self, id: &ast::GlobalItemId) -> ItemStatus {
        self.statuses.get(id).copied().unwrap_or(ItemStatus::Unresolved)
    }

    pub fn mark_as_in_progress(&mut self, id: ast::GlobalItemId) -> Result<(), CyclicItemErr> {
        match self.get_status(&id) {
            ItemStatus::Unresolved => {
                self.statuses.insert(id, ItemStatus::InProgress);
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

    pub fn mark_as_resolved(&mut self, id: ast::GlobalItemId) {
        self.statuses.insert(id, ItemStatus::Resolved);
        self.check_stack.pop();
    }

    pub fn get_resolved_fn_sig(&self, id: ast::GlobalItemId) -> Option<&ResolvedFnSig> {
        self.resolved_fn_sigs.get(&id)
    }

    pub fn insert_resolved_fn_sig(&mut self, id: ast::GlobalItemId, resolved_sig: ResolvedFnSig) {
        self.resolved_fn_sigs.insert(id, resolved_sig);
    }
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
