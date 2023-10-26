use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;

use crate::{ast, db::DefId, hir};

pub struct ItemState {
    statuses: ast::ItemMap<ItemStatus>,
    resolved_ids: ast::ItemMap<ItemResolvedId>,
    check_stack: Vec<ast::GlobalItemId>,
}

impl ItemState {
    pub fn new() -> Self {
        Self {
            statuses: FxHashMap::default(),
            resolved_ids: FxHashMap::default(),
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

    pub fn get_resolved_id(&self, id: ast::GlobalItemId) -> Option<&ItemResolvedId> {
        self.resolved_ids.get(&id)
    }
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum ItemStatus {
    Unresolved,
    InProgress,
    Resolved,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ItemResolvedId {
    DefId(DefId),
    Pat(hir::Pat),
}

#[derive(Debug, Clone, Copy)]
pub struct CyclicItemErr {
    /// The item that is actually cyclic and referenced by `causee`
    pub origin: ast::GlobalItemId,
    /// The item that caused the cycle to happen, and references `origin`
    pub causee: ast::GlobalItemId,
}
