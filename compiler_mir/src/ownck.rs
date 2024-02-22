use std::{fmt, mem};

use compiler_helpers::create_bool_enum;
use itertools::Itertools as _;
use ustr::ustr;

use compiler_core::{
    db::{AdtKind, DefId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    ty::{Ty, TyKind},
};

use crate::{
    lower::{AssignKind, LowerBody, ScopeKind},
    BlockId, FxHashMap, FxHashSet, Inst, Span, ValueId, ValueKind,
};

impl<'cx, 'db> LowerBody<'cx, 'db> {
    pub(super) fn try_move(&mut self, value: ValueId, moved_to: Span) {
        let result = self.try_move_aux(value, moved_to);
        self.emit_result(result);
    }

    fn try_move_aux(&mut self, value: ValueId, moved_to: Span) -> DiagnosticResult<()> {
        // If the value is copy, we don't need to move it.
        // Just check that its parents can be used.
        if !self.value_is_move(value) {
            self.walk_parents(value, |this, parent, _| this.check_if_moved(parent, moved_to))?;

            return Ok(());
        }

        self.check_can_move(value, moved_to)?;
        self.check_if_moved(value, moved_to)?;

        // Mark the value and its fields as moved.
        self.set_moved_with_fields(value, moved_to);

        // Mark its parents (if any) as partially moved
        self.walk_parents(value, |this, parent, child| -> DiagnosticResult<()> {
            this.check_move_out_of_ref(parent, child, moved_to)?;
            this.set_partially_moved(parent, moved_to);
            Ok(())
        })?;

        self.insert_loop_move(value, moved_to);
        self.check_move_out_of_global(value, moved_to)?;

        self.set_destroy_flag(value);

        Ok(())
    }

    pub(super) fn try_use(&mut self, value: ValueId, moved_to: Span) {
        let result = self.check_if_moved(value, moved_to);
        self.emit_result(result);
    }

    fn check_can_move(&mut self, value: ValueId, moved_to: Span) -> DiagnosticResult<()> {
        match self.value_states.cannot_move(value) {
            Some(CannotMove::SliceIndex) => Err(Diagnostic::error(format!(
                "cannot move element of type `{}` out of slice",
                self.ty_of(value).display(self.cx.db)
            ))
            .with_label(Label::primary(moved_to, "cannot move out"))),
            Some(CannotMove::SliceSlice) => {
                Err(Diagnostic::error("slice must be behind a `&` or `&mut` reference")
                    .with_label(Label::primary(moved_to, "cannot move slice")))
            }
            None => Ok(()),
        }
    }

    fn check_if_moved(&mut self, value: ValueId, moved_to: Span) -> DiagnosticResult<()> {
        if !self.value_is_move(value) || self.value_states.cannot_move(value).is_some() {
            return Ok(());
        }

        match self.value_state(value) {
            ValueState::Owned => Ok(()),
            ValueState::Moved(already_moved_to) | ValueState::MaybeMoved(already_moved_to) => {
                Err(self.use_after_move_err(value, moved_to, already_moved_to, "move", "moved"))
            }
            ValueState::PartiallyMoved(already_moved_to) => Err(self.use_after_move_err(
                value,
                moved_to,
                already_moved_to,
                "partial move",
                "partially moved",
            )),
        }
    }

    fn use_after_move_err(
        &self,
        value: ValueId,
        moved_to: Span,
        already_moved_to: Span,
        move_kind: &str,
        past_move_kind: &str,
    ) -> Diagnostic {
        let name = self.value_name(value);

        Diagnostic::error(format!("use of {past_move_kind} {name}"))
            .with_label(Label::primary(moved_to, format!("{name} used here after {move_kind}")))
            .with_label(Label::secondary(
                already_moved_to,
                format!("{name} already {past_move_kind} here"),
            ))
    }

    fn check_move_out_of_global(&self, value: ValueId, moved_to: Span) -> DiagnosticResult<()> {
        if let ValueKind::Global(id) = self.body.value(value).kind {
            let global = &self.cx.mir.globals[id];
            let def = &self.cx.db[global.def_id];

            Err(Diagnostic::error(format!("cannot move out of global item `{}`", def.qpath))
                .with_label(Label::primary(moved_to, "global item moved here")))
        } else {
            Ok(())
        }
    }

    fn check_move_out_of_ref(
        &self,
        parent: ValueId,
        field: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        let parent_ty = self.ty_of(parent);

        if parent_ty.is_ref() {
            Err(Diagnostic::error(format!(
                "cannot move {} out of reference `{}`",
                self.value_name(field),
                parent_ty.display(self.cx.db)
            ))
            .with_label(Label::primary(
                moved_to,
                format!("cannot move out of {}", self.value_name(parent)),
            )))
        } else {
            Ok(())
        }
    }

    fn insert_loop_move(&mut self, value: ValueId, span: Span) {
        let scope = self.scope();

        if scope.loop_depth == 0 || self.value_depth(value) >= scope.loop_depth {
            return;
        }

        if let Some(loop_scope) = self.closest_loop_scope_mut() {
            loop_scope.moved_in.insert(value, span);
        }
    }

    pub(super) fn check_loop_moves(&mut self) {
        let Some(loop_scope) = self.scope_mut().kind.as_loop_mut() else {
            return;
        };

        let moved_in_loop = mem::take(&mut loop_scope.moved_in);

        for (value, moved_to) in moved_in_loop {
            if !matches!(self.value_state(value), ValueState::Owned) {
                let name = self.value_name(value);

                self.cx.diagnostics.push(
                    Diagnostic::error(format!("use of moved {name}"))
                        .with_label(Label::primary(
                            moved_to,
                            format!("{name} moved here, in the previous loop iteration"),
                        ))
                        .with_label(Label::secondary(self.scope().span, "inside this loop")),
                );
            }
        }
    }

    pub(super) fn move_out(&mut self, value: ValueId, moved_to: Span) {
        let result = self.move_out_aux(value, moved_to);
        self.emit_result(result);
    }

    fn move_out_aux(&mut self, value: ValueId, moved_to: Span) -> DiagnosticResult<()> {
        self.check_if_moved(value, moved_to)?;

        let scope = self.scope_mut();
        scope.created_values.shift_remove(&value);
        scope.moved_out.insert(value);

        self.walk_fields(value, |this, field| this.move_out_aux(field, moved_to))
    }

    pub(super) fn value_state(&mut self, value: ValueId) -> ValueState {
        if let Some(state) = self.value_states.get(self.current_block, value).cloned() {
            return state;
        }

        let state = self.solve_value_state(value);
        self.value_states.insert(self.current_block, value, state.clone());
        state
    }

    fn solve_value_state(&self, value: ValueId) -> ValueState {
        let block = self.current_block;

        let mut work: Vec<BlockId> = self.body.block(block).predecessors.iter().copied().collect();
        let mut visited = FxHashSet::from_iter([block]);
        let mut result_state = ValueState::Owned;
        let mut last_move_span: Option<Span> = None;
        let mut is_initial_state = true;

        while let Some(block) = work.pop() {
            visited.insert(block);

            if let Some(state) = self.value_states.get(block, value).cloned() {
                match &state {
                    ValueState::Owned => (),
                    ValueState::Moved(moved_to)
                    | ValueState::MaybeMoved(moved_to)
                    | ValueState::PartiallyMoved(moved_to) => {
                        last_move_span = Some(*moved_to);
                    }
                };

                match &result_state {
                    ValueState::Owned if is_initial_state => {
                        result_state = state;
                        is_initial_state = false;
                    }
                    ValueState::MaybeMoved(_) => break,
                    ValueState::Owned | ValueState::Moved(_) | ValueState::PartiallyMoved(_) => {
                        if result_state != state {
                            result_state = ValueState::MaybeMoved(
                                last_move_span.expect("to have been moved somewhere"),
                            );
                        }
                    }
                }
            } else {
                // Add this block's predecessors, since we need to
                // calculate the value's state for those blocks too
                work.extend(
                    self.body.block(block).predecessors.iter().filter(|b| !visited.contains(b)),
                );
            }
        }

        debug_assert!(
            !is_initial_state,
            "value v{} aka {} (type: {}) is missing a state in block {:?}.",
            value.0,
            self.value_name(value),
            self.ty_of(value).display(self.cx.db),
            block,
        );

        result_state
    }

    pub(super) fn value_is_move(&self, value: ValueId) -> bool {
        self.ty_of(value).is_move(self.cx.db)
    }

    pub(super) fn set_owned(&mut self, value: ValueId) {
        self.set_value_state(value, ValueState::Owned);
    }

    pub(super) fn set_moved(&mut self, value: ValueId, moved_to: Span) {
        self.set_value_state(value, ValueState::Moved(moved_to));
    }

    pub(super) fn set_moved_with_fields(&mut self, value: ValueId, moved_to: Span) {
        self.set_moved(value, moved_to);
        self.walk_fields(value, |this, field| {
            this.set_moved(field, moved_to);
            Ok(())
        })
        .unwrap();
    }

    pub(super) fn set_partially_moved(&mut self, value: ValueId, moved_to: Span) {
        self.set_value_state(value, ValueState::PartiallyMoved(moved_to));
    }

    pub(super) fn set_value_state(&mut self, value: ValueId, state: ValueState) {
        self.value_states.insert(self.current_block, value, state);
    }

    pub(super) fn value_is_moved(&mut self, value: ValueId) -> bool {
        matches!(self.value_state(value), ValueState::Moved(..))
    }

    pub(super) fn value_is_partially_moved(&mut self, value: ValueId) -> bool {
        matches!(self.value_state(value), ValueState::PartiallyMoved(..))
    }

    pub(super) fn destroy_scope_values(&mut self) {
        if !self.in_connected_block() {
            return;
        }

        let span = self.scope().span.tail();

        for idx in (0..self.scope().created_values.len()).rev() {
            let value = self.scope().created_values[idx];
            self.destroy_value(value, span);
        }
    }

    pub(super) fn destroy_loop_values(&mut self, span: Span) {
        let values_to_destroy = self
            .scopes
            .iter()
            .rev()
            .take_while_inclusive(|s| !matches!(s.kind, ScopeKind::Loop(_)))
            .flat_map(|s| s.created_values.iter().rev())
            .copied()
            .collect::<Vec<_>>();

        for value in values_to_destroy {
            self.destroy_value(value, span);
        }
    }

    pub(super) fn destroy_all_values(&mut self, span: Span) {
        let values_to_destroy = self
            .scopes
            .iter()
            .rev()
            .flat_map(|s| s.created_values.iter().rev())
            .copied()
            .collect::<Vec<_>>();

        for value in values_to_destroy {
            self.destroy_value(value, span);
        }
    }

    pub(super) fn destroy_value(&mut self, value: ValueId, span: Span) {
        if !self.needs_destroy(value) {
            return;
        }

        match self.value_state(value) {
            ValueState::Moved(_) => {
                // Value has been moved, don't destroy
            }
            ValueState::MaybeMoved(_) => {
                // Conditional destroy
                let destroy_flag = self.body.destroy_flags[&value];

                let destroy_block = self.body.create_block("destroy");
                let no_destroy_block = self.body.create_block("no_destroy");

                self.ins(self.current_block).brif(
                    destroy_flag,
                    destroy_block,
                    Some(no_destroy_block),
                );

                let destroy_glue = self.needs_destroy_glue(value);

                self.position_at(destroy_block);
                self.destroy_and_set_flag(value, destroy_glue, span);
                self.ins(destroy_block).br(no_destroy_block);

                // Now that the value is destroyed, it has definitely been moved...
                self.position_at(no_destroy_block);
            }
            ValueState::PartiallyMoved { .. } => {
                self.destroy_and_set_flag(value, false, span);
            }
            ValueState::Owned => {
                // Unconditional destroy
                let destroy_glue = self.needs_destroy_glue(value);
                self.destroy_and_set_flag(value, destroy_glue, span);
            }
        }
    }

    pub(super) fn needs_destroy_glue(&self, value: ValueId) -> bool {
        match self.ty_of(value).kind() {
            TyKind::Adt(adt_id, _) => {
                matches!(self.cx.db[*adt_id].kind, AdtKind::Union(_))
            }
            TyKind::Slice(..) | TyKind::Str | TyKind::Param(_) => true,
            _ => false,
        }
    }

    pub(super) fn destroy_fields(&mut self, value: ValueId, span: Span) {
        self.walk_fields(value, |this, field| {
            this.destroy_value(field, span);
            Ok(())
        })
        .unwrap();
    }

    pub(super) fn destroy_value_entirely(&mut self, value: ValueId, span: Span) {
        self.destroy_fields(value, span);
        self.destroy_value(value, span);
    }

    pub(super) fn create_destroy_flag(&mut self, value: ValueId) {
        if !self.needs_destroy(value) {
            return;
        }

        let init = self.const_bool(true);
        let flag = self.push_inst_with_named_register(
            self.cx.db.types.bool,
            ustr("destroy_flag"),
            |value| Inst::StackAlloc { value, init: Some(init) },
        );
        self.body.destroy_flags.insert(value, flag);
    }

    pub(super) fn destroy_and_set_flag(&mut self, value: ValueId, destroy_glue: bool, span: Span) {
        self.ins(self.current_block).destroy(value, destroy_glue, span);
        self.set_destroy_flag(value);
    }

    fn set_destroy_flag(&mut self, value: ValueId) {
        if let Some(destroy_flag) = self.body.destroy_flags.get(&value).copied() {
            let const_false = self.const_bool(false);
            self.ins(self.current_block).store(const_false, destroy_flag);
            self.walk_fields(value, |this, field| {
                this.set_destroy_flag(field);
                Ok(())
            })
            .unwrap();
        }
    }

    pub(super) fn needs_destroy(&self, value: ValueId) -> bool {
        let ty = self.ty_of(value);
        ty.is_ref() || ty.needs_free(self.cx.db)
    }

    pub(super) fn check_slice_assign_mutability(
        &mut self,
        kind: AssignKind,
        slice: ValueId,
        span: Span,
    ) {
        if let Err(root) = self
            .value_imm_root(slice, BreakOnMutRef::Yes)
            .and_then(|()| self.value_ty_imm_root(slice))
        {
            self.emit_assign_mutability_err(kind, slice, span, root);
        }
    }

    pub(super) fn check_assign_mutability(&mut self, kind: AssignKind, lhs: ValueId, span: Span) {
        if let Err(root) = self.value_imm_root(lhs, BreakOnMutRef::Yes) {
            self.emit_assign_mutability_err(kind, lhs, span, root);
        }
    }

    fn emit_assign_mutability_err(
        &mut self,
        kind: AssignKind,
        lhs: ValueId,
        span: Span,
        root: ImmutableRoot,
    ) {
        self.cx.diagnostics.push(self.imm_root_err(
            match kind {
                AssignKind::Assign => "cannot assign to",
                AssignKind::Swap => "cannot swap",
            },
            lhs,
            root,
            span,
        ));
    }

    pub(super) fn check_ref_mutability(&mut self, value: ValueId, span: Span) {
        if let Err(root) = self
            .value_ty_imm_root(value)
            .and_then(|()| self.value_imm_root(value, BreakOnMutRef::No))
        {
            self.cx.diagnostics.push(self.imm_root_err(
                "cannot take &mut reference to",
                value,
                root,
                span,
            ));
        }
    }

    fn imm_root_err(
        &self,
        prefix: &str,
        value: ValueId,
        root: ImmutableRoot,
        span: Span,
    ) -> Diagnostic {
        match root {
            ImmutableRoot::Def(root) => {
                let root_name = self.value_name(root);

                let message = if root == value {
                    format!("{prefix} immutable value {root_name}")
                } else {
                    format!(
                        "{} {}, as {} is not declared as mutable",
                        prefix,
                        self.value_name(value),
                        root_name
                    )
                };

                Diagnostic::error(message)
                    .with_label(Label::primary(span, format!("{prefix} immutable value")))
            }
            ImmutableRoot::Ref(root) => {
                let root_name = self.value_name(root);

                let message = format!(
                    "{} to {}, as {} is behind a `&` reference",
                    prefix,
                    self.value_name(value),
                    root_name,
                );

                Diagnostic::error(message)
                    .with_label(Label::primary(span, format!("{prefix} to immutable reference")))
                    .with_note(format!(
                        "{} is of type `{}`, which is immutable",
                        root_name,
                        self.ty_of(root).display(self.cx.db)
                    ))
            }
        }
    }

    fn value_imm_root(
        &self,
        value: ValueId,
        break_on_mut_ty: BreakOnMutRef,
    ) -> Result<(), ImmutableRoot> {
        match &self.body.value(value).kind {
            ValueKind::Param(id, _) | ValueKind::Local(id) => {
                if self.def_is_imm(*id, self.ty_of(value)) {
                    Err(ImmutableRoot::Def(value))
                } else {
                    Ok(())
                }
            }
            ValueKind::Global(id) => {
                if self.def_is_imm(self.cx.mir.globals[*id].def_id, self.ty_of(value)) {
                    Err(ImmutableRoot::Def(value))
                } else {
                    Ok(())
                }
            }
            ValueKind::Field(parent, _)
            | ValueKind::Variant(parent, _)
            | ValueKind::Deref(parent) => {
                match (self.value_ty_imm_root(*parent), break_on_mut_ty) {
                    (Ok(()), BreakOnMutRef::Yes) => Ok(()),
                    (Ok(()), BreakOnMutRef::No) => self.value_imm_root(*parent, break_on_mut_ty),
                    (Err(err), _) => Err(err),
                }
            }
            ValueKind::Fn(_) | ValueKind::Const(_) | ValueKind::Register(_) => {
                let root = self.value_roots.root_of(value);
                if root != value {
                    self.value_imm_root(root, break_on_mut_ty)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn value_ty_imm_root(&self, value: ValueId) -> Result<(), ImmutableRoot> {
        if self.ty_of(value).is_imm_ref() {
            Err(ImmutableRoot::Ref(value))
        } else {
            Ok(())
        }
    }

    fn def_is_imm(&self, id: DefId, ty: Ty) -> bool {
        self.cx.db[id].mutability.is_imm() && !ty.is_mut_ref()
    }
}

#[derive(Debug)]
pub(super) struct ValueStates {
    states: FxHashMap<BlockId, BlockState>,
    #[allow(clippy::zero_sized_map_values)]
    cannot_move: FxHashMap<ValueId, CannotMove>,
}

impl ValueStates {
    pub(super) fn new() -> Self {
        #[allow(clippy::zero_sized_map_values)]
        Self { states: FxHashMap::default(), cannot_move: FxHashMap::default() }
    }

    pub(super) fn get(&self, block: BlockId, value: ValueId) -> Option<&ValueState> {
        self.states.get(&block).and_then(|b| b.states.get(&value))
    }

    pub(super) fn insert(&mut self, block: BlockId, value: ValueId, state: ValueState) {
        self.states.entry(block).or_default().states.insert(value, state);
    }

    pub(super) fn set_cannot_move(&mut self, value: ValueId, kind: CannotMove) {
        self.cannot_move.insert(value, kind);
    }

    pub(super) fn cannot_move(&mut self, value: ValueId) -> Option<&CannotMove> {
        self.cannot_move.get(&value)
    }
}

impl Default for ValueStates {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct BlockState {
    states: FxHashMap<ValueId, ValueState>,
}

impl BlockState {
    fn new() -> Self {
        Self { states: FxHashMap::default() }
    }
}

impl Default for BlockState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(super) enum CannotMove {
    SliceIndex,
    SliceSlice,
}

impl fmt::Display for ValueStates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (block, block_state) in &self.states {
            writeln!(f, "b{}:\n{}", block.0, block_state)?;
        }

        Ok(())
    }
}

impl fmt::Display for BlockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (value, state) in &self.states {
            writeln!(
                f,
                "- v{} : {}",
                value.0,
                match state {
                    ValueState::Owned => "owned",
                    ValueState::Moved(_) => "moved",
                    ValueState::MaybeMoved(_) => "maybe moved",
                    ValueState::PartiallyMoved(_) => "partially moved",
                }
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum ValueState {
    /// The value is owned, and should be dropped at the end of its scope
    Owned,

    /// The value has been moved. It shouldn't be dropped in its scope.
    Moved(Span),

    /// The value has been moved in one branch, but is still
    /// owned in another branch. This value should be dropped conditionally at
    /// the end of its scope
    MaybeMoved(Span),

    // Some of this value's fields have been moved, and the parent value is considered as moved.
    // The parent value should be destroyed in its scope.
    PartiallyMoved(Span),
}

#[derive(Debug, Clone, Copy)]
enum ImmutableRoot {
    Def(ValueId),
    Ref(ValueId),
}

create_bool_enum!(BreakOnMutRef);
