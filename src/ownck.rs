mod errors;

use indexmap::IndexSet;
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    db::{Db, DefId, DefKind},
    diagnostics::Diagnostic,
    hir::{self, Hir},
    middle::Pat,
    span::{Span, Spanned},
    ty::{Ty, TyKind},
    word::Word,
};

pub fn ownck(db: &mut Db, hir: &mut Hir) {
    for fun in &hir.fns {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                let mut cx = Ownck::new(db);
                let mut env = Env::new();

                let scope =
                    env.with(body.id, ScopeKind::Block, body.span, |env| {
                        for p in &fun.sig.params {
                            match &p.pat {
                                Pat::Name(name) => {
                                    env.insert(name.id, name.span());
                                }
                                Pat::Discard(_) => (),
                            }
                        }

                        cx.block(env, body);
                        cx.try_move(env, body);
                    });

                cx.destroy_scopes.push(scope);
                hir.fn_destroy_glues.insert(fun.id, cx.into_destroy_glue(&env));
            }
            hir::FnKind::Extern { .. } => (),
        }
    }

    for let_ in &hir.lets {
        let mut cx = Ownck::new(db);
        let mut env = Env::new();

        cx.expr(&mut env, &let_.value);

        hir.let_destroy_glues.insert(let_.id, cx.into_destroy_glue(&env));
    }
}

struct Ownck<'db> {
    db: &'db mut Db,
    destroy_scopes: Vec<Scope>,
}

impl<'db> Ownck<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, destroy_scopes: vec![] }
    }

    fn into_destroy_glue(self, env: &Env) -> hir::DestroyGlue {
        let mut glue = hir::DestroyGlue::new();

        for scope in self.destroy_scopes {
            glue.to_destroy.entry(scope.block_id).or_default().extend(
                scope.items.into_iter().filter(|item| {
                    let value = &env.values[item];

                    if value.owning_block_id != scope.block_id {
                        return false;
                    }

                    match &value.state {
                        ValueState::Owned => true,
                        ValueState::Moved {
                            span: _,
                            is_conditional: true,
                            to_block,
                        } => {
                            glue.needs_destroy_flag.insert(*item, *to_block);
                            true
                        }
                        _ => false,
                    }
                }),
            );
        }

        glue
    }

    fn expr(&mut self, env: &mut Env, expr: &hir::Expr) {
        self.expr_inner(env, expr, OwnckKind::Value);
    }

    fn place(&mut self, env: &mut Env, expr: &hir::Expr) {
        self.expr_inner(env, expr, OwnckKind::Place);
    }

    fn expr_inner(&mut self, env: &mut Env, expr: &hir::Expr, kind: OwnckKind) {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                self.expr(env, &let_.value);

                match &let_.pat {
                    Pat::Name(name) => {
                        // Create an owned value for the let's name
                        env.insert(name.id, name.span());
                    }
                    Pat::Discard(_) => (),
                }

                self.try_move(env, &let_.value);
            }
            hir::ExprKind::Assign(assign) => {
                self.place(env, &assign.lhs);
                self.expr(env, &assign.rhs);
                self.try_move(env, &assign.rhs);
            }
            hir::ExprKind::If(if_) => {
                self.expr(env, &if_.cond);
                self.try_move(env, &if_.cond);

                // We create a new env for the `then` case, so that
                // ownership can be moved into each of the branches without
                // colliding with each other
                let mut then_env = env.clone();
                self.block_with_scope(
                    &mut then_env,
                    &if_.then,
                    ScopeKind::Branch,
                );

                self.block_with_scope(env, &if_.otherwise, ScopeKind::Branch);

                // After checking the separate envs, we merge them
                // by extending the original env
                env.extend(then_env);
            }
            hir::ExprKind::Loop(loop_) => {
                if let Some(cond) = &loop_.cond {
                    self.expr(env, cond);
                    self.try_move(env, cond);
                }

                self.block_with_scope(env, &loop_.expr, ScopeKind::Loop);
            }
            hir::ExprKind::Break => (),
            hir::ExprKind::Block(_) => {
                self.block_with_scope(env, expr, ScopeKind::Block);
            }
            hir::ExprKind::Return(ret) => {
                self.expr(env, &ret.expr);
                self.try_move(env, &ret.expr);
            }
            hir::ExprKind::Call(call) => {
                self.expr(env, &call.callee);
                // TODO: mark callee as moved (needed for closures)
                // self.try_mark_expr_moved(&call.callee);

                for arg in &call.args {
                    self.expr(env, &arg.expr);
                    self.try_move(env, &arg.expr);
                }

                // Create an owned value for the call's result
                env.insert_expr(expr);
            }
            hir::ExprKind::Member(access) => {
                self.expr(env, &access.expr);
                if kind != OwnckKind::Place {
                    self.try_partial_move(
                        env,
                        &access.expr,
                        access.member,
                        expr.ty,
                    );
                }
                env.insert_expr(expr);
            }
            hir::ExprKind::Name(name) => {
                env.insert(name.id, expr.span);
            }
            hir::ExprKind::Unary(un) => {
                self.expr(env, &un.expr);
                self.try_move(env, &un.expr);
                env.insert_expr(expr);
            }
            hir::ExprKind::Binary(bin) => {
                self.expr(env, &bin.lhs);
                self.try_move(env, &bin.lhs);

                self.expr(env, &bin.rhs);
                self.try_move(env, &bin.rhs);

                env.insert_expr(expr);
            }
            hir::ExprKind::Cast(cast) => {
                self.expr(env, &cast.expr);
                self.try_move(env, &cast.expr);
                env.insert_expr(expr);
            }
            hir::ExprKind::Lit(_) => env.insert_expr(expr),
        }
    }

    fn block_with_scope(
        &mut self,
        env: &mut Env,
        expr: &hir::Expr,
        scope_kind: ScopeKind,
    ) {
        let scope = env
            .with(expr.id, scope_kind, expr.span, |env| self.block(env, expr));
        self.destroy_scopes.push(scope);
    }

    fn block(&mut self, env: &mut Env, expr: &hir::Expr) {
        if expr.ty.is_unit() && env.current_block_id() != expr.id {
            env.insert_expr(expr);
        }

        let block = expr.kind.as_block().unwrap();

        if block.exprs.is_empty() {
            return;
        }

        for expr in &block.exprs {
            self.expr(env, expr);
        }
    }

    fn try_partial_move(
        &mut self,
        env: &mut Env,
        expr: &hir::Expr,
        member: Word,
        member_ty: Ty,
    ) {
        if !self.ty_is_move(member_ty) {
            return;
        }

        if let Err(diagnostic) =
            self.try_move_expr(env, expr, &MoveKind::PartialMove(member))
        {
            self.db.diagnostics.emit(diagnostic);
        }
    }

    fn try_move(&mut self, env: &mut Env, expr: &hir::Expr) {
        if let Err(diagnostic) =
            self.try_move_expr(env, expr, &MoveKind::Move(expr.span))
        {
            self.db.diagnostics.emit(diagnostic);
        }
    }

    fn try_move_expr(
        &mut self,
        env: &mut Env,
        expr: &hir::Expr,
        kind: &MoveKind,
    ) -> Result<(), Diagnostic> {
        match &expr.kind {
            hir::ExprKind::Name(name) => self.try_move_item(
                env,
                hir::DestroyGlueItem::Def(name.id),
                kind,
                expr.ty,
            ),
            hir::ExprKind::Block(block) => match block.exprs.last() {
                Some(last) if !expr.ty.is_unit() => {
                    self.try_move_expr(env, last, kind)
                }
                _ => self.try_move_item(
                    env,
                    hir::DestroyGlueItem::Expr(expr.id),
                    kind,
                    expr.ty,
                ),
            },
            _ => self.try_move_item(
                env,
                hir::DestroyGlueItem::Expr(expr.id),
                kind,
                expr.ty,
            ),
        }
    }

    fn try_move_item(
        &mut self,
        env: &mut Env,
        item: hir::DestroyGlueItem,
        kind: &MoveKind,
        ty: Ty,
    ) -> Result<(), Diagnostic> {
        if !self.ty_is_move(ty) {
            return Ok(());
        }

        env.try_move(self.db, item, kind).map_err(|err| {
            let moved_to = kind.moved_to();

            match err {
                MoveError::AlreadyMoved(already_moved_to) => {
                    errors::already_moved(
                        self.db,
                        item,
                        moved_to,
                        already_moved_to,
                    )
                }
                MoveError::AlreadyPartiallyMoved(already_moved_member) => {
                    errors::already_partially_moved(
                        self.db,
                        item,
                        moved_to,
                        already_moved_member,
                    )
                }
                MoveError::MoveAfterPartiallyMoved(already_moved_to) => {
                    errors::move_after_partially_moved(
                        self.db,
                        item,
                        moved_to,
                        already_moved_to,
                    )
                }
                MoveError::MoveOutOfGlobal(id) => {
                    errors::move_global_item(self.db, id, kind.moved_to())
                }
                MoveError::MoveIntoLoop { value_span, loop_span } => {
                    errors::move_into_loop(
                        self.db,
                        item,
                        kind.moved_to(),
                        value_span,
                        loop_span,
                    )
                }
            }
        })
    }

    fn ty_is_move(&self, ty: Ty) -> bool {
        match ty.kind() {
            TyKind::Struct(sid) => self.db[*sid].kind.is_ref(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct Env {
    values: FxHashMap<hir::DestroyGlueItem, Value>,
    scopes: Vec<Scope>,
}

impl Env {
    fn new() -> Self {
        Self { values: FxHashMap::default(), scopes: vec![] }
    }

    #[must_use]
    fn with(
        &mut self,
        block_id: hir::BlockExprId,
        kind: ScopeKind,
        span: Span,
        mut f: impl FnMut(&mut Self),
    ) -> Scope {
        self.scopes.push(Scope {
            block_id,
            kind,
            depth: self.scopes.len(),
            span,
            items: IndexSet::default(),
        });

        f(self);

        self.scopes.pop().unwrap()
    }

    fn insert_expr(&mut self, expr: &hir::Expr) {
        self.insert(expr.id, expr.span);
    }

    fn insert(&mut self, item: impl Into<hir::DestroyGlueItem>, span: Span) {
        let item = item.into();
        let current_block_id = self.current_block_id();

        if self.values.contains_key(&item) {
            assert!(
                !matches!(item, hir::DestroyGlueItem::Expr(_)),
                "inserting an Expr item twice doesn't make since"
            );
            return;
        }

        self.current_mut().items.insert(item);
        self.values.insert(
            item,
            Value {
                owning_block_id: current_block_id,
                state: ValueState::Owned,
                span,
            },
        );
    }

    fn extend(&mut self, other: Env) {
        self.values.extend(other.values);
    }

    fn try_move(
        &mut self,
        db: &Db,
        item: hir::DestroyGlueItem,
        kind: &MoveKind,
    ) -> Result<(), MoveError> {
        if let hir::DestroyGlueItem::Def(id) = item {
            if let DefKind::Global | DefKind::ExternGlobal =
                db[id].kind.as_ref()
            {
                return Err(MoveError::MoveOutOfGlobal(id));
            }
        }

        let current_block_id = self.current_block_id();

        let value = self.value(&item);

        if let Some(loop_scope) =
            self.is_value_moved_into_scope(value, ScopeKind::Loop)
        {
            return Err(MoveError::MoveIntoLoop {
                value_span: value.span,
                loop_span: loop_scope.span,
            });
        }

        let is_conditional =
            self.is_value_moved_into_scope(value, ScopeKind::Branch).is_some();

        let value = self.value_mut(&item);

        match &mut value.state {
            ValueState::Owned => {
                match kind {
                    MoveKind::Move(moved_to) => {
                        if !is_conditional {
                            value.owning_block_id = current_block_id;
                        }

                        value.state = ValueState::Moved {
                            span: *moved_to,
                            to_block: current_block_id,
                            is_conditional,
                        };
                    }
                    MoveKind::PartialMove(member) => {
                        value.state =
                            ValueState::PartiallyMoved(FxHashMap::from_iter([
                                (member.name(), member.span()),
                            ]));
                    }
                }

                Ok(())
            }
            ValueState::Moved { span: moved_to, .. } => {
                Err(MoveError::AlreadyMoved(*moved_to))
            }
            ValueState::PartiallyMoved(moved_members) => match kind {
                MoveKind::Move(_) => Err(MoveError::MoveAfterPartiallyMoved(
                    moved_members.values().copied().collect(),
                )),
                MoveKind::PartialMove(member) => {
                    if let Some(moved_to) =
                        moved_members.insert(member.name(), member.span())
                    {
                        Err(MoveError::AlreadyPartiallyMoved(Word::new(
                            member.name(),
                            moved_to,
                        )))
                    } else {
                        Ok(())
                    }
                }
            },
        }
    }

    fn is_value_moved_into_scope(
        &self,
        value: &Value,
        scope_kind: ScopeKind,
    ) -> Option<&Scope> {
        let value_scope_depth = self.find_value_scope(value).depth;

        self.scopes
            .iter()
            .rev()
            .find(|s| s.kind == scope_kind && s.depth > value_scope_depth)
    }

    #[track_caller]
    fn value(&self, item: &hir::DestroyGlueItem) -> &Value {
        self.values.get(item).unwrap()
    }

    #[track_caller]
    fn value_mut(&mut self, item: &hir::DestroyGlueItem) -> &mut Value {
        self.values.get_mut(item).unwrap()
    }

    fn current(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn current_block_id(&self) -> hir::BlockExprId {
        self.current().block_id
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn find_value_scope(&self, value: &Value) -> &Scope {
        self.scopes
            .iter()
            .find(|s| s.block_id == value.owning_block_id)
            .expect("value's scope must exist while value exists")
    }
}

#[derive(Debug, Clone)]
struct Scope {
    block_id: hir::BlockExprId,
    kind: ScopeKind,
    depth: usize,
    span: Span,
    items: IndexSet<hir::DestroyGlueItem>,
}

#[derive(Debug, Clone)]
struct Value {
    owning_block_id: hir::BlockExprId,
    state: ValueState,
    span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ScopeKind {
    Block,
    Branch,
    Loop,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum MoveKind {
    Move(Span),
    PartialMove(Word),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone)]
enum MoveError {
    AlreadyMoved(Span),
    AlreadyPartiallyMoved(Word),
    MoveAfterPartiallyMoved(Vec<Span>),
    MoveOutOfGlobal(DefId),
    MoveIntoLoop { value_span: Span, loop_span: Span },
}

impl MoveKind {
    fn moved_to(&self) -> Span {
        match self {
            MoveKind::Move(moved_to) => *moved_to,
            MoveKind::PartialMove(member) => member.span(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ValueState {
    Owned,
    Moved { span: Span, to_block: hir::BlockExprId, is_conditional: bool },
    PartiallyMoved(FxHashMap<Ustr, Span>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum OwnckKind {
    Value,
    Place,
}
