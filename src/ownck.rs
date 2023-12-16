mod errors;

use indexmap::IndexSet;
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    db::Db,
    hir::{self, Hir},
    middle::Pat,
    span::{Span, Spanned},
    word::Word,
};

pub fn ownck(db: &mut Db, hir: &mut Hir) {
    for fun in &hir.fns {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                let mut cx = Ownck::new(db);

                cx.env.push_scope(body.id);

                for p in &fun.sig.params {
                    match &p.pat {
                        Pat::Name(name) => {
                            cx.env.insert(name.id, name.span());
                        }
                        Pat::Discard(_) => (),
                    }
                }

                cx.expr(body);

                let scope = cx.env.pop_scope().unwrap();
                cx.collect_destroy_ids_from_scope(&scope);

                hir.fn_destroy_glues.insert(fun.id, cx.destroy_glue);
            }
            hir::FnKind::Extern { .. } => (),
        }
    }

    for let_ in &hir.lets {
        let mut cx = Ownck::new(db);
        cx.expr(&let_.value);
        hir.let_destroy_glues.insert(let_.id, cx.destroy_glue);
    }
}

struct Ownck<'db> {
    db: &'db mut Db,
    destroy_glue: hir::DestroyGlue,
    env: Env,
}

impl<'db> Ownck<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, destroy_glue: hir::DestroyGlue::new(), env: Env::new() }
    }

    fn expr(&mut self, expr: &hir::Expr) {
        self.expr_inner(expr, OwnckKind::Value);
    }

    fn place(&mut self, expr: &hir::Expr) {
        self.expr_inner(expr, OwnckKind::Place);
    }

    fn expr_inner(&mut self, expr: &hir::Expr, kind: OwnckKind) {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                self.expr(&let_.value);

                match &let_.pat {
                    Pat::Name(name) => {
                        // Create an owned value for the let's name
                        self.env.insert(name.id, name.span());
                    }
                    Pat::Discard(_) => (),
                }

                self.try_move(&let_.value);
            }
            hir::ExprKind::Assign(assign) => {
                self.place(&assign.lhs);
                self.expr(&assign.rhs);
                self.try_move(&assign.rhs);
            }
            hir::ExprKind::If(if_) => {
                self.expr(&if_.cond);
                self.try_move(&if_.cond);
                self.expr(&if_.then);
                self.expr(&if_.otherwise);
            }
            hir::ExprKind::Loop(loop_) => {
                if let Some(cond) = &loop_.cond {
                    self.expr(cond);
                    self.try_move(cond);
                }
                self.expr(&loop_.expr);
            }
            hir::ExprKind::Break => (),
            hir::ExprKind::Block(block) => self.block(expr, block),
            hir::ExprKind::Return(ret) => {
                self.expr(&ret.expr);
                self.try_move(&ret.expr);
            }
            hir::ExprKind::Call(call) => {
                self.expr(&call.callee);
                // TODO: mark callee as moved (needed for closures)
                // self.try_mark_expr_moved(&call.callee);

                for arg in &call.args {
                    self.expr(&arg.expr);
                    self.try_move(&arg.expr);
                }

                // Create an owned value for the call's result
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Member(access) => {
                self.expr(&access.expr);
                if kind != OwnckKind::Place {
                    self.try_partial_move(&access.expr, access.member);
                }
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Name(name) => {
                self.env.insert(name.id, expr.span);
            }
            hir::ExprKind::Unary(un) => {
                self.expr(&un.expr);
                self.try_move(&un.expr);
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Binary(bin) => {
                self.expr(&bin.lhs);
                self.try_move(&bin.lhs);

                self.expr(&bin.rhs);
                self.try_move(&bin.rhs);

                self.env.insert_expr(expr);
            }
            hir::ExprKind::Cast(cast) => {
                self.expr(&cast.expr);
                self.try_move(&cast.expr);
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Lit(_) => self.env.insert_expr(expr),
        }
    }

    fn block(&mut self, expr: &hir::Expr, block: &hir::Block) {
        if expr.ty.is_unit() && self.env.current_block_id() != expr.id {
            self.env.insert_expr(expr);
        }

        if block.exprs.is_empty() {
            return;
        }

        self.env.push_scope(expr.id);

        for expr in &block.exprs {
            self.expr(expr);
        }

        let scope = self.env.pop_scope().unwrap();
        self.collect_destroy_ids_from_scope(&scope);
    }

    fn collect_destroy_ids_from_scope(&mut self, scope: &Scope) {
        self.destroy_glue.to_destroy.entry(scope.block_id).or_default().extend(
            scope.items.iter().filter_map(|item| {
                let value = &self.env.values[item];

                if value.state == ValueState::Owned
                    && value.owning_block_id == scope.block_id
                {
                    Some(*item)
                } else {
                    None
                }
            }),
        );
    }

    #[track_caller]
    fn try_partial_move(&mut self, expr: &hir::Expr, member: Word) {
        self.try_move_expr(expr, &MoveKind::PartialMove(member));
    }

    #[track_caller]
    fn try_move(&mut self, expr: &hir::Expr) {
        self.try_move_expr(expr, &MoveKind::Move(expr.span));
    }

    #[track_caller]
    fn try_move_expr(&mut self, expr: &hir::Expr, kind: &MoveKind) {
        match &expr.kind {
            hir::ExprKind::Name(name) => {
                self.try_move_inner(name.id, kind);
            }
            hir::ExprKind::Block(block) => match block.exprs.last() {
                Some(last) if !expr.ty.is_unit() => {
                    self.try_move_expr(last, kind);
                }
                _ => {
                    self.try_move_inner(expr.id, kind);
                }
            },
            _ => self.try_move_inner(expr.id, kind),
        }
    }

    #[track_caller]
    fn try_move_inner(
        &mut self,
        item: impl Into<hir::DestroyGlueItem> + Copy,
        kind: &MoveKind,
    ) {
        match self.env.try_move(item, kind) {
            Ok(()) => (),
            Err(err) => {
                let item = item.into();
                let moved_to = kind.moved_to();

                let diag = match err {
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
                };

                self.db.diagnostics.emit(diag);
            }
        }
    }
}

#[derive(Debug)]
struct Env {
    values: FxHashMap<hir::DestroyGlueItem, Value>,
    scopes: Vec<Scope>,
}

impl Env {
    fn new() -> Self {
        Self { values: FxHashMap::default(), scopes: vec![] }
    }

    fn push_scope(&mut self, block_id: hir::BlockExprId) {
        self.scopes.push(Scope { block_id, items: IndexSet::default() });
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
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

    #[track_caller]
    fn try_move(
        &mut self,
        item: impl Into<hir::DestroyGlueItem>,
        kind: &MoveKind,
    ) -> Result<(), MoveError> {
        let current_block_id = self.current_block_id();

        let value = self
            .values
            .get_mut(&item.into())
            .expect("tried to mark a non existing value");

        value.owning_block_id = current_block_id;

        match &mut value.state {
            ValueState::Owned => {
                match kind {
                    MoveKind::Move(moved_to) => {
                        value.state = ValueState::Moved(*moved_to);
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
            ValueState::Moved(moved_to) => {
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

    fn current(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn current_block_id(&self) -> hir::BlockExprId {
        self.current().block_id
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
}

#[derive(Debug)]
struct Scope {
    block_id: hir::BlockExprId,
    items: IndexSet<hir::DestroyGlueItem>,
}

#[derive(Debug, Clone)]
struct Value {
    owning_block_id: hir::BlockExprId,
    state: ValueState,
    #[allow(unused)]
    span: Span, // TODO: remove?
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
    Moved(Span),
    PartiallyMoved(FxHashMap<Ustr, Span>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum OwnckKind {
    Value,
    Place,
}
