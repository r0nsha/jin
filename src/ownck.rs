use indexmap::IndexSet;
use rustc_hash::FxHashMap;

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{self, Hir},
    middle::Pat,
    span::{Span, Spanned},
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

                self.try_mark_moved(&let_.value);
            }
            hir::ExprKind::Assign(assign) => {
                self.expr(&assign.lhs);
                self.expr(&assign.rhs);
                self.try_mark_moved(&assign.rhs);
            }
            hir::ExprKind::If(_) => todo!("move: if cond"),
            hir::ExprKind::Loop(_) => todo!("move: loop"),
            hir::ExprKind::Break => (),
            hir::ExprKind::Block(block) => self.block(expr, block),
            hir::ExprKind::Return(ret) => {
                self.expr(&ret.expr);
                self.try_mark_moved(&ret.expr);
            }
            hir::ExprKind::Call(call) => {
                self.expr(&call.callee);
                // TODO: mark callee as moved (needed for closures)
                // self.try_mark_expr_moved(&call.callee);

                for arg in &call.args {
                    self.expr(&arg.expr);
                    self.try_mark_moved(&arg.expr);
                }

                // Create an owned value for the call's result
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Member(access) => {
                self.expr(&access.expr);
                self.try_mark_moved_ex(&access.expr, expr.span);
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Name(name) => {
                self.env.insert(name.id, expr.span);
            }
            hir::ExprKind::Unary(un) => {
                self.expr(&un.expr);
                self.try_mark_moved(&un.expr);
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Binary(bin) => {
                self.expr(&bin.lhs);
                self.try_mark_moved(&bin.lhs);

                self.expr(&bin.rhs);
                self.try_mark_moved(&bin.rhs);

                self.env.insert_expr(expr);
            }
            hir::ExprKind::Cast(cast) => {
                self.expr(&cast.expr);
                self.try_mark_moved(&cast.expr);
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
    fn try_mark_moved(&mut self, expr: &hir::Expr) {
        self.try_mark_moved_ex(expr, expr.span);
    }

    #[track_caller]
    fn try_mark_moved_ex(&mut self, expr: &hir::Expr, moved_to: Span) {
        match &expr.kind {
            hir::ExprKind::Name(name) => {
                self.try_mark_moved_inner(name.id, moved_to);
            }
            hir::ExprKind::Block(block) => match block.exprs.last() {
                Some(last) if !expr.ty.is_unit() => {
                    self.try_mark_moved(last);
                }
                _ => {
                    self.try_mark_moved_inner(expr.id, moved_to);
                }
            },
            _ => self.try_mark_moved_inner(expr.id, moved_to),
        }
    }

    #[track_caller]
    fn try_mark_moved_inner(
        &mut self,
        item: impl Into<hir::DestroyGlueItem> + Copy,
        moved_to: Span,
    ) {
        match self.env.mark_moved(item, moved_to) {
            ValueState::Owned => (),
            ValueState::Moved(already_moved_to) => {
                let value_name = match item.into() {
                    hir::DestroyGlueItem::Expr(_) => {
                        "temporary value".to_string()
                    }
                    hir::DestroyGlueItem::Def(id) => {
                        format!("value `{}`", self.db[id].name)
                    }
                };

                self.db.diagnostics.emit(
                    Diagnostic::error()
                        .with_message(format!("use of moved {value_name}"))
                        .with_label(Label::primary(moved_to).with_message(
                            format!("{value_name} used here after being moved"),
                        ))
                        .with_label(
                            Label::secondary(already_moved_to).with_message(
                                format!("{value_name} already moved here"),
                            ),
                        ),
                );
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
    fn mark_moved(
        &mut self,
        item: impl Into<hir::DestroyGlueItem>,
        moved_to: Span,
    ) -> ValueState {
        let current_block_id = self.current_block_id();

        let value = self
            .values
            .get_mut(&item.into())
            .expect("tried to mark a non existing value");

        value.owning_block_id = current_block_id;
        match value.state {
            ValueState::Owned => {
                std::mem::replace(&mut value.state, ValueState::Moved(moved_to))
            }
            ValueState::Moved(_) => value.state,
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

#[derive(Debug, Clone, Copy)]
struct Value {
    owning_block_id: hir::BlockExprId,
    state: ValueState,
    span: Span, // TODO: remove?
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ValueState {
    Owned,
    Moved(Span),
}
