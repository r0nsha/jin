use indexmap::IndexMap;

use crate::{
    db::{Db, DefId},
    hir::{self, Hir},
    middle::Pat,
    span::{Span, Spanned},
};

pub fn ownck(db: &Db, hir: &mut Hir) {
    for (fn_id, fun) in hir.fns.iter_enumerated() {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                let mut cx = Ownck::new(db);

                cx.env.push_scope();

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
                cx.collect_destroy_ids_from_scope(body.id, &scope);

                hir.fn_destroy_glues.insert(fn_id, cx.destroy_glue);
            }
            hir::FnKind::Extern { .. } => (),
        }
    }

    for let_ in &hir.lets {
        todo!("ownck let_.value")
    }
}

struct Ownck<'db> {
    _db: &'db Db,
    destroy_glue: hir::DestroyGlue,
    env: Env,
}

impl<'db> Ownck<'db> {
    fn new(db: &'db Db) -> Self {
        Self { _db: db, destroy_glue: hir::DestroyGlue::new(), env: Env::new() }
    }

    fn expr(&mut self, expr: &hir::Expr) {
        // TODO: move unary expr
        // TODO: move unary lhs & rhs
        // TODO: move cast
        match &expr.kind {
            hir::ExprKind::Let(_) => todo!("move: let"),
            hir::ExprKind::Assign(_) => todo!("move: assign"),
            hir::ExprKind::If(_) => todo!("move: if cond"),
            hir::ExprKind::Loop(_) => todo!("move: loop"),
            hir::ExprKind::Break => (),
            hir::ExprKind::Block(block) => self.block(expr, block),
            hir::ExprKind::Return(_) => todo!("move: return"),
            hir::ExprKind::Call(call) => {
                self.expr(&call.callee);
                // TODO: mark callee as moved (needed for closures)
                // self.env.mark_moved(call.callee.id);

                for arg in &call.args {
                    self.expr(&arg.expr);
                    self.env.mark_moved(arg.expr.id);
                }

                // Create an owned value for the call's result
                self.env.insert_expr(expr);
            }
            hir::ExprKind::Member(_) => todo!("move: member"),
            hir::ExprKind::Name(_) => {
                // TODO: move: name
            }
            hir::ExprKind::Unary(_)
            | hir::ExprKind::Binary(_)
            | hir::ExprKind::Cast(_)
            | hir::ExprKind::Lit(_) => {
                self.env.insert_expr(expr);
            }
        }
    }

    fn block(&mut self, expr: &hir::Expr, block: &hir::Block) {
        if block.exprs.is_empty() {
            return;
        }

        self.env.push_scope();
        block.exprs.iter().for_each(|expr| self.expr(expr));

        let scope = self.env.pop_scope().unwrap();
        self.collect_destroy_ids_from_scope(expr.id, &scope);
    }

    fn collect_destroy_ids_from_scope(
        &mut self,
        destroy_block_id: hir::DestroyBlockId,
        scope: &Scope,
    ) {
        self.destroy_glue
            .to_destroy
            .entry(destroy_block_id)
            .or_default()
            .extend(
                scope
                    .values
                    .iter()
                    .filter(|(_, value)| value.state == ValueState::Owned)
                    .map(|(item, _)| *item),
            );
    }
}

#[derive(Debug)]
struct Env(Vec<Scope>);

impl Env {
    fn new() -> Self {
        Self(vec![])
    }

    fn push_scope(&mut self) {
        self.0.push(Scope { values: IndexMap::default() });
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.0.pop()
    }

    fn insert_expr(&mut self, expr: &hir::Expr) {
        self.insert(expr.id, expr.span);
    }

    fn insert(&mut self, item: impl Into<hir::DestroyGlueItem>, span: Span) {
        self.current_mut()
            .values
            .insert(item.into(), Value { state: ValueState::Owned, span });
    }

    fn lookup_mut(
        &mut self,
        item: impl Into<hir::DestroyGlueItem>,
    ) -> Option<&mut Value> {
        let key = item.into();
        self.0.iter_mut().find_map(|s| s.values.get_mut(&key))
    }

    #[track_caller]
    fn mark_moved(&mut self, item: impl Into<hir::DestroyGlueItem>) {
        self.lookup_mut(item)
            .expect("tried to mark a non existing value")
            .state = ValueState::Moved;
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.0.last_mut().unwrap()
    }
}

#[derive(Debug)]
struct Scope {
    values: IndexMap<hir::DestroyGlueItem, Value>,
}

#[derive(Debug, Clone, Copy)]
struct Value {
    state: ValueState,
    span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ValueState {
    Owned,
    Moved,
}
