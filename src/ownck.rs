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
                let mut env = Env::new();

                env.push_scope();

                for p in &fun.sig.params {
                    match &p.pat {
                        Pat::Name(name) => {
                            env.create_def_value(name.id, name.span());
                        }
                        Pat::Discard(_) => (),
                    }
                }

                cx.expr(&mut env, body);

                let scope = env.pop_scope().unwrap();
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
}

impl<'db> Ownck<'db> {
    fn new(db: &'db Db) -> Self {
        Self { _db: db, destroy_glue: hir::DestroyGlue::new() }
    }

    fn expr(&mut self, env: &mut Env, expr: &hir::Expr) {
        // TODO: move unary expr
        // TODO: move unary lhs & rhs
        // TODO: move cast
        match &expr.kind {
            hir::ExprKind::Let(_) => todo!("move: let"),
            hir::ExprKind::Assign(_) => todo!("move: assign"),
            hir::ExprKind::If(_) => todo!("move: if cond"),
            hir::ExprKind::Loop(_) => todo!("move: loop"),
            hir::ExprKind::Break => (),
            hir::ExprKind::Block(block) => self.block(env, expr, block),
            hir::ExprKind::Return(_) => todo!("move: return"),
            hir::ExprKind::Call(call) => {
                self.expr(env, &call.callee);
                call.args.iter().for_each(|arg| self.expr(env, &arg.expr));
                env.create_expr_value(expr);
            }
            hir::ExprKind::Member(_) => todo!("move: member"),
            hir::ExprKind::Name(_) => {
                // TODO: move: name
            }
            hir::ExprKind::Unary(_)
            | hir::ExprKind::Binary(_)
            | hir::ExprKind::Cast(_)
            | hir::ExprKind::Lit(_) => {
                env.create_expr_value(expr);
            }
        }
    }

    fn block(&mut self, env: &mut Env, expr: &hir::Expr, block: &hir::Block) {
        if block.exprs.is_empty() {
            return;
        }

        env.push_scope();
        block.exprs.iter().for_each(|expr| self.expr(env, expr));

        let scope = env.pop_scope().unwrap();
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

    fn create_expr_value(&mut self, expr: &hir::Expr) {
        self.current_mut().values.insert(
            hir::DestroyGlueItem::Expr(expr.id),
            Value { state: ValueState::Owned, span: expr.span },
        );
    }

    fn create_def_value(&mut self, id: DefId, span: Span) {
        self.current_mut().values.insert(
            hir::DestroyGlueItem::Def(id),
            Value { state: ValueState::Owned, span },
        );
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
