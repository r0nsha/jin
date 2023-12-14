use crate::{
    db::Db,
    hir::{self, Hir, HirMap},
    span::Span,
};

pub fn ownck(db: &Db, hir: &Hir) {
    for fun in &hir.fns {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                let mut cx = Ownck::new(db);
                cx.expr(&mut Env::new(), body);
                todo!("{:?}", cx.destroy_glue);
            }
            hir::FnKind::Extern { .. } => (),
        }
    }

    for let_ in &hir.lets {
        todo!("ownck let_.value")
    }
}

struct Ownck<'db> {
    db: &'db Db,
    destroy_glue: hir::DestroyGlue,
}

impl<'db> Ownck<'db> {
    fn new(db: &'db Db) -> Self {
        Self { db, destroy_glue: hir::DestroyGlue::new() }
    }

    fn expr(&mut self, env: &mut Env, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Let(_) => todo!(),
            hir::ExprKind::Assign(_) => todo!(),
            hir::ExprKind::If(_) => todo!(),
            hir::ExprKind::Loop(_) => todo!(),
            hir::ExprKind::Break => todo!(),
            hir::ExprKind::Block(block) => {
                env.push_scope();
                block.exprs.iter().for_each(|expr| self.expr(env, expr));

                let scope = env.pop_scope().unwrap();
                let destroy_block_id = expr.id;

                self.destroy_glue.exprs_to_destroy.insert(
                    destroy_block_id,
                    scope
                        .expr_states
                        .iter()
                        .filter(|(_, value)| value.state == ValueState::Owned)
                        .map(|(expr_id, _)| *expr_id)
                        .collect(),
                );
            }
            hir::ExprKind::Return(_) => todo!(),
            hir::ExprKind::Call(call) => {
                self.expr(env, &call.callee);
                call.args.iter().for_each(|arg| self.expr(env, &arg.expr));
            }
            hir::ExprKind::Member(_) => todo!(),
            hir::ExprKind::Name(_) => (),
            hir::ExprKind::Unary(_)
            | hir::ExprKind::Binary(_)
            | hir::ExprKind::Cast(_)
            | hir::ExprKind::Lit(_) => {
                env.create_owned(expr);
            }
        }
    }
}

#[derive(Debug)]
struct Env(Vec<Scope>);

impl Env {
    fn new() -> Self {
        Self(vec![])
    }

    fn push_scope(&mut self) {
        self.0.push(Scope { expr_states: HirMap::default() });
    }

    fn pop_scope(&mut self) -> Option<Scope> {
        self.0.pop()
    }

    fn create_owned(&mut self, expr: &hir::Expr) {
        self.current_mut().expr_states.insert(
            expr.id,
            Value { state: ValueState::Owned, span: expr.span },
        );
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.0.last_mut().unwrap()
    }
}

#[derive(Debug)]
struct Scope {
    expr_states: HirMap<Value>,
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
