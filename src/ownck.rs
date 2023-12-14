use crate::{
    db::Db,
    hir::{self, Hir, HirMap},
    span::Span,
};

pub fn ownck(db: &Db, hir: &mut Hir) {
    for fun in &hir.fns {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                Ownck::new(db).expr(&mut Env::new(), body);
                todo!("collect into destroy_glue")
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
}

impl<'db> Ownck<'db> {
    fn new(db: &'db Db) -> Self {
        Self { db }
    }

    fn expr(&mut self, env: &mut Env, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Let(_) => todo!(),
            hir::ExprKind::Assign(_) => todo!(),
            hir::ExprKind::If(_) => todo!(),
            hir::ExprKind::Loop(_) => todo!(),
            hir::ExprKind::Break => todo!(),
            hir::ExprKind::Block(block) => env.with(|env| {
                block.exprs.iter().for_each(|expr| self.expr(env, expr));
            }),
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

    fn with<R>(&mut self, mut f: impl FnMut(&mut Self) -> R) -> R {
        self.0.push(Scope { expr_states: HirMap::default() });
        let res = f(self);
        self.0.pop();
        res
    }

    fn create_owned(&mut self, expr: &hir::Expr) {
        self.current_mut().expr_states.insert(
            expr.id,
            Value { state: ValueState::Owned, span: expr.span },
        );
    }

    fn lookup(&self) -> Option<Value> {
        todo!("iter in reverse")
    }

    fn current_mut(&mut self) -> &mut Scope {
        self.0.last_mut().unwrap()
    }
}

#[derive(Debug)]
struct Scope {
    expr_states: HirMap<Value>,
}

#[derive(Debug)]
struct Value {
    state: ValueState,
    span: Span,
}

#[derive(Debug)]
enum ValueState {
    Owned,
    Moved,
}
