use crate::{
    db::Db,
    hir::{self, Hir},
};

pub fn ownck(db: &mut Db, hir: &mut Hir) {
    for fun in &hir.fns {
        match &fun.kind {
            hir::FnKind::Bare { body } => {
                Ownck::new(db).expr(body);
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
    db: &'db mut Db,
    scopes: Scopes,
}

impl<'db> Ownck<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, scopes: Scopes::new() }
    }

    fn expr(&mut self, expr: &hir::Expr) {
        todo!("expr")
    }
}

#[derive(Debug)]
pub struct Scopes(Vec<Scope>);

impl Scopes {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn with_scope<R>(&mut self, mut f: impl FnMut(&mut Self) -> R) -> R {
        self.0.push(Scope { value_states: vec![] });
        let res = f(self);
        self.0.pop();
        res
    }

    pub fn create_owned(&mut self) {
        todo!("create owned value state")
    }

    pub fn lookup(&self) -> Option<ValueState> {
        todo!("iter in reverse")
    }
}

#[derive(Debug)]
pub struct Scope {
    pub value_states: Vec<ValueState>,
}

#[derive(Debug)]
pub enum ValueState {
    Owned,
    Moved,
}
