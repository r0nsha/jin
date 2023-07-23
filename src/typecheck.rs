use ena::unify::InPlaceUnificationTable;

use crate::{
    ast::{Ast, LitKind},
    ty::*,
};

pub fn typecheck(ast: &mut Ast) {
    let mut cx = Typecheck {
        unification_table: InPlaceUnificationTable::new(),
    };

    cx.infer(ast);
}

struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl Typecheck {
    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }

    fn infer(&mut self, ast: &mut Ast) -> Constraints {
        match ast {
            Ast::Fun(fun) => {
                // let arg_ty_var = self.fresh_ty_var();

                let body_constraints = self.infer(&mut fun.body);
                fun.ty = Some(Ty::fun(fun.body.ty().unwrap().clone(), fun.span));

                Constraints(vec![])
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => {
                    lit.ty = Some(Ty::int(lit.span));
                    Constraints(vec![])
                }
            },
        }
    }

    fn check(&mut self, ast: &mut Ast, expected: Ty) -> Constraints {
        match ast {
            Ast::Fun(fun) => todo!(),
            Ast::Lit(lit) => todo!(),
        }
    }
}

struct Constraints(Vec<Constraint>);

impl Constraints {
    fn merge(self, other: Self) -> Self {
        Self(self.0.into_iter().chain(other.0).collect())
    }
}

enum Constraint {
    TyEq(Ty, Ty),
}
