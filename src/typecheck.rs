use ena::unify::InPlaceUnificationTable;

use crate::{ast::Ast, ty::*};

pub fn typecheck(ast: &mut Ast) {}

struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl Typecheck {
    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }

    fn infer(&mut self, ast: &mut Ast) -> (Inferred, Ty) {
        match ast {
            Ast::Fun(_) => todo!(),
            Ast::Lit(_) => todo!(),
        }
    }

    fn check(&mut self, ast: &mut Ast, expected: Ty) -> Inferred {
        match ast {
            Ast::Fun(_) => todo!(),
            Ast::Lit(_) => todo!(),
        }
    }
}

struct Inferred {
    contraints: Vec<Constraint>,
}

enum Constraint {
    TyEq(Ty, Ty),
}
