use ena::unify::InPlaceUnificationTable;

use crate::{ast::Ast, ty::*};

pub fn typecheck() {}

struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl Typecheck {
    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }

    fn infer(&mut self, ast: Ast) -> (Inferred, Ty) {
        match ast {
            Ast::Fun(_) => todo!(),
            Ast::Lit(_) => todo!(),
        }
    }

    fn check(&mut self, ast: Ast, expected: Ty) -> Inferred {
        match ast {
            Ast::Fun(_) => todo!(),
            Ast::Lit(_) => todo!(),
        }
    }
}

struct Inferred {
    contraints: Vec<Constraint>,
    typed_ast: Ast,
}

enum Constraint {
    TyEq(TyKind, TyKind),
}

#[cfg(test)]
mod test {
    use crate::ast::Ast;

    use super::typecheck;

    #[test]
    fn empty_main() {
        let ast = Ast::fun("main", Ast::int(42));
        // assert_eq!()
    }
}
