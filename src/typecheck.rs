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

    fn infer(&mut self, ast: Ast) -> (Inferred, Ty) {}

    fn check(&mut self, ast: Ast, expected: Ty) -> Inferred {}
}

struct Inferred {
    contraints: Vec<Constraint>,
    typed_ast: Ast,
}

enum Constraint {
    TyEq(Ty, Ty),
}

#[cfg(test)]
mod test {
    use crate::ast::Ast;

    use super::typecheck;

    #[test]
    fn empty_main() {
        let ast = Ast::fun("main", Ast::int(42));
        // assert_eq!(f(), 0);
    }
}
