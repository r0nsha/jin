use ena::unify::InPlaceUnificationTable;

use crate::ty::TyVar;

pub fn typecheck() {}

pub struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl Typecheck {
    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }
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
