use ena::unify::InPlaceUnificationTable;

use crate::{
    ast::{Ast, LitKind},
    ty::*,
};

pub fn typecheck(ast: Ast) -> Ast {
    let mut cx = Typecheck {
        unification_table: InPlaceUnificationTable::new(),
    };

    let (inferred, _) = cx.infer(ast);

    inferred.typed_ast
}

struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
}

struct Inferred {
    contraints: Vec<Constraint>,
    typed_ast: Ast,
}

enum Constraint {
    TyEq(Ty, Ty),
}

// TODO: making this mutate the Ast instead for brevity?
impl Typecheck {
    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }

    fn infer(&mut self, ast: Ast) -> (Inferred, Ty) {
        match ast {
            Ast::Fun(fun) => {
                // let arg_ty_var = self.fresh_ty_var();

                let (body_inferred, body_ty) = self.infer(*fun.body);
                let fun_ty = Ty::fun(body_ty, fun.span);

                (
                    Inferred {
                        contraints: vec![],
                        typed_ast: Ast::fun(
                            &fun.name,
                            body_inferred.typed_ast,
                            fun.span,
                            Some(fun_ty.clone()),
                        ),
                    },
                    fun_ty,
                )
                // fun.ty = Some(Ty::fun(fun.body.ty().unwrap()))
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => {
                    let ty = Ty::int(lit.span);

                    (
                        Inferred {
                            contraints: vec![],
                            typed_ast: Ast::int(*value, lit.span, Some(ty.clone())),
                        },
                        ty,
                    )
                }
            },
        }
    }

    fn check(&mut self, ast: Ast, expected: Ty) -> Inferred {
        match ast {
            Ast::Fun(fun) => todo!(),
            Ast::Lit(lit) => todo!(),
        }
    }
}
