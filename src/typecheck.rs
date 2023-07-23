use ena::unify::InPlaceUnificationTable;

use crate::{ast::*, ty::*};

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
                fun.ty = Some(Ty::fun(fun.body.ty_cloned(), fun.span));

                Constraints::none()
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => {
                    lit.ty = Some(Ty::int(lit.span));
                    Constraints::none()
                }
            },
        }
    }

    fn check(&mut self, ast: &mut Ast, expected_ty: Ty) -> Constraints {
        match (ast, &expected_ty.kind) {
            (Ast::Fun(Fun { body, .. }), TyKind::Fun(FunTy { return_ty })) => {
                self.check(body, return_ty.as_ref().clone())
            }
            (
                Ast::Lit(Lit {
                    kind: LitKind::Int(_),
                    ..
                }),
                TyKind::Int(IntTy::Int),
            ) => Constraints::none(),
            (ast, _) => {
                let mut constraints = self.infer(ast);
                constraints.push(Constraint::TyEq(expected_ty, ast.ty_cloned()));
                constraints
            }
        }
    }
}

struct Constraints(Vec<Constraint>);

impl Constraints {
    fn none() -> Self {
        Self(vec![])
    }

    fn push(&mut self, constraint: Constraint) {
        self.0.push(constraint)
    }

    fn merge(self, other: Self) -> Self {
        Self(self.0.into_iter().chain(other.0).collect())
    }
}

enum Constraint {
    TyEq(Ty, Ty),
}
