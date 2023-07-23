use ena::unify::InPlaceUnificationTable;

use crate::{ast::*, ty::*};

pub fn typecheck(mut ast: Ast) -> TyResult<Ast> {
    let mut cx = Typecheck {
        unification_table: InPlaceUnificationTable::new(),
    };

    cx.infer(&mut ast);

    Ok(ast)
}

pub type TyResult<T> = Result<T, TyError>;

#[derive(Debug)]
pub enum TyError {
    TyNotEq(Ty, Ty),
    InfiniteTy(TyVar, Ty),
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
                fun.ty = Some(Ty::fun(fun.body.ty_cloned()));

                Constraints::none()
            }
            // Ast::App(fun, arg) => {
            //     let (arg_out, arg_ty) = self.infer(env.clone(), *arg);
            //
            //     let ret_ty = Type::Var(self.fresh_ty_var());
            //     let fun_ty = Type::fun(arg_ty, ret_ty.clone());
            //
            //     // Because we inferred an argument type, we can
            //     // cconstruct a function type to check against.
            //     let fun_out = self.check(env, *fun, fun_ty);
            //
            //     (
            //         GenOut::new(
            //             // Pass on constraints from both child nodes
            //             arg_out
            //                 .constraints
            //                 .into_iter()
            //                 .chain(fun_out.constraints.into_iter())
            //                 .collect(),
            //             Ast::app(fun_out.typed_ast, arg_out.typed_ast),
            //         ),
            //         ret_ty,
            //     )
            // }
            //  Ast::Var(v) => {
            //   let ty = &env[&v];
            //   (
            //     GenOut::new(
            //       vec![],
            //       // Return a `TypedVar` instead of `Var`
            //       Ast::Var(TypedVar(v, ty.clone())
            //     ),
            //     ty.clone(),
            //   )
            // },
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => {
                    lit.ty = Some(Ty::int());
                    Constraints::none()
                }
            },
        }
    }

    fn check(&mut self, ast: &mut Ast, expected_ty: Ty) -> Constraints {
        match (ast, expected_ty) {
            (Ast::Fun(fun), Ty::Fun(fun_ty)) => {
                // let env = env.update(arg, *arg_ty);
                self.check(&mut fun.body, fun_ty.ret.as_ref().clone())
            }
            (
                Ast::Lit(Lit {
                    kind: LitKind::Int(_),
                    ..
                }),
                Ty::Int(IntTy::Int),
            ) => Constraints::none(),
            (ast, expected_ty) => {
                let mut constraints = self.infer(ast);
                constraints.push(Constraint::TyEq(expected_ty, ast.ty_cloned()));
                constraints
            }
        }
    }

    fn unification(&mut self, constraints: Vec<Constraint>) -> Result<(), TyError> {
        for constraint in constraints {
            match constraint {
                Constraint::TyEq(left, right) => self.unify_ty_ty(left, right)?,
            }
        }
        Ok(())
    }

    fn unify_ty_ty(&mut self, unnorm_left: Ty, unnorm_right: Ty) -> Result<(), TyError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);

        match (left, right) {
            (Ty::Fun(l), Ty::Fun(r)) => {
                // self.unify_ty_ty(*f1.arg, f2.arg)?;
                self.unify_ty_ty(l.ret.as_ref().clone(), r.ret.as_ref().clone())
            }

            (Ty::Var(l), Ty::Var(r)) => self
                .unification_table
                .unify_var_var(l, r)
                .map_err(|(l, r)| TyError::TyNotEq(l, r)),

            (Ty::Var(v), ty) | (ty, Ty::Var(v)) => {
                ty.occurs_check(v)
                    .map_err(|ty| TyError::InfiniteTy(v, ty))?;

                self.unification_table
                    .unify_var_value(v, Some(ty))
                    .map_err(|(l, r)| TyError::TyNotEq(l, r))
            }

            (Ty::Int(IntTy::Int), Ty::Int(IntTy::Int)) => Ok(()),

            (left, right) => Err(TyError::TyNotEq(left, right)),
        }
    }

    fn normalize_ty(&mut self, ty: Ty) -> Ty {
        match ty {
            Ty::Fun(fun) => {
                // let arg = self.normalize_ty(*arg);
                let ret = self.normalize_ty(*fun.ret);
                Ty::fun(ret)
            }
            Ty::Var(var) => match self.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => ty,
            },
            Ty::Int(_) => ty,
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
