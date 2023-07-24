mod scope;

use std::collections::HashSet;

use ena::unify::InPlaceUnificationTable;

use crate::{ast::*, ty::*};

use self::scope::{FunScope, FunScopes};

pub fn typecheck(mut module: Module) -> TyResult<(Module, TypeScheme)> {
    let mut cx = Typecheck::new();

    let mut constraints = Constraints::none();
    let mut unbound = HashSet::new();

    for binding in &mut module.bindings {
        let constraints = cx.infer_fun(&mut fun);
        unbound.extend(cx.substitute(fun.ty_cloned()));
    }

    cx.unification(constraints)?;

    let (mut unbound, ty) = cx.substitute(module.ty_cloned());
    let unbound_ast = cx.substitute_ast(&mut module);

    unbound.extend(unbound_ast);

    // Return our typed ast and it's type scheme
    Ok((module, TypeScheme { unbound, ty }))
}

pub type TyResult<T> = Result<T, TyError>;

#[derive(Debug)]
pub enum TyError {
    TyNotEq(Ty, Ty),
    InfiniteTy(TyVar, Ty),
}

struct Typecheck {
    unification_table: InPlaceUnificationTable<TyVar>,
    fun_scopes: FunScopes,
}

// Utils
impl Typecheck {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            fun_scopes: FunScopes::new(),
        }
    }

    pub fn fresh_ty_var(&mut self) -> TyVar {
        self.unification_table.new_key(None)
    }
}

// Infer/Check
impl Typecheck {
    fn infer(&mut self, ast: &mut Ast) -> Constraints {
        match ast {
            Ast::Fun(fun) => self.infer_fun(fun),
            Ast::Ret(ret) => {
                ret.set_ty(Ty::Never);

                if let Some(fun_scope) = self.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    if let Some(value) = ret.value.as_mut() {
                        let value_constraints = self.infer(value);
                        let check_constraints = self.check(value, expected_ty);

                        value_constraints.merge(check_constraints)
                    } else {
                        Constraints::one(Constraint::TyEq(expected_ty, Ty::Unit))
                    }
                } else {
                    // TODO: diagnostic
                    todo!("cannot use return outside of function scope")
                }
            }
            // Ast::App(fun, arg) => {
            //     let (arg_out, arg_ty) = self.infer(env.clone(), *arg);
            //
            //     let ret_ty = Ty::Var(self.fresh_ty_var());
            //     let fun_ty = Ty::fun(arg_ty, ret_ty.clone());
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
                LitKind::Int(_) => {
                    lit.set_ty(Ty::int());
                    Constraints::none()
                }
            },
        }
    }

    fn infer_fun(&mut self, fun: &mut Fun) -> Constraints {
        // let arg_ty_var = self.fresh_ty_var();

        let fun_ret_ty = Ty::var(self.fresh_ty_var());
        fun.set_ty(Ty::fun(fun_ret_ty.clone()));

        self.fun_scopes.push(FunScope { ret_ty: fun_ret_ty });
        let body_constraints = self.infer(&mut fun.body);
        self.fun_scopes.pop();

        body_constraints
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
}

// Unification
impl Typecheck {
    fn unification(&mut self, constraints: Constraints) -> Result<(), TyError> {
        for constraint in constraints.0 {
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
            Ty::Int(_) | Ty::Unit | Ty::Never => ty,
        }
    }
}

// Substitute
impl Typecheck {
    fn substitute(&mut self, ty: Ty) -> (HashSet<TyVar>, Ty) {
        match ty {
            Ty::Fun(fun) => {
                // let (mut arg_unbound, arg) = self.substitute(*arg);
                let (ret_unbound, ret) = self.substitute(fun.ret.as_ref().clone());
                // arg_unbound.extend(ret_unbound);
                (ret_unbound, Ty::fun(ret))
            }
            Ty::Var(v) => {
                let root = self.unification_table.find(v);
                match self.unification_table.probe_value(root) {
                    Some(ty) => self.substitute(ty),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        (unbound, Ty::Var(root))
                    }
                }
            }
            _ => (HashSet::new(), ty),
        }
    }

    fn substitute_ast(&mut self, ast: &mut Ast) -> HashSet<TyVar> {
        let unbound = match ast {
            Ast::Fun(fun) => {
                // let (mut unbound, ty) = self.substitute(arg.1);
                // let arg = TypedVar(arg.0, ty);

                let unbound_body = self.substitute_ast(&mut fun.body);
                // unbound.extend(unbound_body);

                unbound_body
            }
            // Ast::App(fun, arg) => {
            //     let (mut unbound_fun, fun) = self.substitute_ast(*fun);
            //     let (unbound_arg, arg) = self.substitute_ast(*arg);
            //     unbound_fun.extend(unbound_arg);
            //     (unbound_fun, Expr::app(fun, arg))
            // }
            // Ast::Var(v) => {
            //     let (unbound, ty) = self.substitute(v.1);
            //     (unbound, Ast::Var(TypedVar(v.0, ty)))
            // }
            Ast::Ret(_) | Ast::Lit(_) => HashSet::new(),
        };

        let (unbound_ty, ty) = self.substitute(ast.ty_cloned());
        *ast.ty_mut() = Some(ty);

        unbound
    }
}

#[derive(Debug, Clone)]
struct Constraints(Vec<Constraint>);

impl Constraints {
    fn none() -> Self {
        Self(vec![])
    }

    fn one(c: Constraint) -> Self {
        Self(vec![c])
    }

    fn push(&mut self, constraint: Constraint) {
        self.0.push(constraint)
    }

    fn merge(self, other: Self) -> Self {
        Self(self.0.into_iter().chain(other.0).collect())
    }
}

#[derive(Debug, Clone)]
enum Constraint {
    TyEq(Ty, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    unbound: HashSet<TyVar>,
    ty: Ty,
}
