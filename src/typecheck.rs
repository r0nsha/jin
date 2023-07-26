mod scope;

use std::collections::HashSet;

use ariadne::{Label, ReportKind};
use ena::unify::InPlaceUnificationTable;

use crate::{
    ast::*,
    diagnostics::{create_report, CompilerReport},
    span::Span,
    ty::*,
    CompilerResult,
};

use self::scope::{FunScope, FunScopes};

pub fn typecheck(mut module: Module) -> CompilerResult<Module> {
    let mut cx = Typecheck::new();

    // Generate constraints
    let mut constraints = Constraints::none();

    for binding in &mut module.bindings {
        let constr = cx.infer_binding(binding)?;
        constraints.extend(constr);
    }

    // Unification
    cx.unification(constraints)?;

    // Substitution
    let mut unbound = HashSet::new();

    for binding in &mut module.bindings {
        let (unbound_ty, _binding_ty) = cx.substitute(binding.get_actual_ty().unwrap().clone());
        unbound.extend(unbound_ty);

        let unbound_binding = cx.substitute_binding(binding);
        unbound.extend(unbound_binding);
    }

    // Return our typed ast and it's type scheme
    Ok(module)
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
    fn infer(&mut self, ast: &mut Ast) -> CompilerResult<Constraints> {
        match ast {
            Ast::Binding(binding) => self.infer_binding(binding),
            Ast::Fun(fun) => self.infer_fun(fun),
            Ast::Ret(ret) => {
                ret.set_ty(Ty::Never);

                if let Some(fun_scope) = self.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    if let Some(value) = ret.value.as_mut() {
                        let value_constraints = self.infer(value)?;
                        let check_constraints = self.check(value, expected_ty)?;

                        Ok(value_constraints.merge(check_constraints))
                    } else {
                        Ok(Constraints::one(Constraint::TyEq {
                            expected: expected_ty,
                            actual: Ty::Unit,
                            span: ret.span,
                        }))
                    }
                } else {
                    Err(create_report(ReportKind::Error, &ret.span)
                        .with_message("cannot return outside of function scope")
                        .with_label(Label::new(ret.span))
                        .finish())
                }
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(_) => {
                    lit.set_ty(Ty::int());
                    Ok(Constraints::none())
                }
            },
        }
    }

    fn infer_binding(&mut self, binding: &mut Binding) -> CompilerResult<Constraints> {
        binding.set_ty(Ty::Unit);

        match &mut binding.kind {
            BindingKind::Fun { name: _, fun } => self.infer_fun(fun),
        }
    }

    fn infer_fun(&mut self, fun: &mut Fun) -> CompilerResult<Constraints> {
        // let arg_ty_var = self.fresh_ty_var();

        let fun_ret_ty = Ty::Unit; // Ty::var(self.fresh_ty_var());
        fun.set_ty(Ty::fun(fun_ret_ty.clone()));

        self.fun_scopes.push(FunScope { ret_ty: fun_ret_ty });
        let body_constraints = self.infer(&mut fun.body);
        self.fun_scopes.pop();

        body_constraints
    }

    fn check(&mut self, ast: &mut Ast, expected_ty: Ty) -> CompilerResult<Constraints> {
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
            ) => Ok(Constraints::none()),
            (ast, expected_ty) => {
                let mut constraints = self.infer(ast)?;
                constraints.push(Constraint::TyEq {
                    expected: expected_ty,
                    actual: ast.ty_cloned(),
                    span: ast.span(),
                });
                Ok(constraints)
            }
        }
    }
}

// Unification
impl Typecheck {
    fn unification(&mut self, constraints: Constraints) -> Result<(), TyError> {
        for constraint in constraints.0 {
            match constraint {
                Constraint::TyEq {
                    expected,
                    actual,
                    span,
                } => self.unify_ty_ty(expected, actual, &span)?,
            }
        }
        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: Ty, actual: Ty, span: &Span) -> Result<(), TyError> {
        let expected = self.normalize_ty(expected);
        let actual = self.normalize_ty(actual);

        match (expected, actual) {
            (Ty::Fun(expected), Ty::Fun(actual)) => {
                // self.unify_ty_ty(*f1.arg, f2.arg)?;
                self.unify_ty_ty(
                    expected.ret.as_ref().clone(),
                    actual.ret.as_ref().clone(),
                    span,
                )
            }

            (Ty::Var(expected), Ty::Var(actual)) => self
                .unification_table
                .unify_var_var(expected, actual)
                .map_err(|(expected, actual)| TyError::TyNotEq {
                    expected,
                    actual,
                    span: *span,
                }),

            (Ty::Var(var), ty) | (ty, Ty::Var(var)) => {
                ty.occurs_check(var).map_err(|ty| TyError::InfiniteTy {
                    var,
                    ty,
                    span: *span,
                })?;

                self.unification_table
                    .unify_var_value(var, Some(ty))
                    .map_err(|(expected, actual)| TyError::TyNotEq {
                        expected,
                        actual,
                        span: *span,
                    })
            }

            (Ty::Int(IntTy::Int), Ty::Int(IntTy::Int)) => Ok(()),

            (expected, actual) => Err(TyError::TyNotEq {
                expected,
                actual,
                span: *span,
            }),
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
        match ast {
            Ast::Binding(binding) => self.substitute_binding(binding),
            Ast::Fun(fun) => self.substitute_fun(fun),
            Ast::Ret(_) | Ast::Lit(_) => {
                let (unbound_ty, ty) = self.substitute(ast.ty_cloned());
                ast.set_ty(ty);
                unbound_ty
            }
        }
    }

    fn substitute_binding(&mut self, binding: &mut Binding) -> HashSet<TyVar> {
        match &mut binding.kind {
            BindingKind::Fun { name: _, fun } => self.substitute_fun(fun),
        }
    }

    fn substitute_fun(&mut self, fun: &mut Fun) -> HashSet<TyVar> {
        // let (mut unbound, ty) = self.substitute(arg.1);
        // let arg = TypedVar(arg.0, ty);

        let unbound_body = self.substitute_ast(&mut fun.body);
        // unbound.extend(unbound_body);

        let (unbound_ty, ty) = self.substitute(fun.ty_cloned());
        fun.set_ty(ty);

        unbound_body.union(&unbound_ty).copied().collect()
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

    fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    fn merge(self, other: Self) -> Self {
        Self(self.0.into_iter().chain(other.0).collect())
    }
}

#[derive(Debug, Clone)]
enum Constraint {
    TyEq {
        expected: Ty,
        actual: Ty,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    unbound: HashSet<TyVar>,
    ty: Ty,
}

#[derive(Debug)]
pub enum TyError {
    TyNotEq {
        expected: Ty,
        actual: Ty,
        span: Span,
    },
    InfiniteTy {
        ty: Ty,
        var: TyVar,
        span: Span,
    },
}

impl From<TyError> for CompilerReport {
    fn from(value: TyError) -> Self {
        match value {
            TyError::TyNotEq {
                expected,
                actual,
                span,
            } => create_report(ReportKind::Error, &span)
                .with_message(format!(
                    "expected type `{expected}`, found `{actual}` instead"
                ))
                .with_label(Label::new(span).with_message(format!("found type `{actual}` here")))
                .finish(),
            TyError::InfiniteTy { span, .. } => create_report(ReportKind::Error, &span)
                .with_message("type has infinite size")
                .with_label(Label::new(span))
                .finish(),
        }
    }
}
