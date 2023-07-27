mod scope;

use std::collections::HashSet;

use ena::unify::InPlaceUnificationTable;
use miette::Diagnostic;
use thiserror::Error;

use crate::{
    ast::*,
    hir,
    span::{Span, Spanned},
    state::State,
    ty::*,
    util::ErrExt,
    CompilerResult,
};

use self::scope::{FunScope, FunScopes};

pub fn check(state: &State, modules: Vec<Module>) -> CompilerResult<hir::Cache> {
    check_inner(modules).map_err(|err| err.with_source_code(state))
}

fn check_inner(mut modules: Vec<Module>) -> TypeResult<hir::Cache> {
    let mut cx = CheckContext::new();
    let mut cache = hir::Cache::new();

    // TODO: create hir modules
    // for module in modules {
    //     self.resolved_modules
    //         .insert_module(ResolvedModule::from(module));
    // }

    // TODO: create hir for each global binding
    // for module in self.resolved_modules.iter_mut() {
    //     // TODO: add all global names to binding_infos in each module
    // }

    // TODO: resolve all names in all asts
    // for module in self.resolved_modules.iter_mut() {
    //     for binding in &mut module.bindings {
    //         self.resolve_binding(binding);
    //     }
    // }

    let mut constraints = Constraints::none();
    let mut unbound = HashSet::new();

    for mut module in modules {
        for binding in &mut module.bindings {
            let constr = cx.infer_binding(binding)?;
            constraints.extend(constr);
        }

        // Unification
        cx.unification(constraints)?;

        for binding in &mut module.bindings {
            let (unbound_ty, _binding_ty) = cx.substitute(binding.get_actual_ty().unwrap().clone());
            unbound.extend(unbound_ty);

            let unbound_binding = cx.substitute_binding(binding);
            unbound.extend(unbound_binding);
        }
    }

    Ok(cache)
}

struct CheckContext {
    unification_table: InPlaceUnificationTable<TyVar>,
    fun_scopes: FunScopes,
}

// Utils
impl CheckContext {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
            fun_scopes: FunScopes::new(),
        }
    }

    pub fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
    }
}

// Infer/Check
impl CheckContext {
    fn infer(&mut self, ast: &mut Ast) -> TypeResult<Constraints> {
        match ast {
            Ast::Binding(binding) => self.infer_binding(binding),
            Ast::Fun(fun) => self.infer_fun(fun),
            Ast::Ret(ret) => {
                ret.set_ty(Ty::never(ret.span));

                if let Some(fun_scope) = self.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    if let Some(value) = ret.value.as_mut() {
                        let value_constraints = self.infer(value)?;
                        let check_constraints = self.check(value, expected_ty)?;

                        Ok(value_constraints.merge(check_constraints))
                    } else {
                        Ok(Constraints::one(Constraint::TyEq {
                            expected: expected_ty,
                            actual: Ty::unit(ret.span),
                        }))
                    }
                } else {
                    Err(TypeError::MisplacedReturn { span: ret.span })
                }
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(_) => {
                    lit.set_ty(Ty::int(lit.span));
                    Ok(Constraints::none())
                }
            },
        }
    }

    fn infer_binding(&mut self, binding: &mut Binding) -> TypeResult<Constraints> {
        binding.set_ty(Ty::unit(binding.span));

        match &mut binding.kind {
            BindingKind::Fun { name: _, fun } => self.infer_fun(fun),
        }
    }

    fn infer_fun(&mut self, fun: &mut Fun) -> TypeResult<Constraints> {
        // let arg_ty_var = self.fresh_ty_var();

        let fun_ret_ty = self.fresh_ty_var(fun.span);
        fun.set_ty(Ty::fun(fun_ret_ty.clone(), fun.span));

        self.fun_scopes.push(FunScope { ret_ty: fun_ret_ty });
        let body_constraints = self.infer(&mut fun.body);
        self.fun_scopes.pop();

        body_constraints
    }

    fn check(&mut self, ast: &mut Ast, expected_ty: Ty) -> TypeResult<Constraints> {
        match (ast, &expected_ty.kind) {
            (Ast::Fun(fun), TyKind::Fun(fun_ty)) => {
                // let env = env.update(arg, *arg_ty);
                self.check(&mut fun.body, fun_ty.ret.as_ref().clone())
            }
            (
                Ast::Lit(Lit {
                    kind: LitKind::Int(_),
                    ..
                }),
                TyKind::Int(IntTy::Int),
            ) => Ok(Constraints::none()),
            (ast, _) => {
                let mut constraints = self.infer(ast)?;
                constraints.push(Constraint::TyEq {
                    expected: expected_ty,
                    actual: ast.ty_cloned(),
                });
                Ok(constraints)
            }
        }
    }
}

// Unification
impl CheckContext {
    fn unification(&mut self, constraints: Constraints) -> TypeResult<()> {
        for constraint in constraints.0 {
            match constraint {
                Constraint::TyEq { expected, actual } => self.unify_ty_ty(expected, actual)?,
            }
        }
        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: Ty, actual: Ty) -> TypeResult<()> {
        let expected = self.normalize_ty(expected);
        let actual = self.normalize_ty(actual);

        match (&expected.kind, &actual.kind) {
            (TyKind::Fun(expected), TyKind::Fun(actual)) => {
                // self.unify_ty_ty(*f1.arg, f2.arg)?;
                self.unify_ty_ty(expected.ret.as_ref().clone(), actual.ret.as_ref().clone())
            }

            (TyKind::Var(expected), TyKind::Var(actual)) => self
                .unification_table
                .unify_var_var(*expected, *actual)
                .map_err(|(expected, actual)| TypeError::TyNotEq { expected, actual }),

            (TyKind::Var(var), _) => {
                actual
                    .occurs_check(*var)
                    .map_err(|ty| TypeError::InfiniteTy { var: *var, ty })?;

                self.unification_table
                    .unify_var_value(*var, Some(actual))
                    .map_err(|(expected, actual)| TypeError::TyNotEq { expected, actual })
            }

            (_, TyKind::Var(var)) => {
                expected
                    .occurs_check(*var)
                    .map_err(|ty| TypeError::InfiniteTy { var: *var, ty })?;

                self.unification_table
                    .unify_var_value(*var, Some(expected))
                    .map_err(|(expected, actual)| TypeError::TyNotEq { expected, actual })
            }

            (TyKind::Int(IntTy::Int), TyKind::Int(IntTy::Int)) => Ok(()),

            (_, _) => Err(TypeError::TyNotEq { expected, actual }),
        }
    }

    fn normalize_ty(&mut self, ty: Ty) -> Ty {
        match ty.kind {
            TyKind::Fun(fun) => {
                let ret = self.normalize_ty(*fun.ret);
                Ty::fun(ret, ty.span)
            }
            TyKind::Var(var) => match self.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => ty,
            },
            TyKind::Int(_) | TyKind::Unit | TyKind::Never => ty,
        }
    }
}

// Substitute
impl CheckContext {
    fn substitute(&mut self, ty: Ty) -> (HashSet<TyVar>, Ty) {
        match ty.kind {
            TyKind::Fun(fun) => {
                // let (mut arg_unbound, arg) = self.substitute(*arg);
                let (ret_unbound, ret) = self.substitute(fun.ret.as_ref().clone());
                // arg_unbound.extend(ret_unbound);
                (ret_unbound, Ty::fun(ret, ty.span))
            }
            TyKind::Var(v) => {
                let root = self.unification_table.find(v);
                match self.unification_table.probe_value(root) {
                    Some(ty) => self.substitute(ty),
                    None => {
                        let mut unbound = HashSet::new();
                        unbound.insert(root);
                        (unbound, Ty::var(root, ty.span))
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
    TyEq { expected: Ty, actual: Ty },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeScheme {
    unbound: HashSet<TyVar>,
    ty: Ty,
}

type TypeResult<T> = CompilerResult<T, TypeError>;

#[derive(Error, Diagnostic, Debug)]
enum TypeError {
    #[error("expected `{expected}`, got `{actual}` instead")]
    #[diagnostic(code(typeck::incompatible_types))]
    TyNotEq {
        #[label("expected type `{expected}` originates here")]
        expected: Ty,
        #[label("found type `{actual}` here")]
        actual: Ty,
    },
    #[error("type `{ty}` has an infinite size")]
    #[diagnostic(code(typeck::infinite_type))]
    InfiniteTy {
        #[label]
        ty: Ty,
        var: TyVar,
    },
    #[error("cannot return outside of function scope")]
    #[diagnostic(code(typeck::infinite_type))]
    MisplacedReturn {
        #[label]
        span: Span,
    },
}

impl Spanned for TypeError {
    fn span(&self) -> Span {
        match self {
            TypeError::TyNotEq { expected, .. } => expected.span,
            TypeError::InfiniteTy { ty, .. } => ty.span,
            TypeError::MisplacedReturn { span } => *span,
        }
    }
}
