mod env;

use std::collections::HashSet;

use ena::unify::InPlaceUnificationTable;
use miette::Diagnostic;
use slotmap::Key;
use thiserror::Error;

use crate::{
    ast::*,
    hir::{self, BindingId, Hir, ModuleId},
    span::{Span, Spanned},
    state::State,
    ty::*,
    util::ErrExt,
    CompilerResult,
};

use self::env::{Env, FunScope};

pub fn check(state: &State, modules: Vec<Module>) -> CompilerResult<hir::Cache> {
    check_inner(modules).map_err(|err| err.with_source_code(state))
}

fn check_inner(mut modules: Vec<Module>) -> CheckResult<hir::Cache> {
    let mut cx = CheckContext::new();

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

    // TODO: generate constraints
    // TODO: unification
    // TODO: substitution

    // let mut constraints = Constraints::none();
    // let mut unbound = HashSet::new();
    //
    // for mut module in modules {
    //     for binding in &module.bindings {
    //         let constr = cx.infer_binding(binding)?;
    //         constraints.extend(constr);
    //     }
    //
    //     // Unification
    //     cx.unification(constraints)?;
    //
    //     for binding in &mut module.bindings {
    //         let (unbound_ty, _binding_ty) = cx.substitute(binding.get_actual_ty().unwrap().clone());
    //         unbound.extend(unbound_ty);
    //
    //         let unbound_binding = cx.substitute_binding(binding);
    //         unbound.extend(unbound_binding);
    //     }
    // }

    Ok(cx.cache)
}

struct CheckContext {
    cache: hir::Cache,
    unification_table: InPlaceUnificationTable<TyVar>,
}

// Utils
impl CheckContext {
    fn new() -> Self {
        Self {
            cache: hir::Cache::new(),
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    fn fresh_ty_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
    }
}

// Infer/Check
impl CheckContext {
    fn infer_ast(&mut self, env: &mut Env, ast: &Ast) -> CheckResult<(Hir, Constraints)> {
        match ast {
            Ast::Binding(binding) => self.infer_binding(env, binding),
            Ast::Fun(_) => todo!(),
            Ast::Ret(ret) => {
                let ty = Ty::never(ret.span);

                if let Some(fun_scope) = env.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    let (value, constraints) = if let Some(value) = ret.value.as_ref() {
                        let (_, value_constraints) = self.infer_ast(env, value)?;
                        let (hir, check_constraints) = self.check(env, value, expected_ty)?;

                        (
                            Some(Box::new(hir)),
                            value_constraints.merge(check_constraints),
                        )
                    } else {
                        (
                            None,
                            Constraints::one(Constraint::TyEq {
                                expected: expected_ty,
                                actual: Ty::unit(ret.span),
                            }),
                        )
                    };

                    Ok((
                        Hir::Ret(hir::Ret {
                            value,
                            span: ret.span,
                            ty,
                        }),
                        constraints,
                    ))
                } else {
                    Err(CheckError::MisplacedReturn { span: ret.span })
                }
            }
            Ast::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => Ok((
                    Hir::Lit(hir::Lit {
                        kind: hir::LitKind::Int(*value),
                        span: lit.span,
                        ty: Ty::int(lit.span),
                    }),
                    Constraints::none(),
                )),
            },
        }
    }

    fn infer_binding(
        &mut self,
        env: &mut Env,
        binding: &Binding,
    ) -> CheckResult<(Hir, Constraints)> {
        let (kind, constraints) = match &binding.kind {
            BindingKind::Fun { name: _, fun } => {
                let (fun, constraints) = self.infer_fun(env, fun)?;
                (hir::BindingKind::Fun(Box::new(fun)), constraints)
            }
        };

        // TODO: patterns
        let span = binding.span;
        let ty = Ty::unit(binding.span);

        let current_module = self.cache.get_module(env.module_id()).unwrap();
        let qualified_name = current_module.name().clone().child(binding.name());

        let id = self.cache.insert_binding_info(hir::BindingInfo {
            id: BindingId::null(),
            qualified_name,
            vis: Vis::Public,
            scope: env.scopes.depth().into(),
            uses: 0,
            ty: kind.ty().clone(),
            span,
        });

        Ok((
            Hir::Binding(hir::Binding { id, kind, span, ty }),
            constraints,
        ))
    }

    fn infer_fun(&mut self, env: &mut Env, fun: &Fun) -> CheckResult<(hir::Fun, Constraints)> {
        // let arg_ty_var = self.fresh_ty_var();

        let fun_ret_ty = self.fresh_ty_var(fun.span);

        env.fun_scopes.push(FunScope {
            ret_ty: fun_ret_ty.clone(),
        });

        let (body, body_constraints) = self.infer_ast(env, &fun.body)?;

        env.fun_scopes.pop();

        let span = body.span();

        Ok((
            hir::Fun {
                kind: hir::FunKind::Orphan {
                    body: hir::Block {
                        statements: vec![body],
                        span,
                        ty: fun_ret_ty.clone(),
                    },
                },
                ty: Ty::fun(fun_ret_ty, fun.span),
                span: fun.span,
            },
            body_constraints,
        ))
    }

    fn check(
        &mut self,
        env: &mut Env,
        ast: &Ast,
        expected_ty: Ty,
    ) -> CheckResult<(Hir, Constraints)> {
        match (ast, &expected_ty.kind) {
            (Ast::Fun(fun), TyKind::Fun(fun_ty)) => {
                self.check(env, &fun.body, fun_ty.ret.as_ref().clone())
            }
            (
                Ast::Lit(Lit {
                    kind: LitKind::Int(value),
                    span,
                }),
                TyKind::Int(IntTy::Int),
            ) => Ok((
                Hir::Lit(hir::Lit {
                    kind: hir::LitKind::Int(*value),
                    span: *span,
                    ty: expected_ty,
                }),
                Constraints::none(),
            )),
            (ast, _) => {
                let (hir, mut constraints) = self.infer_ast(env, ast)?;

                constraints.push(Constraint::TyEq {
                    expected: expected_ty,
                    actual: hir.ty().clone(),
                });

                Ok((hir, constraints))
            }
        }
    }
}

// Unification
// impl CheckContext {
//     fn unification(&mut self, constraints: Constraints) -> CheckResult<()> {
//         for constraint in constraints.0 {
//             match constraint {
//                 Constraint::TyEq { expected, actual } => self.unify_ty_ty(expected, actual)?,
//             }
//         }
//         Ok(())
//     }
//
//     fn unify_ty_ty(&mut self, expected: Ty, actual: Ty) -> CheckResult<()> {
//         let expected = self.normalize_ty(expected);
//         let actual = self.normalize_ty(actual);
//
//         match (&expected.kind, &actual.kind) {
//             (TyKind::Fun(expected), TyKind::Fun(actual)) => {
//                 // self.unify_ty_ty(*f1.arg, f2.arg)?;
//                 self.unify_ty_ty(expected.ret.as_ref().clone(), actual.ret.as_ref().clone())
//             }
//
//             (TyKind::Var(expected), TyKind::Var(actual)) => self
//                 .unification_table
//                 .unify_var_var(*expected, *actual)
//                 .map_err(|(expected, actual)| CheckError::TyNotEq { expected, actual }),
//
//             (TyKind::Var(var), _) => {
//                 actual
//                     .occurs_check(*var)
//                     .map_err(|ty| CheckError::InfiniteTy { var: *var, ty })?;
//
//                 self.unification_table
//                     .unify_var_value(*var, Some(actual))
//                     .map_err(|(expected, actual)| CheckError::TyNotEq { expected, actual })
//             }
//
//             (_, TyKind::Var(var)) => {
//                 expected
//                     .occurs_check(*var)
//                     .map_err(|ty| CheckError::InfiniteTy { var: *var, ty })?;
//
//                 self.unification_table
//                     .unify_var_value(*var, Some(expected))
//                     .map_err(|(expected, actual)| CheckError::TyNotEq { expected, actual })
//             }
//
//             (TyKind::Int(IntTy::Int), TyKind::Int(IntTy::Int)) => Ok(()),
//
//             (_, _) => Err(CheckError::TyNotEq { expected, actual }),
//         }
//     }
//
//     fn normalize_ty(&mut self, ty: Ty) -> Ty {
//         match ty.kind {
//             TyKind::Fun(fun) => {
//                 let ret = self.normalize_ty(*fun.ret);
//                 Ty::fun(ret, ty.span)
//             }
//             TyKind::Var(var) => match self.unification_table.probe_value(var) {
//                 Some(ty) => self.normalize_ty(ty),
//                 None => ty,
//             },
//             TyKind::Int(_) | TyKind::Unit | TyKind::Never => ty,
//         }
//     }
// }

// Substitute
// impl CheckContext {
//     fn substitute(&mut self, ty: Ty) -> (HashSet<TyVar>, Ty) {
//         match ty.kind {
//             TyKind::Fun(fun) => {
//                 // let (mut arg_unbound, arg) = self.substitute(*arg);
//                 let (ret_unbound, ret) = self.substitute(fun.ret.as_ref().clone());
//                 // arg_unbound.extend(ret_unbound);
//                 (ret_unbound, Ty::fun(ret, ty.span))
//             }
//             TyKind::Var(v) => {
//                 let root = self.unification_table.find(v);
//                 match self.unification_table.probe_value(root) {
//                     Some(ty) => self.substitute(ty),
//                     None => {
//                         let mut unbound = HashSet::new();
//                         unbound.insert(root);
//                         (unbound, Ty::var(root, ty.span))
//                     }
//                 }
//             }
//             _ => (HashSet::new(), ty),
//         }
//     }
//
//     fn substitute_hir(&mut self, hir: &mut Hir) -> HashSet<TyVar> {
//         match ast {
//             Ast::Binding(binding) => self.substitute_binding(binding),
//             Ast::Fun(fun) => self.substitute_fun(fun),
//             Ast::Ret(_) | Ast::Lit(_) => {
//                 let (unbound_ty, ty) = self.substitute(ast.ty_cloned());
//                 ast.set_ty(ty);
//                 unbound_ty
//             }
//         }
//     }
//
//     fn substitute_binding(&mut self, binding: &mut Binding) -> HashSet<TyVar> {
//         match &mut binding.kind {
//             BindingKind::Fun { name: _, fun } => self.substitute_fun(fun),
//         }
//     }
//
//     fn substitute_fun(&mut self, fun: &mut Fun) -> HashSet<TyVar> {
//         // let (mut unbound, ty) = self.substitute(arg.1);
//         // let arg = TypedVar(arg.0, ty);
//
//         let unbound_body = self.substitute_hir(&mut fun.body);
//         // unbound.extend(unbound_body);
//
//         let (unbound_ty, ty) = self.substitute(fun.ty_cloned());
//         fun.set_ty(ty);
//
//         unbound_body.union(&unbound_ty).copied().collect()
//     }
// }

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

type CheckResult<T> = CompilerResult<T, CheckError>;

#[derive(Error, Diagnostic, Debug)]
enum CheckError {
    #[error("expected `{expected}`, got `{actual}` instead")]
    #[diagnostic(code(check::incompatible_types))]
    TyNotEq {
        #[label("expected type `{expected}` originates here")]
        expected: Ty,
        #[label("found type `{actual}` here")]
        actual: Ty,
    },
    #[error("type `{ty}` has an infinite size")]
    #[diagnostic(code(check::infinite_type))]
    InfiniteTy {
        #[label]
        ty: Ty,
        var: TyVar,
    },
    #[error("cannot return outside of function scope")]
    #[diagnostic(code(check::infinite_type))]
    MisplacedReturn {
        #[label]
        span: Span,
    },
}

impl Spanned for CheckError {
    fn span(&self) -> Span {
        match self {
            CheckError::TyNotEq { expected, .. } => expected.span,
            CheckError::InfiniteTy { ty, .. } => ty.span,
            CheckError::MisplacedReturn { span } => *span,
        }
    }
}
