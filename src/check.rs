mod env;
mod error;
mod resolve;
mod type_context;

use std::collections::HashSet;

use ustr::Ustr;

use crate::{
    ast::*,
    hir::{self, Hir},
    span::{Span, Spanned},
    state::State,
    ty::*,
    CompilerResult,
};

use self::{
    env::{Env, FunScope},
    error::CheckError,
    type_context::TypeContext,
};

pub fn check(state: &State, modules: Vec<Module>) -> CompilerResult<hir::Cache> {
    let mut cx = CheckContext::new();

    resolve::create_modules(&mut cx, modules)?;

    // TODO: infer all global bindings
    // cache.insert_global_binding(hir::Binding {
    //     id: todo!(),
    //     kind: todo!(),
    //     span: todo!(),
    //     ty: todo!(),
    // });

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

pub(super) struct CheckContext {
    pub(super) cache: hir::Cache,
    pub(super) global_scope: resolve::GlobalScope,
    pub(super) typecx: TypeContext,
}

// Utils
impl CheckContext {
    fn new() -> Self {
        Self {
            cache: hir::Cache::new(),
            global_scope: resolve::GlobalScope::new(),
            typecx: TypeContext::new(),
        }
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
                    Hir::Const(hir::Const {
                        kind: hir::ConstKind::Int(*value),
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
        // let (kind, constraints) = match &binding.kind {
        //     BindingKind::Fun { name: _, fun } => {
        //         let (fun, constraints) = self.infer_fun(env, fun)?;
        //         (hir::BindingKind::Fun(Box::new(fun)), constraints)
        //     }
        // };

        // TODO: patterns
        // let span = binding.span;
        // let ty = Ty::unit(binding.span);

        todo!();
        // Ok((
        //     Hir::Binding(hir::Binding { id, kind, span, ty }),
        //     constraints,
        // ))
    }

    fn infer_fun(&mut self, env: &mut Env, fun: &Fun) -> CheckResult<(hir::Fun, Constraints)> {
        // let arg_ty_var = self.fresh_ty_var();

        let fun_ret_ty = self.typecx.fresh_var(fun.span);

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
                Hir::Const(hir::Const {
                    kind: hir::ConstKind::Int(*value),
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

type CheckResult<T> = Result<T, CheckError>;
