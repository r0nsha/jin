mod constraint;
mod error;
mod substitute;
mod type_env;
mod typecx;
mod unify;

use std::collections::HashSet;

use crate::{
    db::{Database, TypeId},
    diagnostics::Diagnostic,
    hir::*,
    ty::{Type, TypeKind},
};

use self::{
    constraint::{Constraint, Constraints},
    type_env::{FunScope, TypeEnv},
    typecx::TypeCx,
};

pub(crate) fn infer(db: &mut Database, modules: &mut [Module]) -> Result<(), Diagnostic> {
    let mut cx = InferCx::new(db);

    // TODO: generate constraints
    let constraints = cx.infer(modules);
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

    Ok(())
}

pub(super) struct InferCx<'a> {
    pub(super) db: &'a mut Database,
    pub(super) typecx: TypeCx,
}

impl<'a> InferCx<'a> {
    fn new(db: &mut Database) -> Self {
        Self {
            db,
            typecx: TypeCx::new(),
        }
    }
}

impl<'a> InferCx<'a> {
    fn infer(&mut self, modules: &mut [Module]) -> Constraints {
        let mut constraints = Constraints::none();

        for module in modules {
            constraints.extend(self.infer_module(module));
        }

        constraints
    }

    fn infer_module(&mut self, module: &mut Module) -> Constraints {
        let mut constraints = Constraints::none();

        let mut env = TypeEnv::new(module.id);

        for binding in &mut module.bindings {
            constraints.extend(self.infer_binding(&mut env, binding));
        }

        constraints
    }

    fn infer_hir(
        &mut self,
        env: &mut TypeEnv,
        hir: &Hir,
        expected_ty: Option<TypeId>,
    ) -> Constraints {
        match hir {
            Hir::Ret(ret) => {
                let ty = Type::never(ret.span);

                if let Some(fun_scope) = env.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    if let Some(value) = ret.value.as_ref() {
                        self.infer_hir(env, value, Some(expected_ty))
                    } else {
                        Constraints::one(Constraint::TypeEq {
                            expected: expected_ty,
                            actual: Type::alloc(&mut self.db, TypeKind::Unit, ret.span),
                        })
                    }
                } else {
                    // Err(CheckError::MisplacedReturn { span: ret.span });
                    todo!()
                }
            }
            Hir::Lit(lit) => match &lit.kind {
                LitKind::Int(value) => Ok((
                    Hir::Const(hir::Const {
                        kind: hir::ConstKind::Int(*value),
                        span: lit.span,
                        ty: Type::int(lit.span),
                    }),
                    Constraints::none(),
                )),
            },
        }
    }

    fn infer_binding(&mut self, env: &mut TypeEnv, binding: &Binding) -> Constraints {
        // let (kind, constraints) = match &binding.kind {
        //     BindingKind::Fun { name: _, fun } => {
        //         let (fun, constraints) = self.infer_fun(env, fun)?;
        //         (hir::BindingKind::Fun(Box::new(fun)), constraints)
        //     }
        // };

        // TODO: patterns
        // let span = binding.span;
        // let ty = Type::unit(binding.span);

        todo!();
        // Ok((
        //     Hir::Binding(hir::Binding { id, kind, span, ty }),
        //     constraints,
        // ))
    }

    fn infer_fun(&mut self, env: &mut TypeEnv, fun: &Fun) -> CheckResult<(hir::Fun, Constraints)> {
        // let arg_ty_var = self.fresh_type_var();

        let fun_ret_ty = self.typecx.fresh_type_var(fun.span);

        env.fun_scopes.push(FunScope {
            ret_ty: fun_ret_ty.clone(),
        });

        let (body, body_constraints) = self.infer_hir(env, &fun.body)?;

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
                ty: Type::fun(fun_ret_ty, fun.span),
                span: fun.span,
            },
            body_constraints,
        ))
    }
}
