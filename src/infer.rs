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

    dbg!(constraints);
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
    fn new(db: &'a mut Database) -> Self {
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

    fn infer_binding(&mut self, env: &mut TypeEnv, binding: &mut Binding) -> Constraints {
        match &mut binding.kind {
            BindingKind::Fun(fun) => self.infer_fun(env, fun),
        }
    }

    fn infer_hir(
        &mut self,
        env: &mut TypeEnv,
        hir: &mut Hir,
        expected_ty: Option<TypeId>,
    ) -> Constraints {
        match hir {
            Hir::Ret(ret) => {
                ret.ty = Type::alloc(&mut self.db, TypeKind::Never, ret.span);

                if let Some(fun_scope) = env.fun_scopes.current() {
                    let expected_ty = fun_scope.ret_ty.clone();

                    if let Some(value) = ret.value.as_mut() {
                        let constraints = self.infer_hir(env, value, Some(expected_ty));
                        constraints.merge(Constraints::one(Constraint::TypeEq {
                            expected: expected_ty,
                            actual: value.ty(),
                        }))
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
            Hir::Const(r#const) => match &r#const.kind {
                ConstKind::Int(_) | ConstKind::Unit => Constraints::none(),
            },
        }
    }

    fn infer_fun(&mut self, env: &mut TypeEnv, fun: &mut Fun) -> Constraints {
        let ret_ty = self.typecx.fresh_type_var(&mut self.db, fun.span);

        env.fun_scopes.push(FunScope { id: fun.id, ret_ty });
        let body_constraints = self.infer_block(env, &mut fun.body, Some(ret_ty));
        env.fun_scopes.pop();

        body_constraints
    }

    fn infer_block(
        &mut self,
        env: &mut TypeEnv,
        block: &mut Block,
        expected_ty: Option<TypeId>,
    ) -> Constraints {
        let mut constraints = Constraints::none();

        let last_index = block.statements.len() - 1;

        for (i, stmt) in block.statements.iter_mut().enumerate() {
            let expected_ty = if i == last_index { expected_ty } else { None };
            constraints.extend(self.infer_hir(env, stmt, expected_ty));
        }

        block.ty = block.statements.last().map_or_else(
            || Type::alloc(&mut self.db, TypeKind::Unit, block.span),
            |stmt| stmt.ty(),
        );

        constraints
    }
}
