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
    ty::{FunType, IntType, Type, TypeKind},
};

use self::{
    constraint::{Constraint, Constraints},
    type_env::{FunScope, TypeEnv},
    typecx::TypeCx,
};

pub(crate) fn infer(db: &mut Database, modules: &mut [Module]) -> Result<(), Diagnostic> {
    let mut cx = InferCx::new(db);

    // TODO: generate constraints
    let constraints = cx.infer_all(modules);

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
    fn infer_all(&mut self, modules: &mut [Module]) -> Constraints {
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
            constraints.extend(binding.infer(self, &mut env, None));
        }

        constraints
    }
}

trait Infer<'a> {
    fn infer(
        &mut self,
        cx: &mut InferCx<'a>,
        env: &mut TypeEnv,
        expected_ty: Option<TypeId>,
    ) -> Constraints;
}

impl Infer<'_> for Hir {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        env: &mut TypeEnv,
        expected_ty: Option<TypeId>,
    ) -> Constraints {
        match self {
            Hir::Ret(ret) => ret.infer(cx, env, expected_ty),
            Hir::Const(r#const) => r#const.infer(cx, env, expected_ty),
        }
    }
}

impl Infer<'_> for Binding {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        env: &mut TypeEnv,
        _expected_ty: Option<TypeId>,
    ) -> Constraints {
        self.ty = Type::alloc(&mut cx.db, TypeKind::Unit, self.span);

        let constraints = match &mut self.kind {
            BindingKind::Fun(fun) => fun.infer(cx, env, None),
        };

        self.id.get_mut(&mut cx.db).ty = self.kind.ty();

        constraints
    }
}

impl Infer<'_> for Fun {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        env: &mut TypeEnv,
        _expected_ty: Option<TypeId>,
    ) -> Constraints {
        let ret_ty = cx.typecx.fresh_type_var(&mut cx.db, self.span);

        let fun_ty = TypeKind::Fun(FunType {
            ret: Box::new(ret_ty.get(&cx.db).clone()),
        });

        let ty = Type::alloc(&mut cx.db, fun_ty, self.span);

        self.ty = ty;
        self.id.get_mut(&mut cx.db).ty = ty;

        env.fun_scopes.push(FunScope {
            id: self.id,
            ret_ty,
        });

        let body_constraints = self.body.infer(cx, env, Some(ret_ty));

        env.fun_scopes.pop();

        body_constraints
    }
}

impl Infer<'_> for Block {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        env: &mut TypeEnv,
        expected_ty: Option<TypeId>,
    ) -> Constraints {
        let mut constraints = Constraints::none();

        let last_index = self.statements.len() - 1;

        for (i, stmt) in self.statements.iter_mut().enumerate() {
            let expected_ty = if i == last_index { expected_ty } else { None };
            constraints.extend(stmt.infer(cx, env, expected_ty));
        }

        self.ty = self.statements.last().map_or_else(
            || Type::alloc(&mut cx.db, TypeKind::Unit, self.span),
            |stmt| stmt.ty(),
        );

        constraints
    }
}

impl Infer<'_> for Ret {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        env: &mut TypeEnv,
        _expected_ty: Option<TypeId>,
    ) -> Constraints {
        self.ty = Type::alloc(&mut cx.db, TypeKind::Never, self.span);

        if let Some(fun_scope) = env.fun_scopes.current() {
            let ret_ty = fun_scope.ret_ty;

            if let Some(value) = self.value.as_mut() {
                let constraints = value.infer(cx, env, Some(ret_ty));
                constraints.merge(Constraints::one(Constraint::TypeEq {
                    expected: ret_ty,
                    actual: value.ty(),
                }))
            } else {
                Constraints::one(Constraint::TypeEq {
                    expected: ret_ty,
                    actual: Type::alloc(&mut cx.db, TypeKind::Unit, self.span),
                })
            }
        } else {
            // Err(CheckError::MisplacedReturn { span: ret.span });
            todo!()
        }
    }
}

impl Infer<'_> for Const {
    fn infer(
        &mut self,
        cx: &mut InferCx<'_>,
        _env: &mut TypeEnv,
        _expected_ty: Option<TypeId>,
    ) -> Constraints {
        let (constraints, ty) = match &self.kind {
            ConstKind::Int(_) => (
                Constraints::none(),
                // TODO: use a polymorphic int
                Type::alloc(&mut cx.db, TypeKind::Int(IntType::Int), self.span),
            ),
            ConstKind::Unit => (
                Constraints::none(),
                Type::alloc(&mut cx.db, TypeKind::Unit, self.span),
            ),
        };

        self.ty = ty;

        constraints
    }
}
