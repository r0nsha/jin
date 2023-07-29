use ena::unify::InPlaceUnificationTable;

use crate::{span::Span, ty::*};

pub struct TypeContext {
    unification_table: InPlaceUnificationTable<TyVar>,
}

impl TypeContext {
    pub fn new() -> Self {
        Self {
            unification_table: InPlaceUnificationTable::new(),
        }
    }

    pub fn fresh_var(&mut self, span: Span) -> Ty {
        Ty::var(self.unification_table.new_key(None), span)
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
