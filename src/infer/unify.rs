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
