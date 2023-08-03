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
