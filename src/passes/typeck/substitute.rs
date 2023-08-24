use std::collections::HashSet;

use crate::{
    passes::typeck::infcx::InferCtxt,
    tast::{
        Bin, Block, Call, CallArg, Expr, Function, FunctionSig, If, Item, ItemKind, Return,
        TypedAst,
    },
    ty::{FunctionType, FunctionTypeParam, InferType, Type, TypeKind, TypeVar, Typed},
};

impl<'db> InferCtxt<'db> {
    pub fn substitution(&mut self, tast: &mut TypedAst) -> HashSet<TypeVar> {
        let mut unbound_vars = HashSet::new();

        for i in 0..self.db.symbols.len() {
            let ty = self.db.symbols[i.into()].ty;
            self.db.symbols[i.into()].ty = self.substitute_ty(ty, &mut unbound_vars);
        }

        for item in &mut tast.items {
            item.substitute(self, &mut unbound_vars);
        }

        unbound_vars
    }

    fn substitute_ty(&mut self, ty: Type, unbound_vars: &mut HashSet<TypeVar>) -> Type {
        Type::new(self.substitute_tykind(&ty, unbound_vars))
    }

    fn substitute_tykind(
        &mut self,
        ty: &TypeKind,
        unbound_vars: &mut HashSet<TypeVar>,
    ) -> TypeKind {
        match ty {
            TypeKind::Function(fun) => TypeKind::Function(FunctionType {
                ret: self.substitute_ty(fun.ret, unbound_vars),
                params: fun
                    .params
                    .iter()
                    .map(|param| FunctionTypeParam {
                        name: param.name,
                        ty: self.substitute_ty(param.ty, unbound_vars),
                    })
                    .collect(),
            }),
            TypeKind::Infer(InferType::TypeVar(var)) => {
                let root = self.ty_unification_table.find(*var);

                if let Some(ty) = self.ty_unification_table.probe_value(root) {
                    self.substitute_tykind(&ty, unbound_vars)
                } else {
                    unbound_vars.insert(root);
                    TypeKind::Infer(InferType::TypeVar(root))
                }
            }
            TypeKind::Infer(InferType::IntVar(var)) => {
                let root = self.int_unification_table.find(*var);

                self.int_unification_table
                    .probe_value(root)
                    .map_or_else(|| TypeKind::DEFAULT_INT, Into::into)
            }
            _ => ty.clone(),
        }
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, cx: &mut InferCtxt<'db>, unbound_vars: &mut HashSet<TypeVar>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        match self {
            Self::Item(inner) => inner.substitute(cx, unbound_vars),
            Self::If(inner) => inner.substitute(cx, unbound_vars),
            Self::Block(inner) => inner.substitute(cx, unbound_vars),
            Self::Return(inner) => inner.substitute(cx, unbound_vars),
            Self::Call(inner) => inner.substitute(cx, unbound_vars),
            Self::Bin(inner) => inner.substitute(cx, unbound_vars),
            Self::Name(_) | Self::Lit(_) => (),
        }

        self.set_ty(cx.substitute_ty(self.ty(), unbound_vars));
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        match &mut self.kind {
            ItemKind::Function(fun) => fun.substitute(cx, unbound_vars),
        }

        self.ty = cx.substitute_ty(self.ty, unbound_vars);
    }
}

impl Substitute<'_> for Function {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.sig.substitute(cx, unbound_vars);
        self.body.substitute(cx, unbound_vars);
        self.ty = cx.substitute_ty(self.ty, unbound_vars);
    }
}

impl Substitute<'_> for FunctionSig {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        for param in &mut self.params {
            param.ty = cx.substitute_ty(param.ty, unbound_vars);
        }
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.cond.substitute(cx, unbound_vars);
        self.then.substitute(cx, unbound_vars);
        self.otherwise.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        for stmt in &mut self.exprs {
            stmt.substitute(cx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.expr.substitute(cx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.callee.substitute(cx, unbound_vars);

        for arg in &mut self.args {
            match arg {
                CallArg::Positional(expr) | CallArg::Named(_, expr) => {
                    expr.substitute(cx, unbound_vars);
                }
            }

            arg.set_ty(cx.substitute_ty(arg.ty(), unbound_vars));
        }
    }
}

impl Substitute<'_> for Bin {
    fn substitute(&mut self, cx: &mut InferCtxt<'_>, unbound_vars: &mut HashSet<TypeVar>) {
        self.lhs.substitute(cx, unbound_vars);
        self.rhs.substitute(cx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, cx: &mut InferCtxt<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        for item in self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, cx: &mut InferCtxt<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        if let Some(item) = self {
            item.substitute(cx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, cx: &mut InferCtxt<'db>, unbound_vars: &mut HashSet<TypeVar>) {
        self.as_mut().substitute(cx, unbound_vars);
    }
}
