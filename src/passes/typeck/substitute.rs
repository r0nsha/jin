use std::collections::HashSet;

use crate::{
    passes::typeck::infcx::{InferCtxt, InferCtxtInner},
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
            self.db.symbols[i.into()].ty =
                substitute_ty(&mut self.inner.borrow_mut(), ty, &mut unbound_vars);
        }

        let mut infcx = self.inner.borrow_mut();

        for item in &mut tast.items {
            item.substitute(&mut infcx, &mut unbound_vars);
        }

        unbound_vars
    }
}

fn substitute_ty(
    infcx: &mut InferCtxtInner,
    ty: Type,
    unbound_vars: &mut HashSet<TypeVar>,
) -> Type {
    Type::new(substitute_tykind(infcx, &ty, unbound_vars))
}

fn substitute_tykind(
    infcx: &mut InferCtxtInner,
    ty: &TypeKind,
    unbound_vars: &mut HashSet<TypeVar>,
) -> TypeKind {
    match ty {
        TypeKind::Function(fun) => TypeKind::Function(FunctionType {
            ret: substitute_ty(infcx, fun.ret, unbound_vars),
            params: fun
                .params
                .iter()
                .map(|param| FunctionTypeParam {
                    name: param.name,
                    ty: substitute_ty(infcx, param.ty, unbound_vars),
                })
                .collect(),
        }),
        TypeKind::Infer(InferType::TypeVar(var)) => {
            let root = infcx.ty_unification_table.find(*var);

            if let Some(ty) = infcx.ty_unification_table.probe_value(root) {
                substitute_tykind(infcx, &ty, unbound_vars)
            } else {
                unbound_vars.insert(root);
                TypeKind::Infer(InferType::TypeVar(root))
            }
        }
        TypeKind::Infer(InferType::IntVar(var)) => {
            let root = infcx.int_unification_table.find(*var);

            infcx
                .int_unification_table
                .probe_value(root)
                .map_or_else(|| TypeKind::DEFAULT_INT, Into::into)
        }
        _ => ty.clone(),
    }
}

trait Substitute<'db> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>);
}

impl Substitute<'_> for Expr {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        match self {
            Self::Item(inner) => inner.substitute(infcx, unbound_vars),
            Self::If(inner) => inner.substitute(infcx, unbound_vars),
            Self::Block(inner) => inner.substitute(infcx, unbound_vars),
            Self::Return(inner) => inner.substitute(infcx, unbound_vars),
            Self::Call(inner) => inner.substitute(infcx, unbound_vars),
            Self::Bin(inner) => inner.substitute(infcx, unbound_vars),
            Self::Name(_) | Self::Lit(_) => (),
        }

        self.set_ty(substitute_ty(infcx, self.ty(), unbound_vars));
    }
}

impl Substitute<'_> for Item {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        match &mut self.kind {
            ItemKind::Function(fun) => fun.substitute(infcx, unbound_vars),
        }

        self.ty = substitute_ty(infcx, self.ty, unbound_vars);
    }
}

impl Substitute<'_> for Function {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.sig.substitute(infcx, unbound_vars);
        self.body.substitute(infcx, unbound_vars);
        self.ty = substitute_ty(infcx, self.ty, unbound_vars);
    }
}

impl Substitute<'_> for FunctionSig {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        for param in &mut self.params {
            param.ty = substitute_ty(infcx, param.ty, unbound_vars);
        }
    }
}

impl Substitute<'_> for If {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.cond.substitute(infcx, unbound_vars);
        self.then.substitute(infcx, unbound_vars);
        self.otherwise.substitute(infcx, unbound_vars);
    }
}

impl Substitute<'_> for Block {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        for stmt in &mut self.exprs {
            stmt.substitute(infcx, unbound_vars);
        }
    }
}

impl Substitute<'_> for Return {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.expr.substitute(infcx, unbound_vars);
    }
}

impl Substitute<'_> for Call {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.callee.substitute(infcx, unbound_vars);

        for arg in &mut self.args {
            match arg {
                CallArg::Positional(expr) | CallArg::Named(_, expr) => {
                    expr.substitute(infcx, unbound_vars);
                }
            }

            arg.set_ty(substitute_ty(infcx, arg.ty(), unbound_vars));
        }
    }
}

impl Substitute<'_> for Bin {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.lhs.substitute(infcx, unbound_vars);
        self.rhs.substitute(infcx, unbound_vars);
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Vec<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        for item in self {
            item.substitute(infcx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Option<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        if let Some(item) = self {
            item.substitute(infcx, unbound_vars);
        }
    }
}

impl<'db, T: Substitute<'db>> Substitute<'db> for Box<T> {
    fn substitute(&mut self, infcx: &mut InferCtxtInner, unbound_vars: &mut HashSet<TypeVar>) {
        self.as_mut().substitute(infcx, unbound_vars);
    }
}
