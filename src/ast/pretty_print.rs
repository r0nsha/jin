use std::io;

use super::{Expr, Fn, Item, LitKind, Module};
use crate::ast::{CallArg, ExternLet, FnKind, FnSig, Let, TyExpr};

pub(super) fn print_module(module: &Module, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PrettyCx { builder: ptree::TreeBuilder::new(module.name.standard_full_name()) };

    for item in &module.items {
        item.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PrettyCx {
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut PrettyCx);
}

impl PrettyPrint for Expr {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            Self::Item(item) => item.pretty_print(cx),
            Self::Return { expr, .. } => {
                cx.builder.begin_child("return".to_string());

                if let Some(expr) = expr {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::If { cond, then, otherwise, .. } => {
                cx.builder.begin_child("if".to_string());

                cx.builder.begin_child("cond".to_string());
                cond.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.begin_child("then".to_string());
                then.pretty_print(cx);
                cx.builder.end_child();

                if let Some(otherwise) = otherwise {
                    cx.builder.begin_child("else".to_string());
                    otherwise.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            Self::Block { exprs, .. } => {
                cx.builder.begin_child("block".to_string());

                for expr in exprs {
                    expr.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::Call { callee, args, .. } => {
                cx.builder.begin_child("call".to_string());
                callee.pretty_print(cx);

                if !args.is_empty() {
                    cx.builder.begin_child("args".to_string());
                    for arg in args {
                        match arg {
                            CallArg::Positional(expr) => expr.pretty_print(cx),
                            CallArg::Named(name, expr) => {
                                cx.builder.begin_child(name.to_string());
                                expr.pretty_print(cx);
                                cx.builder.end_child();
                            }
                        }
                    }
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            Self::Unary { expr, op, .. } => {
                cx.builder.begin_child(op.to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Binary { lhs, rhs, op, .. } => {
                cx.builder.begin_child(op.to_string());
                lhs.pretty_print(cx);
                rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Cast { expr, ty, .. } => {
                cx.builder.begin_child("cast".to_string());
                expr.pretty_print(cx);
                cx.builder.begin_child("to".to_string());
                ty.pretty_print(cx);
                cx.builder.end_child();
                cx.builder.end_child();
            }
            Self::Member { expr, member, .. } => {
                cx.builder.begin_child(format!("member access `{member}`"));
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Name { word, .. } => {
                cx.builder.add_empty_child(format!("`{word}`"));
            }
            Self::Lit { kind, .. } => match kind {
                LitKind::Str(value) => {
                    cx.builder.add_empty_child(format!("str: {value}"));
                }
                LitKind::Int(value) => {
                    cx.builder.add_empty_child(format!("int: {value}"));
                }
                LitKind::Bool(value) => {
                    cx.builder.add_empty_child(format!("bool: {value}"));
                }
                LitKind::Unit => {
                    cx.builder.add_empty_child("()".to_string());
                }
            },
            Self::Group { expr, span: _ } => {
                cx.builder.begin_child("group".to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();
            }
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            Self::Fn(f) => f.pretty_print(cx),
            Self::Let(l) => l.pretty_print(cx),
            Self::ExternLet(l) => l.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Fn {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("fn {}", self.sig.word));
        self.sig.pretty_print(cx);

        match &self.kind {
            FnKind::Bare { body } => body.pretty_print(cx),
            FnKind::Extern {} => {
                cx.builder.add_empty_child("extern".to_string());
            }
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for FnSig {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        if !self.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in &self.params {
                cx.builder.add_empty_child(format!("{}", param.name));
                param.ty_annot.pretty_print(cx);
            }

            cx.builder.end_child();
        }

        if let Some(ret) = &self.ret {
            cx.builder.begin_child("ret".to_string());
            ret.pretty_print(cx);
            cx.builder.end_child();
        }
    }
}

impl PrettyPrint for Let {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("let {}", self.pat));

        if let Some(ty) = &self.ty_annot {
            cx.builder.begin_child("type".to_string());
            ty.pretty_print(cx);
            cx.builder.end_child();
        }

        self.value.pretty_print(cx);

        cx.builder.end_child();
    }
}

impl PrettyPrint for ExternLet {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("let extern {}", self.word));

        cx.builder.begin_child("type".to_string());
        self.ty_annot.pretty_print(cx);
        cx.builder.end_child();

        cx.builder.end_child();
    }
}

impl PrettyPrint for TyExpr {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            TyExpr::RawPtr(pointee, _) => {
                cx.builder.begin_child("raw ptr".to_string());
                pointee.pretty_print(cx);
                cx.builder.end_child();
            }
            TyExpr::Name(name) => {
                cx.builder.add_empty_child(name.word.to_string());

                if !name.args.is_empty() {
                    cx.builder.begin_child("type args".to_string());
                    for arg in &name.args {
                        arg.pretty_print(cx);
                    }
                    cx.builder.end_child();
                }
            }
            TyExpr::Unit(_) => {
                cx.builder.add_empty_child("()".to_string());
            }
            TyExpr::Hole(_) => {
                cx.builder.add_empty_child("_".to_string());
            }
        }
    }
}
