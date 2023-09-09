use std::io;

use super::{Expr, Fn, Item, LitKind, Module};
use crate::ast::{Block, CallArg, Let, Ty};

pub(super) fn print_module(module: &Module, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PPCtxt { builder: ptree::TreeBuilder::new(module.name.standard_full_name()) };

    for item in &module.items {
        item.pretty_print(&mut cx);
    }

    let tree = cx.builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PPCtxt {
    builder: ptree::TreeBuilder,
}

trait PrettyPrint {
    fn pretty_print(&self, cx: &mut PPCtxt);
}

impl PrettyPrint for Expr {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        match self {
            Self::Item(item) => item.pretty_print(cx),
            Self::Return(ret) => {
                cx.builder.begin_child("return".to_string());

                if let Some(value) = &ret.expr {
                    value.pretty_print(cx);
                }

                cx.builder.end_child();
            }
            Self::If(if_) => {
                cx.builder.begin_child("if".to_string());

                cx.builder.begin_child("cond".to_string());
                if_.cond.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.begin_child("then".to_string());
                if_.then.pretty_print(cx);
                cx.builder.end_child();

                if let Some(otherwise) = &if_.otherwise {
                    cx.builder.begin_child("else".to_string());
                    otherwise.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
            Self::Block(blk) => blk.pretty_print(cx),
            Self::Call(call) => {
                cx.builder.begin_child("call".to_string());
                call.callee.pretty_print(cx);

                if !call.args.is_empty() {
                    cx.builder.begin_child("args".to_string());
                    for arg in &call.args {
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
            Self::UnaryOp(un) => {
                cx.builder.begin_child(un.op.to_string());
                un.expr.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::BinOp(bin) => {
                cx.builder.begin_child(bin.op.to_string());
                bin.lhs.pretty_print(cx);
                bin.rhs.pretty_print(cx);
                cx.builder.end_child();
            }
            Self::Cast(cast) => {
                cx.builder.begin_child("cast".to_string());
                cast.expr.pretty_print(cx);
                cx.builder.begin_child("to".to_string());
                cast.ty.pretty_print(cx);
                cx.builder.end_child();
                cx.builder.end_child();
            }
            Self::Name(name) => {
                cx.builder.add_empty_child(format!("`{}`", name.name));
            }
            Self::Lit(lit) => match lit.kind {
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
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        match self {
            Self::Fn(f) => f.pretty_print(cx),
            Self::Let(l) => l.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Fn {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        cx.builder.begin_child(format!("fn {}", self.sig.name));

        if !self.sig.params.is_empty() {
            cx.builder.begin_child("params".to_string());

            for param in &self.sig.params {
                cx.builder.add_empty_child(format!("{}", param.name));
                param.ty.pretty_print(cx);
            }

            if let Some(ret) = &self.sig.ret {
                ret.pretty_print(cx);
            }

            cx.builder.end_child();
        }

        self.body.pretty_print(cx);

        cx.builder.end_child();
    }
}

impl PrettyPrint for Let {
    fn pretty_print(&self, cx: &mut PPCtxt) {
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
impl PrettyPrint for Block {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        cx.builder.begin_child("block".to_string());

        for expr in &self.exprs {
            expr.pretty_print(cx);
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Ty {
    fn pretty_print(&self, cx: &mut PPCtxt) {
        match self {
            Ty::Name(name) => {
                cx.builder.add_empty_child(name.name.to_string());

                if !name.args.is_empty() {
                    cx.builder.begin_child("tyargs".to_string());
                    for arg in &name.args {
                        arg.pretty_print(cx);
                    }
                    cx.builder.end_child();
                }
            }
            Ty::Unit(_) => {
                cx.builder.add_empty_child("()".to_string());
            }
            Ty::Never(_) => {
                cx.builder.add_empty_child("!".to_string());
            }
            Ty::Infer(_) => {
                cx.builder.add_empty_child("_".to_string());
            }
        }
    }
}
