use std::io;

use super::{Expr, Fn, Item, LitKind, Module};
use crate::{
    ast::{
        CallArg, ExternImport, ExternLet, FnKind, FnSig, Import, ImportPath, Let, TyDef, TyDefKind,
        TyExpr,
    },
    db::StructKind,
    middle::BinOp,
};

pub(super) fn print_module(module: &Module, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PrettyCx { builder: ptree::TreeBuilder::new(module.name.join()) };

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
            Self::Let(let_) => let_.pretty_print(cx),
            Self::Assign { lhs, rhs, op, .. } => {
                cx.builder.begin_child(format!("{}=", op.map(BinOp::as_str).unwrap_or_default()));
                lhs.pretty_print(cx);
                cx.builder.add_empty_child("to".to_string());
                rhs.pretty_print(cx);
                cx.builder.end_child();
            }
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
            Self::Loop { cond, expr, .. } => {
                cx.builder.begin_child("loop".to_string());

                if let Some(cond) = cond {
                    cx.builder.begin_child("cond".to_string());
                    cond.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.begin_child("expr".to_string());
                expr.pretty_print(cx);
                cx.builder.end_child();

                cx.builder.end_child();
            }
            Self::Break { .. } => {
                cx.builder.add_empty_child("break".to_string());
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
            Self::Cast { expr, ty_expr: ty, .. } => {
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
            Self::Lit { kind, .. } => {
                match kind {
                    LitKind::Bool(value) => cx.builder.add_empty_child(format!("bool: {value}")),
                    LitKind::Int(value) => cx.builder.add_empty_child(format!("int: {value}")),
                    LitKind::Float(value) => cx.builder.add_empty_child(format!("float: {value}")),
                    LitKind::Str(value) => cx.builder.add_empty_child(format!("str: {value}")),
                };
            }
        }
    }
}

impl PrettyPrint for Item {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            Self::Fn(x) => x.pretty_print(cx),
            Self::Let(x) => x.pretty_print(cx),
            Self::Type(x) => x.pretty_print(cx),
            Self::Import(x) => x.pretty_print(cx),
            Self::ExternLet(x) => x.pretty_print(cx),
            Self::ExternImport(x) => x.pretty_print(cx),
        }
    }
}

impl PrettyPrint for Fn {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("fn {}", self.sig.word));
        self.sig.pretty_print(cx);

        match &self.kind {
            FnKind::Bare { body } => body.pretty_print(cx),
            FnKind::Extern { is_c_variadic } => {
                cx.builder.add_empty_child(format!(
                    "extern{}",
                    if *is_c_variadic { "(variadic)" } else { "" }
                ));
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
                param.ty_expr.pretty_print(cx);
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

        if let Some(ty) = &self.ty_expr {
            cx.builder.begin_child("type".to_string());
            ty.pretty_print(cx);
            cx.builder.end_child();
        }

        self.value.pretty_print(cx);

        cx.builder.end_child();
    }
}

impl PrettyPrint for TyDef {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("type {}", self.word));

        match &self.kind {
            TyDefKind::Struct(sdef) => {
                cx.builder.add_empty_child(format!(
                    "{}struct",
                    match sdef.kind {
                        StructKind::Extern => "extern ",
                    }
                ));
                cx.builder.begin_child("fields".to_string());

                for field in &sdef.fields {
                    cx.builder.begin_child(field.name.to_string());
                    field.ty_expr.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
        }

        cx.builder.end_child();
    }
}

impl PrettyPrint for Import {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("import {}", self.root.name()));
        self.root.import_path.pretty_print(cx);
        cx.builder.end_child();
    }
}

impl PrettyPrint for ImportPath {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            ImportPath::Node(node) => {
                cx.builder.add_empty_child(node.name().to_string());
                node.import_path.pretty_print(cx);
            }
            ImportPath::None => todo!(),
        }
    }
}

impl PrettyPrint for ExternLet {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.begin_child(format!("let extern {}", self.word));

        cx.builder.begin_child("type".to_string());
        self.ty_expr.pretty_print(cx);
        cx.builder.end_child();

        cx.builder.end_child();
    }
}

impl PrettyPrint for ExternImport {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        cx.builder.add_empty_child(format!("import extern \"{}\"", self.lib));
    }
}

impl PrettyPrint for TyExpr {
    fn pretty_print(&self, cx: &mut PrettyCx) {
        match self {
            TyExpr::Fn(f) => {
                cx.builder.begin_child("fn type".to_string());

                if !f.params.is_empty() {
                    cx.builder.begin_child("params".to_string());

                    for param in &f.params {
                        param.pretty_print(cx);
                    }

                    cx.builder.end_child();
                }

                if let Some(ret) = &f.ret {
                    cx.builder.begin_child("ret".to_string());
                    ret.pretty_print(cx);
                    cx.builder.end_child();
                }

                cx.builder.end_child();
            }
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
                cx.builder.add_empty_child("{}".to_string());
            }
            TyExpr::Hole(_) => {
                cx.builder.add_empty_child("_".to_string());
            }
        }
    }
}
