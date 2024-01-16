use std::io;

use crate::{
    db::Db,
    hir::{Expr, ExprKind, Fn, FnKind, Hir, Let, Pat},
    middle::BinOp,
};

pub(super) fn print(
    db: &Db,
    hir: &Hir,
    w: &mut impl io::Write,
) -> io::Result<()> {
    let mut cx =
        PrettyCx { db, builder: ptree::TreeBuilder::new("Hir".to_string()) };

    for let_ in &hir.lets {
        cx.pp_let(let_);
    }

    for let_ in &hir.extern_lets {
        cx.builder.add_empty_child(format!(
            "let extern `{}` (type: {})",
            let_.word,
            cx.db[let_.id].ty.display(cx.db)
        ));
    }

    for f in &hir.fns {
        cx.pp_fn(f);
    }

    let tree = cx.builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PrettyCx<'db> {
    db: &'db Db,
    builder: ptree::TreeBuilder,
}

impl PrettyCx<'_> {
    fn pp_fn(&mut self, f: &Fn) {
        let fn_ty = f.sig.ty.as_fn().expect("to be a function");

        self.builder.begin_child(format!(
            "fn {} (returns: {}{})",
            self.db[f.def_id].qpath,
            fn_ty.ret.display(self.db),
            if fn_ty.is_c_variadic { ", c variadic" } else { "" }
        ));

        if !f.sig.params.is_empty() {
            self.builder.begin_child("params".to_string());

            for param in &f.sig.params {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    match &param.pat {
                        Pat::Name(name) => name.word.name().as_str(),
                        Pat::Discard(_) => "_",
                    },
                    param.ty.display(self.db)
                ));
            }

            self.builder.end_child();
        }

        match &f.kind {
            FnKind::Bare { body } => {
                self.pp_expr(body);
            }
            FnKind::Extern { .. } => {
                self.builder.add_empty_child("extern".to_string());
            }
        }

        self.builder.end_child();
    }

    fn pp_let(&mut self, let_: &Let) {
        self.builder.begin_child(format!(
            "let (type: {})",
            let_.value.ty.display(self.db)
        ));
        self.pp_expr(&let_.value);
        self.builder.end_child();
    }

    #[allow(clippy::too_many_lines)]
    fn pp_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Let(let_) => {
                self.pp_let(let_);
            }
            ExprKind::Assign(assign) => {
                self.builder.begin_child(format!(
                    "{}=",
                    assign.op.map(BinOp::as_str).unwrap_or_default()
                ));
                self.pp_expr(&assign.lhs);
                self.builder.add_empty_child("to".to_string());
                self.pp_expr(&assign.rhs);
                self.builder.end_child();
            }
            ExprKind::Swap(swap) => {
                self.builder.begin_child(":=".to_string());
                self.pp_expr(&swap.lhs);
                self.builder.add_empty_child("and".to_string());
                self.pp_expr(&swap.rhs);
                self.builder.end_child();
            }
            ExprKind::Match(match_) => {
                self.builder.begin_child("match".to_string());

                self.builder.begin_child("expr".to_string());
                self.pp_expr(&match_.expr);
                self.builder.end_child();

                self.builder.begin_child("cases".to_string());
                self.builder.add_empty_child("TODO: write cases".to_string());
                self.builder.end_child();

                self.builder.end_child();
            }
            ExprKind::Loop(loop_) => {
                self.builder.begin_child("loop".to_string());

                if let Some(cond) = &loop_.cond {
                    self.builder.begin_child("cond".to_string());
                    self.pp_expr(cond);
                    self.builder.end_child();
                }

                self.builder.begin_child("expr".to_string());
                self.pp_expr(&loop_.expr);
                self.builder.end_child();

                self.builder.end_child();
            }
            ExprKind::Break => {
                self.builder.add_empty_child("break".to_string());
            }
            ExprKind::Block(block) => {
                self.builder.begin_child("block".to_string());

                for expr in &block.exprs {
                    self.pp_expr(expr);
                }

                self.builder.end_child();
            }
            ExprKind::Return(ret) => {
                self.builder.begin_child("return".to_string());
                self.pp_expr(&ret.expr);
                self.builder.end_child();
            }
            ExprKind::Call(call) => {
                self.builder.begin_child(format!(
                    "call (result: {})",
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&call.callee);

                if !call.args.is_empty() {
                    self.builder.begin_child("args".to_string());

                    for arg in &call.args {
                        if let Some(name) = arg.name {
                            self.builder.begin_child(name.to_string());
                            self.pp_expr(&arg.expr);
                            self.builder.end_child();
                        } else {
                            self.pp_expr(&arg.expr);
                        }
                    }

                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Unary(un) => {
                self.builder.begin_child(format!(
                    "{} (result: {})",
                    un.op,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&un.expr);
                self.builder.end_child();
            }
            ExprKind::Binary(bin) => {
                self.builder.begin_child(format!(
                    "{} (result: {})",
                    bin.op,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&bin.lhs);
                self.pp_expr(&bin.rhs);
                self.builder.end_child();
            }
            ExprKind::Cast(cast) => {
                self.builder.begin_child(format!(
                    "cast (to: {})",
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&cast.expr);
                self.builder.end_child();
            }
            ExprKind::Field(access) => {
                self.builder.begin_child(format!(
                    "field `{}` (type: {})",
                    access.field,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(&access.expr);
                self.builder.end_child();
            }
            ExprKind::Name(name) => {
                self.builder.add_empty_child(format!(
                    "`{}` (type: {})",
                    self.db[name.id].qpath,
                    expr.ty.display(self.db)
                ));
            }
            ExprKind::Variant(variant) => {
                let variant = &self.db[variant.id];

                self.builder.add_empty_child(format!(
                    "variant `{}.{}` (type: {})",
                    self.db[variant.adt_id].name,
                    variant.name,
                    expr.ty.display(self.db)
                ));
            }
            ExprKind::SliceLit(lit) => {
                self.builder.begin_child("slice lit".to_string());

                for expr in &lit.exprs {
                    self.pp_expr(expr);
                }

                if let Some(cap) = &lit.cap {
                    self.builder.begin_child("cap".to_string());
                    self.pp_expr(cap);
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::StrLit(v) => {
                self.builder.add_empty_child(v.to_string());
            }
            ExprKind::IntLit(v) => {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    v,
                    expr.ty.display(self.db),
                ));
            }
            ExprKind::FloatLit(v) => {
                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    v,
                    expr.ty.display(self.db),
                ));
            }
            ExprKind::BoolLit(v) => {
                self.builder.add_empty_child(v.to_string());
            }
        }
    }
}
