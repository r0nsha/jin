use std::io;

use crate::{db::Db, tir::*};

pub(super) fn print(db: &Db, tir: &Tir, w: &mut impl io::Write) -> io::Result<()> {
    let mut builder = ptree::TreeBuilder::new("Tir".to_string());

    for glob in tir.globals.iter() {
        match &glob.kind {
            GlobalKind::Bare { value, body } => {
                PrettyCx { builder: &mut builder, db, tir, body }
                    .pp_let(glob.name, glob.ty, *value);
            }
            GlobalKind::Extern => {
                builder.add_empty_child(format!(
                    "let extern `{}` (type: {})",
                    glob.name,
                    glob.ty.display(db)
                ));
            }
        }
    }

    for f in &tir.fns {
        PrettyCx { builder: &mut builder, db, tir, body: &f.body }.pp_fn(f);
    }

    let tree = builder.build();
    ptree::write_tree_with(&tree, w, &ptree::PrintConfig::default())
}

struct PrettyCx<'db> {
    builder: &'db mut ptree::TreeBuilder,
    db: &'db Db,
    tir: &'db Tir,
    body: &'db Body,
}

impl PrettyCx<'_> {
    fn pp_fn(&mut self, f: &Fn) {
        self.builder.begin_child(format!(
            "fn {} (returns: {})",
            self.tir.sigs[f.sig].name,
            self.tir.sigs[f.sig].ty.as_fn().expect("to be a function").ret.display(self.db)
        ));

        let sig = &self.tir.sigs[f.sig];

        if !sig.params.is_empty() {
            self.builder.begin_child("params".to_string());

            for param in f.params(self.tir) {
                let local = self.body.local(param.id);

                self.builder.add_empty_child(format!(
                    "{} (type: {})",
                    local.name,
                    local.ty.display(self.db)
                ));
            }

            self.builder.end_child();
        }

        self.pp_expr(f.value);

        self.builder.end_child();
    }

    fn pp_let(&mut self, name: Ustr, ty: Ty, value: ExprId) {
        self.builder.begin_child(format!("let {} (type: {})", name, ty.display(self.db)));
        self.pp_expr(value);
        self.builder.end_child();
    }

    fn pp_expr(&mut self, expr: ExprId) {
        let expr = self.body.expr(expr);

        match &expr.kind {
            ExprKind::Let { id, def_id: _, value } => {
                let local = self.body.local(*id);
                self.pp_let(local.name, local.ty, *value);
            }
            ExprKind::If { cond, then, otherwise } => {
                self.builder.begin_child("if".to_string());

                self.builder.begin_child("cond".to_string());
                self.pp_expr(*cond);
                self.builder.end_child();

                self.builder.begin_child("then".to_string());
                self.pp_expr(*then);
                self.builder.end_child();

                if let Some(otherwise) = *otherwise {
                    self.builder.begin_child("else".to_string());
                    self.pp_expr(otherwise);
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Block { exprs } => {
                self.builder.begin_child("block".to_string());

                for expr in exprs {
                    self.pp_expr(*expr);
                }

                self.builder.end_child();
            }
            ExprKind::Return { value } => {
                self.builder.begin_child("return".to_string());
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Call { callee, args } => {
                self.builder.begin_child(format!("call (result: {})", expr.ty.display(self.db)));
                self.pp_expr(*callee);

                if !args.is_empty() {
                    self.builder.begin_child("args".to_string());
                    for arg in args {
                        self.pp_expr(*arg);
                    }
                    self.builder.end_child();
                }

                self.builder.end_child();
            }
            ExprKind::Binary { lhs, rhs, op } => {
                self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
                self.pp_expr(*lhs);
                self.pp_expr(*rhs);
                self.builder.end_child();
            }
            ExprKind::Unary { value, op } => {
                self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Cast { value, target } => {
                self.builder.begin_child(format!("cast (to: {})", target.display(self.db)));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Index { value, index } => {
                self.builder.begin_child(format!(
                    "index {} (type: {})",
                    index,
                    expr.ty.display(self.db)
                ));
                self.pp_expr(*value);
                self.builder.end_child();
            }
            ExprKind::Id(id) => match id {
                Id::Fn(fid) => {
                    self.builder.add_empty_child(format!(
                        "`{}` (type: {})",
                        self.tir.sigs[*fid].name,
                        expr.ty.display(self.db)
                    ));
                }
                Id::Global(gid) => {
                    self.builder.add_empty_child(format!(
                        "`{}` (type: {})",
                        self.tir.globals[*gid].name,
                        expr.ty.display(self.db)
                    ));
                }
                Id::Local(lid) => {
                    self.builder.add_empty_child(format!(
                        "`{}` (type: {})",
                        self.body.local(*lid).name,
                        expr.ty.display(self.db)
                    ));
                }
            },
            ExprKind::Const(value) => match value {
                Const::Str(value) => {
                    self.builder.add_empty_child(format!(
                        "{} (type: {})",
                        value,
                        expr.ty.display(self.db)
                    ));
                }
                Const::Int(value) => {
                    self.builder.add_empty_child(format!(
                        "{} (type: {})",
                        value,
                        expr.ty.display(self.db)
                    ));
                }
                Const::Bool(value) => {
                    self.builder.add_empty_child(value.to_string());
                }
                Const::Unit => {
                    self.builder.add_empty_child("()".to_string());
                }
            },
        }
    }
}
