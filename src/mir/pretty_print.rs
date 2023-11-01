use std::{fmt, fmt::Write, io};

use pretty::{docs, RcDoc};

use crate::{db::Db, mir::*};

pub(super) fn print(db: &Db, mir: &Mir, w: &mut impl io::Write) -> io::Result<()> {
    // for glob in &mir.globals {
    //     match &glob.kind {
    //         GlobalKind::Bare { value, body } => {
    //             PrettyCx { builder: &mut builder, db, mir, body }
    //                 .pp_let(glob.name, glob.ty, *value);
    //         }
    //         GlobalKind::Extern => {
    //             builder.add_empty_child(format!(
    //                 "let extern `{}` (type: {})",
    //                 glob.name,
    //                 glob.ty.display(db)
    //             ));
    //         }
    //     }
    // }

    let mut str = String::new();

    for f in &mir.fns {
        PrettyCx { str: &mut str, db, mir, body: &f.body }.pp_fn(f);
    }

    w.write_all(str.as_bytes())
}

struct PrettyCx<'db> {
    str: &'db mut String,
    db: &'db Db,
    mir: &'db Mir,
    body: &'db Body,
}

impl PrettyCx<'_> {
    fn pp_fn(&mut self, f: &Fn) {
        // self.builder.begin_child(format!(
        //     "fn {} (returns: {})",
        //     self.mir.fn_sigs[f.sig].name,
        //     self.mir.fn_sigs[f.sig].ty.as_fn().expect("to be a function").ret.display(self.db)
        // ));
        //
        // let sig = &self.mir.fn_sigs[f.sig];
        //
        // if !sig.params.is_empty() {
        //     self.builder.begin_child("params".to_string());
        //
        //     for param in f.params(self.mir) {
        //         let local = self.body.local(param.id);
        //
        //         self.builder.add_empty_child(format!(
        //             "{} (type: {})",
        //             local.name,
        //             local.ty.display(self.db)
        //         ));
        //     }
        //
        //     self.builder.end_child();
        // }
        //
        // self.pp_expr(f.value);
        //
        // self.builder.end_child();
    }

    fn pp_let(&mut self, name: Ustr, ty: Ty, value: BlockId) {
        // self.builder.begin_child(format!("let {} (type: {})", name, ty.display(self.db)));
        // self.pp_expr(value);
        // self.builder.end_child();
    }

    fn pp_expr(&mut self, expr: BlockId) {
        // let expr = self.body.expr(expr);
        //
        // match &expr.kind {
        //     ExprKind::Let { id, def_id: _, value } => {
        //         let local = self.body.local(*id);
        //         self.pp_let(local.name, local.ty, *value);
        //     }
        //     ExprKind::If { cond, then, otherwise } => {
        //         self.builder.begin_child("if".to_string());
        //
        //         self.builder.begin_child("cond".to_string());
        //         self.pp_expr(*cond);
        //         self.builder.end_child();
        //
        //         self.builder.begin_child("then".to_string());
        //         self.pp_expr(*then);
        //         self.builder.end_child();
        //
        //         self.builder.begin_child("else".to_string());
        //         self.pp_expr(*otherwise);
        //         self.builder.end_child();
        //
        //         self.builder.end_child();
        //     }
        //     ExprKind::Block { exprs } => {
        //         self.builder.begin_child("block".to_string());
        //
        //         for expr in exprs {
        //             self.pp_expr(*expr);
        //         }
        //
        //         self.builder.end_child();
        //     }
        //     ExprKind::Return { value } => {
        //         self.builder.begin_child("return".to_string());
        //         self.pp_expr(*value);
        //         self.builder.end_child();
        //     }
        //     ExprKind::Call { callee, args } => {
        //         self.builder.begin_child(format!("call (result: {})", expr.ty.display(self.db)));
        //         self.pp_expr(*callee);
        //
        //         if !args.is_empty() {
        //             self.builder.begin_child("args".to_string());
        //             for arg in args {
        //                 self.pp_expr(*arg);
        //             }
        //             self.builder.end_child();
        //         }
        //
        //         self.builder.end_child();
        //     }
        //     ExprKind::Binary { lhs, rhs, op } => {
        //         self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
        //         self.pp_expr(*lhs);
        //         self.pp_expr(*rhs);
        //         self.builder.end_child();
        //     }
        //     ExprKind::Unary { value, op } => {
        //         self.builder.begin_child(format!("{} (result: {})", op, expr.ty.display(self.db)));
        //         self.pp_expr(*value);
        //         self.builder.end_child();
        //     }
        //     ExprKind::Cast { value, target } => {
        //         self.builder.begin_child(format!("cast (to: {})", target.display(self.db)));
        //         self.pp_expr(*value);
        //         self.builder.end_child();
        //     }
        //     ExprKind::Index { value, index } => {
        //         self.builder.begin_child(format!(
        //             "index {} (type: {})",
        //             index,
        //             expr.ty.display(self.db)
        //         ));
        //         self.pp_expr(*value);
        //         self.builder.end_child();
        //     }
        //     ExprKind::Id(id) => match id {
        //         Id::Fn(fid) => {
        //             self.builder.add_empty_child(format!(
        //                 "`{}` (type: {})",
        //                 self.mir.fn_sigs[*fid].name,
        //                 expr.ty.display(self.db)
        //             ));
        //         }
        //         Id::Global(gid) => {
        //             self.builder.add_empty_child(format!(
        //                 "`{}` (type: {})",
        //                 self.mir.globals[*gid].name,
        //                 expr.ty.display(self.db)
        //             ));
        //         }
        //         Id::Local(lid) => {
        //             self.builder.add_empty_child(format!(
        //                 "`{}` (type: {})",
        //                 self.body.local(*lid).name,
        //                 expr.ty.display(self.db)
        //             ));
        //         }
        //     },
        //     ExprKind::Const(value) => match value {
        //         Const::Str(value) => {
        //             self.builder.add_empty_child(format!(
        //                 "{} (type: {})",
        //                 value,
        //                 expr.ty.display(self.db)
        //             ));
        //         }
        //         Const::Int(value) => {
        //             self.builder.add_empty_child(format!(
        //                 "{} (type: {})",
        //                 value,
        //                 expr.ty.display(self.db)
        //             ));
        //         }
        //         Const::Bool(value) => {
        //             self.builder.add_empty_child(value.to_string());
        //         }
        //         Const::Unit => {
        //             self.builder.add_empty_child("()".to_string());
        //         }
        //     },
        // }
    }
}

struct PrintFn<'a> {
    name: RcDoc<'a>,
    params: Vec<RcDoc<'a>>,
    ret: RcDoc<'a>,
    blocks: Vec<PrintBlock<'a>>,
}

impl<'a> PrintFn<'a> {
    fn to_doc(self) -> RcDoc<'a> {
        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(self.name)
            .append(
                RcDoc::text("(")
                    .append(RcDoc::intersperse(
                        self.params,
                        RcDoc::text(",").append(RcDoc::space()),
                    ))
                    .append(RcDoc::text(")").nest(NEST).group()),
            )
            .append(RcDoc::space())
            .append(RcDoc::text("->"))
            .append(RcDoc::space())
            .append(
                RcDoc::text("{")
                    .append(RcDoc::hardline())
                    .append(
                        RcDoc::intersperse(
                            self.blocks.into_iter().map(|b| b.to_doc()),
                            RcDoc::hardline().append(RcDoc::hardline()),
                        )
                        .nest(NEST)
                        .group(),
                    )
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("}")),
            )
    }
}

struct PrintBlock<'a> {
    name: RcDoc<'a>,
    instructions: Vec<RcDoc<'a>>,
}

impl<'a> PrintBlock<'a> {
    fn to_doc(self) -> RcDoc<'a> {
        self.name
            .append(RcDoc::text(":"))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(self.instructions, RcDoc::hardline()).nest(NEST).group())
    }
}

fn global_name(name: &str) -> RcDoc {
    RcDoc::text("%").append(name)
}

// TODO:
// fn value_name(id: ValueId) -> RcDoc {
//    RcDoc::text("v").append(id.to_string())
// }

const NEST: isize = 2;
