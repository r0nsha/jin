use std::io;

use pretty::{Doc, RcDoc};

use crate::db::{Database, TypeId};

use super::*;

pub(crate) fn print_module(db: &Database, module: &Module) {
    let doc = RcDoc::<()>::text(format!(
        "module {}",
        module.id.get(db).name.standard_full_name()
    ))
    .append(RcDoc::space())
    .append(RcDoc::text("{"))
    .append(RcDoc::line())
    .append(RcDoc::intersperse(
        module.bindings.iter().map(|b| b.to_doc(db)),
        Doc::line(),
    ))
    .nest(1)
    .append(RcDoc::line())
    .append("}");

    println!("{}", doc.pretty(80));
}

trait ToDoc<'a> {
    fn to_doc(&self, db: &'a Database) -> RcDoc<()>;
}

impl ToDoc<'_> for Hir {
    fn to_doc(&self, db: &'_ Database) -> RcDoc<()> {
        match self {
            Hir::Fun(x) => x.to_doc(db),
            Hir::Block(x) => x.to_doc(db),
            Hir::Ret(x) => x.to_doc(db),
            Hir::Lit(x) => x.to_doc(db),
        }
    }
}

impl ToDoc<'_> for Binding {
    fn to_doc(&self, db: &'_ Database) -> RcDoc<()> {
        RcDoc::text("let")
            .append(Doc::space())
            .append(Doc::text(self.name.as_str()))
            .append(Doc::space())
            .append(Doc::text("="))
            .append(Doc::space())
            .append(self.expr.to_doc(db))
    }
}

impl ToDoc<'_> for Fun {
    fn to_doc(&self, db: &'_ Database) -> RcDoc<()> {
        RcDoc::text("fn()")
            .append(Doc::space())
            .append(self.body.to_doc(db))
    }
}

impl ToDoc<'_> for Block {
    fn to_doc(&self, db: &'_ Database) -> RcDoc<()> {
        RcDoc::text("{")
            .append(RcDoc::softline())
            .append(RcDoc::intersperse(
                self.exprs.iter().map(|e| e.to_doc(db)),
                Doc::line(),
            ))
            .nest(1)
            .append(RcDoc::softline())
            .append("}")
    }
}

impl ToDoc<'_> for Ret {
    fn to_doc(&self, db: &'_ Database) -> RcDoc<()> {
        RcDoc::text("return")
            .append(Doc::space())
            .append(self.expr.as_ref().map_or(RcDoc::nil(), |e| e.to_doc(db)))
    }
}

impl ToDoc<'_> for Lit {
    fn to_doc(&self, _db: &'_ Database) -> RcDoc<()> {
        match &self.kind {
            LitKind::Int(v) => RcDoc::text(v.to_string()),
            LitKind::Unit => RcDoc::text("()"),
        }
    }
}
