use pretty::RcDoc;

use crate::{
    db::{Database, TyId},
    ty::Ty,
};

use super::*;

pub(crate) fn print_module(db: &Database, module: &Module) {
    let doc = RcDoc::text(format!(
        "module {}",
        module.id.get(db).name.standard_full_name()
    ))
    .append(RcDoc::space())
    .append(RcDoc::text("{"))
    .append(RcDoc::line())
    .append(RcDoc::intersperse(
        module.definitions.iter().map(|b| b.to_doc(db)),
        RcDoc::line().append(RcDoc::line()),
    ))
    .nest(1)
    .append(RcDoc::line())
    .append("}");

    println!("{}", doc.pretty(80));
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Node {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        match self {
            Node::Function(x) => x.to_doc(db),
            Node::Block(x) => x.to_doc(db),
            Node::Return(x) => x.to_doc(db),
            Node::Call(x) => x.to_doc(db),
            Node::Name(x) => x.to_doc(db),
            Node::Lit(x) => x.to_doc(db),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Definition {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        match &self.kind {
            DefinitionKind::Function(fun) => fun.to_doc(db),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Function {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        let ret_ty = self.ty.get(db).kind.as_function().unwrap().ret.to_doc(db);

        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(self.name.as_str()))
            .append(RcDoc::text("("))
            // TODO: Print function params
            // .append(RcDoc::intersperse(
            //     self.params.values().map(|p| {
            //         RcDoc::text(p.name.as_str()).append(RcDoc::space()).append(
            //             p.id.expect("to have a definition").get(db).ty.to_doc(db),
            //         )
            //     }),
            //     RcDoc::text(",").append(RcDoc::space()),
            // ))
            .append(RcDoc::text(")"))
            .append(RcDoc::space())
            .append(ret_ty)
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
            .append(self.body.to_doc(db))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Block {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text("{")
            .append(RcDoc::line())
            .append(RcDoc::intersperse(
                self.exprs.iter().map(|e| e.to_doc(db)),
                RcDoc::line(),
            ))
            .nest(1)
            .append(RcDoc::line())
            .append("}")
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Return {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text("return")
            .append(RcDoc::space())
            .append(self.expr.as_ref().map_or(RcDoc::nil(), |e| e.to_doc(db)))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Call {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        self.callee.to_doc(db).append(RcDoc::text("()"))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Name {
    fn to_doc(&self, _db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text(self.name.as_str())
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Lit {
    fn to_doc(&self, _db: &'db Database) -> RcDoc<'d, ()> {
        match &self.kind {
            LitKind::Int(v) => RcDoc::text(v.to_string()),
            LitKind::Unit => RcDoc::text("()"),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for TyId {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        self.get(db).to_doc(db)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Ty {
    fn to_doc(&self, _db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text(self.to_string())
    }
}
