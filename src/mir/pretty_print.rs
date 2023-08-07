use pretty::RcDoc;

use crate::{db::Database, ty::Ty};

use super::*;

pub(crate) fn print(db: &Database, mir: &Mir) {
    let doc = RcDoc::intersperse(
        mir.functions.iter().map(|f| f.to_doc(db)),
        RcDoc::hardline().append(RcDoc::hardline()),
    );

    println!("{}", doc.pretty(80));
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Function {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        let ret_ty = self
            .id
            .get(db)
            .ty
            .get(db)
            .kind
            .as_function()
            .unwrap()
            .ret
            .to_doc(db);

        RcDoc::text("fn")
            .append(RcDoc::space())
            .append(RcDoc::text(self.id().get(db).name.standard_full_name()))
            .append(RcDoc::text("()"))
            .append(RcDoc::space())
            .append(ret_ty)
            .append(RcDoc::space())
            .append(RcDoc::text("{"))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.blocks().iter().map(|blk| blk.to_doc(db)),
                RcDoc::hardline(),
            ))
            // .nest(1)
            .append(RcDoc::hardline())
            .append("}")
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Block {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text("@")
            .append(RcDoc::text(self.name.as_str()))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.instructions.iter().map(|inst| inst.to_doc(db)),
                RcDoc::hardline(),
            ))
            .nest(1)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Instruction {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        match self {
            Instruction::Return(ret) => RcDoc::text("ret")
                .append(RcDoc::space())
                .append(ret.register.to_doc(db)),
            Instruction::IntLit(lit) => {
                register_start(db, lit.register).append(RcDoc::text(lit.value.to_string()))
            }
            Instruction::UnitLit(lit) => register_start(db, lit.register).append(RcDoc::text("()")),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for RegisterId {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text("%").append(RcDoc::text(self.0.to_string()))
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

fn register_start<'db, 'd>(db: &'db Database, reg: RegisterId) -> RcDoc<'d, ()> {
    reg.to_doc(db)
        .append(RcDoc::space())
        .append(RcDoc::text("="))
        .append(RcDoc::space())
}
