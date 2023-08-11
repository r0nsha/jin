use pretty::RcDoc;

use crate::{db::Database, ty::Ty};

use super::*;

pub(crate) fn print(db: &Database, mir: &Mir) {
    let doc = RcDoc::intersperse(
        mir.functions.iter().map(|f| print_function(db, f)),
        RcDoc::hardline().append(RcDoc::hardline()),
    );

    println!("{}", doc.pretty(80));
}

fn print_function<'d>(db: &Database, fun: &Function) -> RcDoc<'d, ()> {
    let ret_ty = fun
        .id
        .get(db)
        .ty
        .get(db)
        .kind
        .as_function()
        .unwrap()
        .ret
        .to_doc(db, fun);

    RcDoc::text("fn")
        .append(RcDoc::space())
        .append(RcDoc::text(
            fun.id().get(db).qualified_name.standard_full_name(),
        ))
        .append(RcDoc::text("()"))
        .append(RcDoc::space())
        .append(ret_ty)
        .append(RcDoc::space())
        .append(RcDoc::text("{"))
        .append(RcDoc::hardline())
        .append(RcDoc::intersperse(
            fun.blocks().iter().map(|blk| blk.to_doc(db, fun)),
            RcDoc::hardline(),
        ))
        .append(RcDoc::hardline())
        .append("}")
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Block {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text("@")
            .append(RcDoc::text(self.name.as_str()))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.instructions.iter().map(|inst| inst.to_doc(db, fun)),
                RcDoc::hardline(),
            ))
            .nest(1)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Instruction {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Instruction::Return(ret) => RcDoc::text("ret")
                .append(RcDoc::space())
                .append(ret.value.to_doc(db, fun)),
            Instruction::IntLit(lit) => register_alloc(db, fun, lit.register)
                .append(RcDoc::text(lit.value.to_string())),
            Instruction::UnitLit(lit) => {
                register_alloc(db, fun, lit.register).append(RcDoc::text("()"))
            }
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Value {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Value::Symbol(id) => {
                RcDoc::text(id.get(db).qualified_name.standard_full_name())
            }
            Value::Register(id) => id.to_doc(db, fun),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for RegisterId {
    fn to_doc(&self, _db: &'db Database, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text("%").append(RcDoc::text(self.0.to_string()))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for TyId {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()> {
        self.get(db).to_doc(db, fun)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Ty {
    fn to_doc(&self, _db: &'db Database, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(self.to_string())
    }
}

fn register_alloc<'db, 'd>(
    db: &'db Database,
    fun: &'db Function,
    reg: RegisterId,
) -> RcDoc<'d, ()> {
    reg.to_doc(db, fun)
        .append(RcDoc::space())
        .append(fun.register(reg).unwrap().ty.to_doc(db, fun))
        .append(RcDoc::space())
        .append(RcDoc::text("="))
        .append(RcDoc::space())
}
