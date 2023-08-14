use pretty::RcDoc;

use crate::ty::typecx::{TyId, TypeCx};
use crate::{db::Database, ty::Ty};

use super::{Block, Function, Instruction, Mir, RegisterId, Value};

pub fn print(db: &Database, tcx: &TypeCx, mir: &Mir) {
    let cx = Cx { db, tcx };
    let doc = RcDoc::intersperse(
        mir.functions.iter().map(|f| print_function(&cx, f)),
        RcDoc::hardline().append(RcDoc::hardline()),
    );

    println!("{}", doc.pretty(80));
}

fn print_function<'d>(cx: &Cx, fun: &Function) -> RcDoc<'d, ()> {
    let ret_ty = cx.tcx[cx.db[fun.id].ty]
        .kind
        .as_function()
        .unwrap()
        .ret
        .to_doc(cx, fun);

    RcDoc::text("fn")
        .append(RcDoc::space())
        .append(RcDoc::text(
            cx.db[fun.id()].qualified_name.standard_full_name(),
        ))
        .append(RcDoc::text("()"))
        .append(RcDoc::space())
        .append(ret_ty)
        .append(RcDoc::space())
        .append(RcDoc::text("{"))
        .append(RcDoc::hardline())
        .append(RcDoc::intersperse(
            fun.blocks().iter().map(|blk| blk.to_doc(cx, fun)),
            RcDoc::hardline(),
        ))
        .append(RcDoc::hardline())
        .append("}")
}

struct Cx<'db> {
    db: &'db Database,
    tcx: &'db TypeCx,
}

impl Cx<'_> {
    fn register_alloc<'d>(
        &self,
        fun: &Function,
        reg: RegisterId,
    ) -> RcDoc<'d, ()> {
        reg.to_doc(self, fun)
            .append(RcDoc::space())
            .append(fun.register(reg).unwrap().ty.to_doc(self, fun))
            .append(RcDoc::space())
            .append(RcDoc::text("="))
            .append(RcDoc::space())
    }
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, cx: &'db Cx, fun: &'db Function) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Block {
    fn to_doc(&self, cx: &'db Cx, fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text("@")
            .append(RcDoc::text(self.name.as_str()))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.instructions.iter().map(|inst| inst.to_doc(cx, fun)),
                RcDoc::hardline(),
            ))
            .nest(1)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Instruction {
    fn to_doc(&self, cx: &'db Cx, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Self::Return(ret) => RcDoc::text("ret")
                .append(RcDoc::space())
                .append(ret.value.to_doc(cx, fun)),
            Self::Call(call) => cx
                .register_alloc(fun, call.register)
                .append(RcDoc::text("call"))
                .append(RcDoc::space())
                .append(call.callee.to_doc(cx, fun)),
            Self::IntLit(lit) => cx
                .register_alloc(fun, lit.register)
                .append(RcDoc::text(lit.value.to_string())),
            Self::UnitLit(lit) => {
                cx.register_alloc(fun, lit.register).append(RcDoc::text("()"))
            }
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Value {
    fn to_doc(&self, cx: &'db Cx, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Self::Definition(id) => {
                RcDoc::text(cx.db[*id].qualified_name.standard_full_name())
            }
            Self::Register(id) => id.to_doc(cx, fun),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for RegisterId {
    fn to_doc(&self, _cx: &'db Cx, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text("%").append(RcDoc::text(self.0.to_string()))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for TyId {
    fn to_doc(&self, cx: &'db Cx, fun: &'db Function) -> RcDoc<'d, ()> {
        cx.tcx[*self].to_doc(cx, fun)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Ty {
    fn to_doc(&self, cx: &'db Cx, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(self.to_string(cx.db))
    }
}
