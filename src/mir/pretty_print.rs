use pretty::RcDoc;

use super::{Block, Function, Inst, Mir, ValueId};
use crate::{
    ast::BinOp,
    db::{Db, DefId, ScopeLevel},
    mir::BlockId,
    ty::TyKind,
};

pub fn print(db: &Db, mir: &Mir) {
    let doc = RcDoc::intersperse(
        mir.functions.values().map(|f| print_function(db, f)),
        RcDoc::hardline().append(RcDoc::hardline()),
    );

    println!("{}", doc.pretty(80));
}

fn print_function<'d>(db: &Db, fun: &Function) -> RcDoc<'d, ()> {
    let ret_ty = db[fun.id].ty.as_fn().unwrap().ret.to_doc(db, fun);

    RcDoc::text("fn")
        .append(RcDoc::space())
        .append(fun.id().to_doc(db, fun))
        .append(RcDoc::text("("))
        .append(RcDoc::intersperse(
            fun.params().iter().map(|p| {
                p.id.to_doc(db, fun)
                    .append(RcDoc::text(":"))
                    .append(RcDoc::space())
                    .append(db[p.id].ty.to_doc(db, fun))
            }),
            RcDoc::text(",").append(RcDoc::space()),
        ))
        .append(RcDoc::text(")"))
        .append(RcDoc::space())
        .append(ret_ty)
        .append(RcDoc::space())
        .append(RcDoc::text("{"))
        .append(RcDoc::hardline())
        .append(RcDoc::intersperse(
            fun.blocks().iter().map(|b| b.to_doc(db, fun)),
            RcDoc::hardline(),
        ))
        .append(RcDoc::hardline())
        .append("}")
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, db: &'db Db, fun: &'db Function) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Block {
    fn to_doc(&self, db: &'db Db, fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(self.name.as_str())
            .append(RcDoc::text(":"))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.instructions.iter().map(|i| i.to_doc(db, fun)),
                RcDoc::hardline(),
            ))
            .nest(1)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Inst {
    fn to_doc(&self, db: &'db Db, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Self::Return(ret) => {
                RcDoc::text("ret").append(RcDoc::space()).append(ret.value.to_doc(db, fun))
            }
            Self::Br(br) => {
                RcDoc::text("br").append(RcDoc::space()).append(br.target.to_doc(db, fun))
            }
            Self::BrIf(brif) => RcDoc::text("brif")
                .append(RcDoc::space())
                .append(brif.cond.to_doc(db, fun))
                .append(RcDoc::space())
                .append(brif.b1.to_doc(db, fun))
                .append(RcDoc::space())
                .append(brif.b2.to_doc(db, fun)),
            Self::Phi(phi) => RcDoc::text("phi").append(RcDoc::space()).append(RcDoc::intersperse(
                phi.phi_values.iter().map(|(blk, value)| {
                    RcDoc::text("(")
                        .append(blk.to_doc(db, fun))
                        .append(RcDoc::space())
                        .append(RcDoc::text("->"))
                        .append(RcDoc::space())
                        .append(value.to_doc(db, fun))
                        .append(RcDoc::text(")"))
                }),
                RcDoc::space(),
            )),
            Self::Call(call) => value_alloc(db, fun, call.value)
                .append(RcDoc::text("call"))
                .append(RcDoc::space())
                .append(call.callee.to_doc(db, fun))
                .append(RcDoc::space())
                .append(RcDoc::text("("))
                .append(RcDoc::intersperse(
                    call.args.iter().map(|a| a.to_doc(db, fun)),
                    RcDoc::text(",").append(RcDoc::space()),
                ))
                .append(RcDoc::text(")")),
            Self::Load(load) => value_alloc(db, fun, load.value)
                .append(RcDoc::text("load"))
                .append(RcDoc::space())
                .append(load.id.to_doc(db, fun)),
            Self::Bin(bin) => value_alloc(db, fun, bin.value)
                .append(RcDoc::text(bin_instruction_name(bin.op)))
                .append(RcDoc::space())
                .append(bin.lhs.to_doc(db, fun))
                .append(RcDoc::space())
                .append(bin.rhs.to_doc(db, fun)),
            Self::IntLit(lit) => {
                value_alloc(db, fun, lit.value).append(RcDoc::text(lit.value.to_string()))
            }
            Self::BoolLit(lit) => {
                value_alloc(db, fun, lit.value).append(RcDoc::text(lit.value.to_string()))
            }
            Self::UnitLit(lit) => value_alloc(db, fun, lit.value).append(RcDoc::text("()")),
            Self::Unreachable(unr) => {
                value_alloc(db, fun, unr.value).append(RcDoc::text("unreachable"))
            }
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for ValueId {
    fn to_doc(&self, _db: &'db Db, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(format!("v{}", self.0))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for TyKind {
    fn to_doc(&self, db: &'db Db, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(self.to_string(db))
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for BlockId {
    fn to_doc(&self, _db: &'db Db, fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(fun.block(*self).expect("to be a valid BlockId").name.as_str())
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for DefId {
    fn to_doc(&self, db: &'db Db, _fun: &'db Function) -> RcDoc<'d, ()> {
        let def = &db[*self];

        let name = match def.scope.level {
            ScopeLevel::Global => def.qpath.standard_full_name(),
            ScopeLevel::Local(..) => def.qpath.name().to_string(),
        };

        RcDoc::text(format!("@{name}/{self}"))
        // RcDoc::text(format!("@{name}"))
    }
}

fn value_alloc<'db, 'd>(db: &'db Db, fun: &'db Function, value: ValueId) -> RcDoc<'d, ()> {
    value
        .to_doc(db, fun)
        .append(RcDoc::text(":"))
        .append(RcDoc::space())
        .append(fun.value(value).unwrap().ty.to_doc(db, fun))
        .append(RcDoc::space())
        .append(RcDoc::text("="))
        .append(RcDoc::space())
}

fn bin_instruction_name(op: BinOp) -> String {
    let prefix = "i";
    let inst = op.name();
    format!("{prefix}{inst}")
}
