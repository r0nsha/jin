use pretty::RcDoc;

use super::{Block, Function, Instruction, Mir, RegisterId, TyId, Value};
use crate::{ast::BinaryOp, db::Database, ty::Ty};

pub fn print(db: &Database, mir: &Mir) {
    let doc = RcDoc::intersperse(
        mir.functions.iter().map(|f| print_function(db, f)),
        RcDoc::hardline().append(RcDoc::hardline()),
    );

    println!("{}", doc.pretty(80));
}

fn print_function<'d>(db: &Database, fun: &Function) -> RcDoc<'d, ()> {
    let ret_ty = db[db[fun.id].ty].as_function().unwrap().ret.to_doc(db, fun);

    RcDoc::text("fn")
        .append(RcDoc::space())
        .append(RcDoc::text(db[fun.id()].qualified_name.standard_full_name()))
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
        block_name(self)
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
            Self::Return(ret) => {
                RcDoc::text("ret").append(RcDoc::space()).append(ret.value.to_doc(db, fun))
            }
            Self::Jmp(jmp) => RcDoc::text("jmp")
                .append(RcDoc::space())
                .append(block_name(fun.block(jmp.target).unwrap())),
            Self::Jnz(jnz) => RcDoc::text("jnz")
                .append(RcDoc::space())
                .append(jnz.cond.to_doc(db, fun))
                .append(RcDoc::space())
                .append(block_name(fun.block(jnz.b1).unwrap()))
                .append(RcDoc::space())
                .append(block_name(fun.block(jnz.b2).unwrap())),
            Self::Phi(phi) => RcDoc::text("phi").append(RcDoc::space()).append(RcDoc::intersperse(
                phi.values.iter().map(|(blk, value)| {
                    RcDoc::text("(")
                        .append(block_name(fun.block(*blk).unwrap()))
                        .append(RcDoc::space())
                        .append(RcDoc::text("->"))
                        .append(RcDoc::space())
                        .append(value.to_doc(db, fun))
                        .append(RcDoc::text(")"))
                }),
                RcDoc::space(),
            )),
            Self::Call(call) => register_alloc(db, fun, call.register)
                .append(RcDoc::text("call"))
                .append(RcDoc::space())
                .append(call.callee.to_doc(db, fun)),
            Self::Binary(bin) => register_alloc(db, fun, bin.register)
                .append(RcDoc::text(binary_instruction_name(bin.op)))
                .append(RcDoc::space())
                .append(bin.lhs.to_doc(db, fun))
                .append(RcDoc::space())
                .append(bin.rhs.to_doc(db, fun)),
            Self::IntLit(lit) => {
                register_alloc(db, fun, lit.register).append(RcDoc::text(lit.value.to_string()))
            }
            Self::BoolLit(lit) => {
                register_alloc(db, fun, lit.register).append(RcDoc::text(lit.value.to_string()))
            }
            Self::UnitLit(lit) => register_alloc(db, fun, lit.register).append(RcDoc::text("()")),
        }
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Value {
    fn to_doc(&self, db: &'db Database, fun: &'db Function) -> RcDoc<'d, ()> {
        match self {
            Self::Definition(id) => RcDoc::text(db[*id].qualified_name.standard_full_name()),
            Self::Register(id) => id.to_doc(db, fun),
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
        db[*self].to_doc(db, fun)
    }
}

impl<'db, 'd> ToDoc<'db, 'd> for Ty {
    fn to_doc(&self, db: &'db Database, _fun: &'db Function) -> RcDoc<'d, ()> {
        RcDoc::text(self.to_string(db))
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

fn binary_instruction_name(op: BinaryOp) -> String {
    let prefix = "i";
    let inst = op.name();
    format!("{prefix}{inst}")
}

fn block_name<'d>(blk: &Block) -> RcDoc<'d, ()> {
    RcDoc::text(format!("@{}", blk.name))
}
