use std::io;

use pretty::RcDoc as D;

use crate::{db::Db, mir::*};

pub(super) fn print(db: &Db, mir: &Mir, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PrettyCx { db, mir };
    let mut docs: Vec<D> = vec![];

    for glob in &mir.globals {
        docs.push(cx.pp_global(glob));
    }

    for f in &mir.fns {
        docs.push(cx.pp_fn(f));
    }

    for sig in &mir.fn_sigs {
        if sig.is_extern {
            docs.push(cx.pp_fn_sig(sig).into_doc());
        }
    }

    D::intersperse(docs, D::hardline().append(D::hardline())).render(80, w)
}

struct PrettyCx<'db> {
    db: &'db Db,
    mir: &'db Mir,
}

impl<'db> PrettyCx<'db> {
    fn pp_global(&mut self, glob: &'db Global) -> D<'db> {
        match &glob.kind {
            GlobalKind::Const(value) => D::text("let")
                .append(D::space())
                .append(global_name(glob.name.as_str()))
                .append(D::text(":"))
                .append(D::space())
                .append(D::text(glob.ty.to_string(self.db)))
                .append(D::space())
                .append(D::text("="))
                .append(D::space())
                .append(pp_const_value(value)),
            GlobalKind::Extern => D::text("let")
                .append(D::space())
                .append(D::text("extern"))
                .append(D::space())
                .append(global_name(glob.name.as_str()))
                .append(D::text(":"))
                .append(D::space())
                .append(D::text(glob.ty.to_string(self.db))),
        }
    }

    fn pp_fn(&mut self, f: &'db Fn) -> D<'db> {
        let sig = &self.mir.fn_sigs[f.sig];

        PrintFn {
            sig: self.pp_fn_sig(sig),
            blocks: f.body.blocks.iter().map(|b| self.pp_blk(b)).collect(),
        }
        .into_doc()
    }

    fn pp_fn_sig(&mut self, sig: &'db FnSig) -> PrintFnSig<'db> {
        PrintFnSig {
            name: global_name(&sig.name),
            params: sig.params.iter().map(|p| D::text(p.ty.to_string(self.db))).collect(),
            ret: D::text(sig.ret.to_string(self.db)),
            is_extern: sig.is_extern,
        }
    }

    fn pp_blk(&mut self, blk: &'db Block) -> PrintBlock<'db> {
        PrintBlock {
            name: D::text(blk.name()),
            insts: blk.insts.iter().map(|i| self.pp_inst(i)).collect(),
        }
    }

    fn pp_inst(&mut self, inst: &Inst) -> D<'db> {
        match inst {
            Inst::Return { value } => D::text("ret").append(D::space()).append(value_name(*value)),
            Inst::Call { value, callee, args } => value_assign(*value)
                .append(D::text("call"))
                .append(D::space())
                .append(value_name(*callee))
                .append(D::text("("))
                .append(D::intersperse(
                    args.iter().map(|a| value_name(*a)),
                    D::text(",").append(D::space()),
                ))
                .append(D::text(")")),
            Inst::Member { value, inner, member } => value_assign(*value)
                .append(value_name(*inner))
                .append(D::text("."))
                .append(D::text(member.as_str())),
            Inst::Load { value, kind } => {
                value_assign(*value).append(D::text("load")).append(D::space()).append(match kind {
                    LoadKind::Fn(id) => global_name(self.mir.fn_sigs[*id].name.as_str()),
                    LoadKind::Global(id) => global_name(self.mir.globals[*id].name.as_str()),
                    LoadKind::Param(id) => D::text("param(")
                        .append(D::text(self.db[*id].name.as_str()))
                        .append(D::text(")")),
                })
            }
            Inst::StrLit { value, lit } => value_assign(*value)
                .append(D::text("\"").append(D::text(lit.as_str())).append(D::text("\""))),
            Inst::IntLit { value, lit } => value_assign(*value).append(D::text(lit.to_string())),
            Inst::BoolLit { value, lit } => value_assign(*value).append(D::text(lit.to_string())),
            Inst::UnitLit { value } => value_assign(*value).append(D::text("{}")),
        }
    }
}

fn pp_const_value<'a>(value: &Const) -> D<'a> {
    match value {
        Const::Str(value) => D::text("\"").append(D::text(value.as_str())).append(D::text("\"")),
        Const::Int(value) => D::text(value.to_string()),
        Const::Bool(value) => D::text(value.to_string()),
        Const::Unit => D::text("{}"),
    }
}

struct PrintFn<'a> {
    sig: PrintFnSig<'a>,
    blocks: Vec<PrintBlock<'a>>,
}

impl<'a> PrintFn<'a> {
    fn into_doc(self) -> D<'a> {
        self.sig.into_doc().append(D::space()).append(
            D::text("{")
                .append(D::hardline())
                .append(
                    D::intersperse(
                        self.blocks.into_iter().map(PrintBlock::into_doc),
                        D::hardline().append(D::hardline()),
                    )
                    .nest(NEST)
                    .group(),
                )
                .append(D::hardline())
                .append(D::text("}")),
        )
    }
}

struct PrintFnSig<'a> {
    name: D<'a>,
    params: Vec<D<'a>>,
    ret: D<'a>,
    is_extern: bool,
}

impl<'a> PrintFnSig<'a> {
    fn into_doc(self) -> D<'a> {
        let mut doc = D::text("fn").append(D::space());

        if self.is_extern {
            doc = doc.append(D::text("extern").append(D::space()));
        }

        doc.append(self.name)
            .append(
                D::text("(")
                    .append(D::intersperse(self.params, D::text(",").append(D::space())))
                    .append(D::text(")").nest(NEST).group()),
            )
            .append(D::space())
            .append(D::text("->"))
            .append(D::space())
            .append(self.ret)
    }
}

struct PrintBlock<'a> {
    name: D<'a>,
    insts: Vec<D<'a>>,
}

impl<'a> PrintBlock<'a> {
    fn into_doc(self) -> D<'a> {
        self.name
            .append(D::text(":"))
            .append(D::hardline())
            .append(D::intersperse(self.insts, D::hardline()).group())
    }
}

fn global_name(name: &str) -> D<'_> {
    D::text("%").append(name)
}

fn value_name<'a>(id: ValueId) -> D<'a> {
    D::text("v").append(id.to_string())
}

fn value_assign<'a>(id: ValueId) -> D<'a> {
    value_name(id).append(D::space()).append(D::text("=")).append(D::space())
}

const NEST: isize = 2;
