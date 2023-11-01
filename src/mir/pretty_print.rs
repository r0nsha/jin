use std::io;

use pretty::RcDoc;

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

    let mut docs: Vec<RcDoc> = vec![];

    for f in &mir.fns {
        let fn_doc = PrettyCx { db, mir, body: &f.body }.pp_fn(f);
        docs.push(fn_doc);
    }

    RcDoc::intersperse(docs, RcDoc::hardline().append(RcDoc::hardline())).render(80, w)
}

struct PrettyCx<'db> {
    db: &'db Db,
    mir: &'db Mir,
    body: &'db Body,
}

impl<'db> PrettyCx<'db> {
    fn pp_fn(&mut self, f: &'db Fn) -> RcDoc<'db> {
        let sig = &self.mir.fn_sigs[f.sig];

        PrintFn {
            name: global_name(&sig.name),
            params: sig.params.iter().map(|p| RcDoc::text(p.ty.to_string(self.db))).collect(),
            ret: RcDoc::text(sig.ret.to_string(self.db)),
            blocks: f.body.blocks.iter().map(|b| self.pp_blk(b)).collect(),
        }
        .into_doc()
    }

    fn pp_blk(&mut self, blk: &'db Block) -> PrintBlock<'db> {
        PrintBlock {
            name: RcDoc::text(blk.name()),
            insts: blk.insts.iter().map(|i| self.pp_inst(i)).collect(),
        }
    }

    fn pp_inst(&mut self, inst: &Inst) -> RcDoc<'db> {
        RcDoc::text("todo: inst")
    }
}

struct PrintFn<'a> {
    name: RcDoc<'a>,
    params: Vec<RcDoc<'a>>,
    ret: RcDoc<'a>,
    blocks: Vec<PrintBlock<'a>>,
}

impl<'a> PrintFn<'a> {
    fn into_doc(self) -> RcDoc<'a> {
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
            .append(self.ret)
            .append(RcDoc::space())
            .append(
                RcDoc::text("{")
                    .append(RcDoc::hardline())
                    .append(
                        RcDoc::intersperse(
                            self.blocks.into_iter().map(PrintBlock::into_doc),
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
    insts: Vec<RcDoc<'a>>,
}

impl<'a> PrintBlock<'a> {
    fn into_doc(self) -> RcDoc<'a> {
        self.name
            .append(RcDoc::text(":"))
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(self.insts, RcDoc::hardline()).group())
    }
}

fn global_name(name: &str) -> RcDoc<'_> {
    RcDoc::text("%").append(name)
}

fn value_name<'a>(id: ValueId) -> RcDoc<'a> {
    RcDoc::text("v").append(id.to_string())
}

const NEST: isize = 2;
