use std::io;

use compiler_core::db::Db;
use compiler_data_structures::index_vec::Key;
use pretty::RcDoc as D;
use ustr::ustr;

use crate::{
    Block, Body, Const, Fn, FnSig, Global, GlobalKind, Inst, Mir, Pat, RtCallKind, ValueId,
    ValueKind,
};

pub(super) fn print(db: &Db, mir: &Mir, w: &mut impl io::Write) -> io::Result<()> {
    let mut cx = PrettyCx { db, mir };
    let mut docs: Vec<D> = vec![];

    for glob in mir.globals.values() {
        docs.push(cx.pp_global(glob));
    }

    for f in mir.fns.values() {
        docs.push(cx.pp_fn(f));
    }

    for sig in mir.fn_sigs.values() {
        if sig.ty.as_fn().unwrap().is_extern() {
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
            GlobalKind::Static(body) => D::text("let")
                .append(D::space())
                .append(Self::global(glob.name.as_str()))
                .append(D::text(":"))
                .append(D::space())
                .append(D::text(glob.ty.to_string(self.db)))
                .append(D::space())
                .append(
                    D::text("{")
                        .append(D::hardline())
                        .append(
                            D::intersperse(
                                body.blocks.iter().map(|b| self.pp_block(body, b).into_doc()),
                                D::hardline().append(D::hardline()),
                            )
                            .group(),
                        )
                        .append(D::hardline())
                        .append(D::text("}")),
                ),
            GlobalKind::Extern => D::text("let")
                .append(D::space())
                .append(D::text("extern"))
                .append(D::space())
                .append(Self::global(glob.name.as_str()))
                .append(D::text(":"))
                .append(D::space())
                .append(D::text(glob.ty.to_string(self.db))),
        }
    }

    fn pp_fn(&mut self, f: &'db Fn) -> D<'db> {
        let sig = &self.mir.fn_sigs[f.sig];

        PrintFn {
            sig: self.pp_fn_sig(sig),
            blocks: f.body.blocks.iter().map(|b| self.pp_block(&f.body, b)).collect(),
        }
        .into_doc()
    }

    fn pp_fn_sig(&mut self, sig: &'db FnSig) -> PrintFnSig<'db> {
        let ty = sig.ty.as_fn().unwrap();

        PrintFnSig {
            name: Self::global(&sig.mangled_name),
            params: sig
                .params
                .iter()
                .map(|p| {
                    D::text(match &p.pat {
                        Pat::Name(name) => name.word.name().as_str(),
                        Pat::Discard(_) => "_",
                    })
                    .append(D::text(":"))
                    .append(D::space())
                    .append(D::text(p.ty.to_string(self.db)))
                })
                .collect(),
            ret: D::text(sig.ty.as_fn().unwrap().ret.to_string(self.db)),
            is_extern: ty.is_extern(),
            is_c_variadic: ty.is_c_variadic(),
        }
    }

    fn pp_block(&mut self, body: &'db Body, block: &'db Block) -> PrintBlock<'db> {
        PrintBlock {
            name: D::text(block.display_name()),
            insts: block.insts.iter().map(|i| self.pp_inst(body, i)).collect(),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn pp_inst(&mut self, body: &'db Body, inst: &'db Inst) -> D<'db> {
        match inst {
            Inst::StackAlloc { value, init, .. } => self
                .value_assign(body, *value)
                .append(D::text("stackalloc"))
                .append(D::space())
                .append(
                    init.map(|init| self.value(body, init))
                        .unwrap_or_else(|| D::text("uninitialized")),
                ),
            Inst::Store { value, target, .. } => D::text("store")
                .append(D::space())
                .append(self.value(body, *value))
                .append(D::text(" in "))
                .append(self.value(body, *target)),
            Inst::Alloc { value, .. } => self
                .value_assign(body, *value)
                .append(D::text("alloc"))
                .append(D::space())
                .append(body.value(*value).ty.to_string(self.db)),
            Inst::SliceAlloc { value, cap, .. } => self
                .value_assign(body, *value)
                .append(D::text("slice_alloc, cap: "))
                .append(self.value(body, *cap)),
            Inst::SliceIndex { value, slice, index, .. } => self.value_assign(body, *value).append(
                D::text("slice_index")
                    .append(D::space())
                    .append(self.value(body, *slice))
                    .append(D::text("["))
                    .append(self.value(body, *index))
                    .append(D::text("]")),
            ),
            Inst::SliceSlice { value, slice, low, high, .. } => {
                self.value_assign(body, *value).append(
                    D::text("slice_slice")
                        .append(D::space())
                        .append(self.value(body, *slice))
                        .append(D::text("["))
                        .append(self.value(body, *low))
                        .append(D::text(".."))
                        .append(self.value(body, *high))
                        .append(D::text("]")),
                )
            }
            Inst::SliceStore { slice, index, value, .. } => D::text("slice_store")
                .append(D::space())
                .append(self.value(body, *value))
                .append(D::text(" in "))
                .append(self.value(body, *slice))
                .append(D::text("["))
                .append(self.value(body, *index))
                .append(D::text("]")),
            Inst::Drop { value, .. } => {
                D::text("drop").append(D::space()).append(self.value(body, *value))
            }
            Inst::Free { value, .. } => {
                D::text("free").append(D::space()).append(self.value(body, *value))
            }
            Inst::IncRef { value, .. } => {
                D::text("incref").append(D::space()).append(self.value(body, *value))
            }
            Inst::DecRef { value, .. } => {
                D::text("decref").append(D::space()).append(self.value(body, *value))
            }
            Inst::Br { target } => {
                D::text("br").append(D::space()).append(D::text(body.block(*target).display_name()))
            }
            Inst::BrIf { cond, then, otherwise, .. } => D::text("brif")
                .append(D::text("("))
                .append(self.value(body, *cond))
                .append(D::text(")"))
                .append(D::space())
                .append(body.block(*then).display_name())
                .append(if let Some(o) = otherwise {
                    D::text(", ").append(D::text(body.block(*o).display_name()))
                } else {
                    D::nil()
                }),
            Inst::Switch { cond, blocks, .. } => D::text("switch")
                .append(D::text("("))
                .append(self.value(body, *cond))
                .append(D::text(")"))
                .append(D::space())
                .append(D::text("["))
                .append(D::intersperse(
                    blocks.iter().map(|&b| D::text(body.block(b).display_name())),
                    D::text(", "),
                ))
                .append(D::text("]")),
            Inst::Return { value, .. } => {
                D::text("ret").append(D::space()).append(self.value(body, *value))
            }
            Inst::Call { value, callee, args, .. } => self
                .value_assign(body, *value)
                .append(D::text("call"))
                .append(D::space())
                .append(self.value(body, *callee))
                .append(D::text("("))
                .append(D::intersperse(
                    args.iter().map(|a| self.value(body, *a)),
                    D::text(",").append(D::space()),
                ))
                .append(D::text(")")),
            Inst::RtCall { value, kind, .. } => self
                .value_assign(body, *value)
                .append(D::text("rtcall"))
                .append(D::space())
                .append(D::text(kind.as_str()))
                .append(D::text("("))
                .append(D::intersperse(
                    match kind {
                        RtCallKind::Panic { msg } => {
                            vec![self.value(body, *msg)]
                        }
                        RtCallKind::SliceGrow { slice, new_cap } => {
                            vec![self.value(body, *slice), self.value(body, *new_cap)]
                        }
                        RtCallKind::SliceUtf8Validate { slice } => {
                            vec![self.value(body, *slice)]
                        }
                    },
                    D::text(",").append(D::space()),
                ))
                .append(D::text(")")),
            Inst::Binary { value, lhs, rhs, op, .. } => self
                .value_assign(body, *value)
                .append(self.value(body, *lhs))
                .append(D::space())
                .append(D::text(op.as_str()))
                .append(D::space())
                .append(self.value(body, *rhs)),
            Inst::Unary { value, inner, op, .. } => self
                .value_assign(body, *value)
                .append(D::text(op.as_str()))
                .append(self.value(body, *inner)),
            Inst::Convert { value, source, target, .. } => self
                .value_assign(body, *value)
                .append(D::text("convert"))
                .append(D::space())
                .append(self.value(body, *source))
                .append(D::space())
                .append(D::text("to"))
                .append(D::space())
                .append(D::text(target.to_string(self.db))),
            Inst::Cast { value, source, target, .. } => self
                .value_assign(body, *value)
                .append(D::text("cast"))
                .append(D::space())
                .append(self.value(body, *source))
                .append(D::space())
                .append(D::text("to"))
                .append(D::space())
                .append(D::text(target.to_string(self.db))),
            Inst::StrLit { value, lit, .. } => self
                .value_assign(body, *value)
                .append(D::text("\"").append(D::text(lit.as_str())).append(D::text("\""))),
            Inst::Unreachable { .. } => D::text("unreachable"),
        }
    }

    fn value_assign(&self, body: &'db Body, id: ValueId) -> D<'db> {
        self.value(body, id).append(D::space()).append(D::text("=")).append(D::space())
    }

    fn global(name: &'db str) -> D<'db> {
        D::text("%").append(name)
    }

    fn value(&self, body: &'db Body, value_id: ValueId) -> D<'db> {
        let value = body.value(value_id);

        match &value.kind {
            ValueKind::Register(name) => {
                D::text(format!("{}{}", name.unwrap_or(ustr("v")), value_id))
            }
            ValueKind::Param(id, idx) => {
                if id.is_null() {
                    D::text(format!("param{idx}"))
                } else {
                    D::text(self.db[*id].name.as_str())
                }
            }
            ValueKind::Local(id) => D::text(self.db[*id].name.as_str()),
            ValueKind::Global(id) => Self::global(&self.mir.globals[*id].name),
            ValueKind::Fn(id) => Self::global(&self.mir.fn_sigs[*id].mangled_name),
            ValueKind::Const(value) => pp_const_value(value),
            ValueKind::Field(value, field) => {
                self.value(body, *value).append(D::text(format!(".{field}")))
            }
            ValueKind::Variant(value, id) => {
                self.value(body, *value).append(D::text(format!(".{}", self.db[*id].name)))
            }
            ValueKind::Deref(value) => self.value(body, *value).append(D::text(".0")),
        }
    }
}

fn pp_const_value<'a>(value: &Const) -> D<'a> {
    match value {
        Const::Str(v) => D::text("\"").append(D::text(v.as_str())).append(D::text("\"")),
        Const::Int(v) => D::text(v.to_string()),
        Const::Float(v) => D::text(v.to_string()),
        Const::Bool(v) => D::text(v.to_string()),
        Const::Unit => D::text("()"),
    }
}

struct PrintFn<'a> {
    sig: PrintFnSig<'a>,
    blocks: Vec<PrintBlock<'a>>,
}

impl<'a> PrintFn<'a> {
    fn into_doc(self) -> D<'a> {
        self.sig.into_doc().append(D::space()).append(D::text("=")).append(D::space()).append(
            D::text("{")
                .append(D::hardline())
                .append(
                    D::intersperse(
                        self.blocks.into_iter().map(PrintBlock::into_doc),
                        D::hardline().append(D::hardline()),
                    )
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
    is_c_variadic: bool,
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
                    .append(if self.is_c_variadic { D::text(", ..") } else { D::nil() })
                    .append(D::text(")").nest(NEST).group()),
            )
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
            .nest(NEST)
    }
}

const NEST: isize = 2;
