use std::{
    fs::{self, File},
    io::Write,
};

use camino::Utf8PathBuf;
use pretty::{docs, RcDoc as D};
use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{
    cgen::{ty::CTy, util::str_value},
    db::Db,
    hir::const_eval::Const,
    middle::{BinOp, CmpOp, UnOp},
    mir::{
        Block, Body, Fn, FnSig, FnSigId, GlobalId, GlobalKind, Inst, LoadKind, Mir, Value, ValueId,
    },
    ty::Ty,
};

const PRELUDE: &str = include_str!("../../jin.c");
const NEST: isize = 2;

pub struct Generator<'db> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,
    pub fn_decls: Vec<D<'db>>,
    pub globals: Vec<D<'db>>,
    pub fn_defs: Vec<D<'db>>,
}

// #[derive(Debug, Clone, Copy)]
// pub enum Local<'cx> {
//     StackAlloc(PointerValue<'cx>, BasicTypeEnum<'cx>),
//     Value(BasicValueEnum<'cx>),
// }

#[derive(Debug, Clone)]
pub struct FnState<'db> {
    pub body: &'db Body,
    // pub locals: FxHashMap<LocalId, Ustr>,
}

impl<'db> FnState<'db> {
    pub fn new(body: &'db Body) -> Self {
        Self {
            body,
            // locals: FxHashMap::default()
        }
    }

    // #[track_caller]
    // fn local(&self, id: LocalId) -> Ustr {
    //     self.locals.get(&id).copied().unwrap_or_else(|| panic!("local {} to be declared", id))
    // }
}

impl<'db> Generator<'db> {
    pub fn run(mut self) -> Utf8PathBuf {
        self.predefine_fns();
        self.define_globals();
        self.define_fns();
        let main_function = self.codegen_main_function();
        self.write_to_file(main_function)
    }

    fn write_to_file(self, main_function: D<'db>) -> Utf8PathBuf {
        const WIDTH: usize = 80;

        let path = self.db.output_path().with_extension("c");
        let mut file = File::create(self.db.output_path().with_extension("c")).unwrap();

        file.write_all(PRELUDE.as_bytes()).unwrap();
        file.write_all(b"\n").unwrap();

        let fn_decls = D::intersperse(self.fn_decls, D::hardline());
        let globals = D::intersperse(self.globals, D::hardline());
        let fn_defs = D::intersperse(self.fn_defs, D::hardline().append(D::hardline()));

        let final_doc = D::intersperse(
            [fn_decls, globals, fn_defs, main_function],
            D::hardline().append(D::hardline()),
        );

        final_doc.render(WIDTH, &mut file).expect("writing to work");

        path
    }

    pub fn codegen_main_function(&mut self) -> D<'db> {
        let main_fn_name = &self.mir.fn_sigs[self.mir.main_fn.expect("to have a main fn")].name;

        D::text("int main() {")
            .append(
                D::hardline()
                    .append(D::intersperse(
                        [D::text(main_fn_name.as_str()).append(D::text("();"))],
                        D::text(";").append(D::hardline()),
                    ))
                    .nest(NEST)
                    .group(),
            )
            .append(D::hardline())
            .append(D::text("}"))
    }

    pub fn init_lazy_globals(
        &mut self,
        // main_function_value: FunctionValue<'cx>,
        // prologue_block: BasicBlock<'cx>,
        // start_block: BasicBlock<'cx>,
    ) {
        todo!()
        // for glob in &self.tir.globals {
        //     match &glob.kind {
        //         GlobalKind::Bare { value, body } => {
        //             if !body.expr(*value).kind.is_const() {
        //                 let mut state =
        //                     FnState::new(body, main_function_value, prologue_block, start_block);
        //                 let value = self.codegen_expr(&mut state, *value);
        //                 self.bx.build_store(self.global(glob.id).as_pointer_value(), value);
        //             }
        //         }
        //         GlobalKind::Extern => (),
        //     }
        // }
    }

    fn add_fn_decl(&mut self, doc: D<'db>) {
        self.fn_decls.push(doc.append(D::text(";")));
    }

    fn add_global(&mut self, doc: D<'db>) {
        self.globals.push(doc.append(D::text(";")));
    }

    pub fn predefine_fns(&mut self) {
        for sig in &self.mir.fn_sigs {
            self.add_fn_decl(self.codegen_fn_sig(sig));
        }
    }

    pub fn define_globals(&mut self) {
        for glob in &self.mir.globals {
            let cty = glob.ty.cty(self);

            let tyname_doc = cty.append(D::space()).append(D::text(glob.name.as_str()));

            let doc = match &glob.kind {
                GlobalKind::Bare { value, body } => {
                    todo!()
                    // if let ExprKind::Const(value) = &body.expr(*value).kind {
                    //     tyname_doc
                    //         .append(D::space())
                    //         .append(D::text("="))
                    //         .append(D::space())
                    //         .append(self.const_value(value))
                    // } else {
                    //     tyname_doc
                    // }
                }
                GlobalKind::Extern => D::text("extern").append(D::space()).append(tyname_doc),
            };

            self.add_global(doc);
        }
    }

    fn codegen_fn_sig(&self, sig: &FnSig) -> D<'db> {
        let fn_ty = sig.ty.as_fn().expect("a function type");

        let initial = if sig.is_extern { D::text("extern").append(D::space()) } else { D::nil() };

        let sig_doc = fn_ty.ret.cty(self).append(D::space()).append(sig.name.as_str()).append(
            D::text("(")
                .append(
                    D::intersperse(
                        sig.params.iter().map(|p| {
                            p.ty.cty(self)
                                .append(D::space())
                                .append(D::text(self.db[p.def_id].name.as_str()))
                        }),
                        D::text(",").append(D::space()),
                    )
                    .nest(1)
                    .group(),
                )
                .append(D::text(")")),
        );

        initial.append(sig_doc).group()
    }

    pub fn define_fns(&mut self) {
        for fun in &self.mir.fns {
            self.define_fn(fun);
        }
    }

    fn define_fn(&mut self, fun: &'db Fn) {
        let sig = &self.mir.fn_sigs[fun.sig];

        let mut state = FnState::new(&fun.body);

        if !sig.params.is_empty() {
            todo!("fn params");
        }

        // TODO: params
        // for local in fun.params(self.mir).iter() {
        // state.locals.insert(local.id, local.name);
        // }

        let block_docs: Vec<D> =
            fun.body.blocks().iter().map(|blk| self.codegen_block(&mut state, blk)).collect();

        let doc = self.codegen_fn_sig(sig).append(D::space()).append(
            D::text("{")
                .append(D::hardline())
                .append(
                    D::intersperse(block_docs, D::hardline().append(D::hardline()))
                        .nest(NEST)
                        .group(),
                )
                .append(D::hardline())
                .append(D::text("}"))
                .group(),
        );

        self.fn_defs.push(doc);
    }

    fn codegen_block(&mut self, state: &mut FnState<'db>, blk: &'db Block) -> D<'db> {
        D::text(blk.name()).append(D::text(":;")).append(D::hardline()).append(
            D::intersperse(blk.insts().iter().map(|i| self.codegen_inst(state, i)), D::hardline())
                .group(),
        )
    }

    fn codegen_inst(&mut self, state: &mut FnState<'db>, inst: &'db Inst) -> D<'db> {
        match inst {
            Inst::Return { value } => {
                statement(|| D::text("return").append(D::space()).append(value_name(*value)))
            }
            Inst::Call { value, callee, args } => self.value_assign(state, *value, || {
                value_name(*callee)
                    .append(D::text("("))
                    .append(
                        D::intersperse(
                            args.iter().copied().map(value_name),
                            D::text(",").append(D::space()),
                        )
                        .nest(1)
                        .group(),
                    )
                    .append(D::text(")"))
            }),
            Inst::Member { value, inner, member } => self.value_assign(state, *value, || {
                value_name(*inner).append(D::text(".")).append(D::text(member.as_str()))
            }),
            Inst::Load { value, kind } => self.value_assign(state, *value, || match kind {
                LoadKind::Fn(id) => D::text(self.mir.fn_sigs[*id].name.as_str()),
                LoadKind::Param(_) => todo!(),
            }),
            Inst::StrLit { value, lit } => self.value_assign(state, *value, || str_value(lit)),
            Inst::IntLit { value, lit } => {
                self.value_assign(state, *value, || D::text(lit.to_string()))
            }
            Inst::BoolLit { value, lit } => {
                self.value_assign(state, *value, || D::text(if *lit { "true" } else { "false" }))
            }
            Inst::UnitLit { value } => self.value_assign(state, *value, || D::text("{0}")),
        }
    }

    fn value_assign(
        &self,
        state: &FnState<'db>,
        id: ValueId,
        f: impl FnOnce() -> D<'db>,
    ) -> D<'db> {
        statement(|| {
            let value = state.body.value(id);

            value
                .ty
                .cdecl(self, value_name(value.id))
                // .append(D::space())
                // .append(value_name(value.id))
                .append(D::space())
                .append(D::text("="))
                .append(D::space())
                .append(f())
        })
    }
}

fn statement<'a>(f: impl FnOnce() -> D<'a>) -> D<'a> {
    f().append(D::text(";"))
}

fn value_name<'a>(id: ValueId) -> D<'a> {
    D::text("v").append(id.to_string())
}
