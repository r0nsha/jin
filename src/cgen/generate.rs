use std::{
    fs::{self, File},
    io::Write,
};

use camino::Utf8PathBuf;
use pretty::{docs, RcDoc as D};
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    cgen::ty::CTy,
    db::Db,
    hir::const_eval::Const,
    middle::{BinOp, CmpOp, UnOp},
    mir::{Block, Body, Fn, FnSig, FnSigId, GlobalId, GlobalKind, Id, Inst, Mir},
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
    pub blocks: Vec<D<'db>>,
    // pub locals: FxHashMap<LocalId, Ustr>,
}

impl<'db> FnState<'db> {
    pub fn new(body: &'db Body) -> Self {
        Self { body, blocks: vec![] /* locals: FxHashMap::default() */ }
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

        initial.append(sig_doc)
    }

    pub fn define_fns(&mut self) {
        for fun in &self.mir.fns {
            self.define_fn(fun);
        }
    }

    fn define_fn(&mut self, fun: &'db Fn) {
        let sig = &self.mir.fn_sigs[fun.sig];
        let fty = sig.ty;

        let mut state = FnState::new(&fun.body);

        if !sig.params.is_empty() {
            todo!("fn params");
        }

        // for local in fun.params(self.mir).iter() {
        // state.locals.insert(local.id, local.name);
        // }

        for blk in fun.body.blocks() {
            self.codegen_block(&mut state, blk);
        }

        let doc = self.codegen_fn_sig(sig).append(D::space()).append(
            D::text("{")
                .append(D::hardline())
                .append(
                    D::intersperse(state.blocks, D::hardline().append(D::hardline()))
                        .nest(NEST)
                        .group(),
                )
                .append(D::hardline())
                .append(D::text("}")),
        );

        self.fn_defs.push(doc);
    }

    fn codegen_block(&mut self, state: &mut FnState<'db>, blk: &'db Block) -> D<'db> {
        D::text(blk.name())
            .append(D::text(blk.id().to_string()))
            .append(D::text(":"))
            .append(D::hardline())
            .append(
                D::intersperse(
                    blk.insts().iter().map(|i| self.codegen_inst(state, i)),
                    D::hardline(),
                )
                .group(),
            )
    }

    fn codegen_inst(&mut self, state: &mut FnState<'db>, inst: &Inst) -> D<'db> {
        match inst {
            Inst::Call { value, callee, args } => todo!(),
            Inst::LoadGlobal { value, id } => todo!(),
            Inst::StrLit { value, lit } => todo!(),
            Inst::IntLit { value, lit } => todo!(),
            Inst::BoolLit { value, lit } => todo!(),
            Inst::UnitLit { value } => todo!(),
        }
    }

    #[track_caller]
    fn function(&self, id: FnSigId) {
        todo!()
        // self.functions
        //     .get(&id)
        //     .copied()
        //     .unwrap_or_else(|| panic!("function {} to be declared", self.tir.fn_sigs[id].name))
    }

    #[track_caller]
    fn global(&self, id: GlobalId) {
        todo!()
        // self.globals
        //     .get(&id)
        //     .copied()
        //     .unwrap_or_else(|| panic!("global {} to be declared", self.tir.globals[id].name))
    }

    fn const_value(&mut self, value: &Const) -> D<'db> {
        match value {
            Const::Str(value) => self.str_value(value),
            Const::Int(value) => {
                if value.is_negative() {
                    D::text("-{value}")
                } else {
                    D::text(value.to_string())
                }
            }
            Const::Bool(value) => self.bool_value(*value),
            Const::Unit => self.unit_value(),
        }
    }
}
