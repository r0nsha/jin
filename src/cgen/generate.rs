use std::{
    fs::{self, File},
    io::Write,
};

use camino::Utf8PathBuf;
use pretty::{docs, RcDoc};
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    cgen::ty::CTy,
    db::Db,
    hir::const_eval::Const,
    middle::{BinOp, CmpOp, UnOp},
    mir::{Body, Fn, FnSig, FnSigId, GlobalId, GlobalKind, Id, Inst, Mir},
    ty::Ty,
};

const PRELUDE: &str = include_str!("../../jin.c");
const NEST: isize = 2;

pub struct Generator<'db, 'a> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,
    pub fn_decls: Vec<RcDoc<'a>>,
    pub globals: Vec<RcDoc<'a>>,
    pub fn_defs: Vec<RcDoc<'a>>,
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
        Self { body /* locals: FxHashMap::default() */ }
    }

    // #[track_caller]
    // fn local(&self, id: LocalId) -> Ustr {
    //     self.locals.get(&id).copied().unwrap_or_else(|| panic!("local {} to be declared", id))
    // }
}

impl<'db, 'a> Generator<'db, 'a> {
    pub fn run(mut self) -> Utf8PathBuf {
        self.predefine_fns();
        self.define_globals();
        self.define_fns();
        let main_function = self.codegen_main_function();
        self.write_to_file(main_function)
    }

    fn write_to_file(self, main_function: RcDoc<'a>) -> Utf8PathBuf {
        const WIDTH: usize = 80;

        let path = self.db.output_path().with_extension("c");
        let mut file = File::create(self.db.output_path().with_extension("c")).unwrap();

        file.write_all(PRELUDE.as_bytes()).unwrap();
        file.write_all(b"\n").unwrap();

        let fn_decls = RcDoc::intersperse(self.fn_decls, RcDoc::hardline());
        let globals = RcDoc::intersperse(self.globals, RcDoc::hardline());
        let fn_defs = RcDoc::intersperse(self.fn_defs, RcDoc::hardline().append(RcDoc::hardline()));

        let final_doc = RcDoc::intersperse(
            [fn_decls, globals, fn_defs, main_function],
            RcDoc::hardline().append(RcDoc::hardline()),
        );

        final_doc.render(WIDTH, &mut file).expect("writing to work");

        path
    }

    pub fn codegen_main_function(&mut self) -> RcDoc<'a> {
        let main_fn_name = &self.mir.fn_sigs[self.mir.main_fn.expect("to have a main fn")].name;

        RcDoc::text("int main() {")
            .append(
                RcDoc::hardline()
                    .append(RcDoc::intersperse(
                        [RcDoc::text(main_fn_name.as_str()).append(RcDoc::text("();"))],
                        RcDoc::text(";").append(RcDoc::hardline()),
                    ))
                    .nest(NEST)
                    .group(),
            )
            .append(RcDoc::hardline())
            .append(RcDoc::text("}"))
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

    fn add_fn_decl(&mut self, doc: RcDoc<'a>) {
        self.fn_decls.push(doc.append(RcDoc::text(";")));
    }

    fn add_global(&mut self, doc: RcDoc<'a>) {
        self.globals.push(doc.append(RcDoc::text(";")));
    }

    pub fn predefine_fns(&mut self) {
        for sig in &self.mir.fn_sigs {
            self.add_fn_decl(self.codegen_fn_sig(sig));
        }
    }

    pub fn define_globals(&mut self) {
        for glob in &self.mir.globals {
            let cty = glob.ty.cty(self);

            let tyname_doc = cty.append(RcDoc::space()).append(RcDoc::text(glob.name.as_str()));

            let doc = match &glob.kind {
                GlobalKind::Bare { value, body } => {
                    todo!()
                    // if let ExprKind::Const(value) = &body.expr(*value).kind {
                    //     tyname_doc
                    //         .append(RcDoc::space())
                    //         .append(RcDoc::text("="))
                    //         .append(RcDoc::space())
                    //         .append(self.const_value(value))
                    // } else {
                    //     tyname_doc
                    // }
                }
                GlobalKind::Extern => {
                    RcDoc::text("extern").append(RcDoc::space()).append(tyname_doc)
                }
            };

            self.add_global(doc);
        }
    }

    fn codegen_fn_sig(&self, sig: &FnSig) -> RcDoc<'a> {
        let fn_ty = sig.ty.as_fn().expect("a function type");

        let initial =
            if sig.is_extern { RcDoc::text("extern").append(RcDoc::space()) } else { RcDoc::nil() };

        let sig_doc = fn_ty.ret.cty(self).append(RcDoc::space()).append(sig.name.as_str()).append(
            RcDoc::text("(")
                .append(
                    RcDoc::intersperse(
                        sig.params.iter().map(|p| {
                            p.ty.cty(self)
                                .append(RcDoc::space())
                                .append(RcDoc::text(self.db[p.def_id].name.as_str()))
                        }),
                        RcDoc::text(",").append(RcDoc::space()),
                    )
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text(")")),
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

        // TODO: codegen body
        // let body = self.codegen_expr(&mut state, fun.value);

        // TODO:
        // if !self.current_block_is_terminating() {
        //     let ret_value =
        //         if fty.as_fn().unwrap().ret.is_unit() && !state.body.expr(fun.value).ty.is_unit() {
        //             self.unit_value().as_basic_value_enum()
        //         } else {
        //             body
        //         };
        //
        //     self.bx.build_return(Some(&ret_value));
        // }
    }

    fn codegen_inst(&mut self, state: &mut FnState<'db>, inst: &Inst) -> RcDoc<'a> {
        // TODO:
        // if self.current_block_is_terminating() {
        //     return Self::undef_value(expr.ty.cty(self));
        // }

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

    fn const_value(&mut self, value: &Const) -> RcDoc<'a> {
        match value {
            Const::Str(value) => self.str_value(value),
            Const::Int(value) => {
                if value.is_negative() {
                    RcDoc::text("-{value}")
                } else {
                    RcDoc::text(value.to_string())
                }
            }
            Const::Bool(value) => self.bool_value(*value),
            Const::Unit => self.unit_value(),
        }
    }
}

// fn get_int_predicate(op: CmpOp, is_signed: bool) -> IntPredicate {
//     match op {
//         CmpOp::Eq => IntPredicate::EQ,
//         CmpOp::Ne => IntPredicate::NE,
//         CmpOp::Lt => {
//             if is_signed {
//                 IntPredicate::SLT
//             } else {
//                 IntPredicate::ULT
//             }
//         }
//         CmpOp::Le => {
//             if is_signed {
//                 IntPredicate::SLE
//             } else {
//                 IntPredicate::ULE
//             }
//         }
//         CmpOp::Gt => {
//             if is_signed {
//                 IntPredicate::SGT
//             } else {
//                 IntPredicate::UGT
//             }
//         }
//         CmpOp::Ge => {
//             if is_signed {
//                 IntPredicate::SGE
//             } else {
//                 IntPredicate::UGE
//             }
//         }
//     }
// }
