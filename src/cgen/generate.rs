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
    tir::{Body, ExprId, ExprKind, Fn, FnSig, FnSigId, GlobalId, GlobalKind, Id, LocalId, Tir},
    ty::Ty,
};

const PRELUDE: &str = include_str!("../../jin.c");
const NEST: isize = 2;

pub struct Generator<'db, 'a> {
    pub db: &'db mut Db,
    pub tir: &'db Tir,
    pub fn_decls: Vec<RcDoc<'a>>,
    pub globals: Vec<RcDoc<'a>>,
    pub fn_defs: Vec<RcDoc<'a>>,
    // pub functions: FxHashMap<FnSigId, FunctionValue<'cx>>,
    // pub globals: FxHashMap<GlobalId, GlobalValue<'cx>>,
    // pub static_strs: UstrMap<PointerValue<'cx>>,
    // pub static_str_slices: UstrMap<PointerValue<'cx>>,
}

// #[derive(Debug, Clone, Copy)]
// pub enum Local<'cx> {
//     Alloca(PointerValue<'cx>, BasicTypeEnum<'cx>),
//     Value(BasicValueEnum<'cx>),
// }

#[derive(Debug, Clone)]
pub struct FnState<'db> {
    pub body: &'db Body,
    pub locals: FxHashMap<LocalId, Ustr>,
}

impl<'db> FnState<'db> {
    pub fn new(body: &'db Body) -> Self {
        Self { body, locals: FxHashMap::default() }
    }

    #[track_caller]
    fn local(&self, id: LocalId) -> Ustr {
        self.locals.get(&id).copied().unwrap_or_else(|| panic!("local {} to be declared", id))
    }
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
        let main_fn_name = &self.tir.fn_sigs[self.tir.main_fn.expect("to have a main fn")].name;

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
        for sig in &self.tir.fn_sigs {
            self.add_fn_decl(self.codegen_fn_sig(sig));
        }
    }

    pub fn define_globals(&mut self) {
        for glob in &self.tir.globals {
            let cty = glob.ty.cty(self);

            let tyname_doc = cty.append(RcDoc::space()).append(RcDoc::text(glob.name.as_str()));

            let doc = match &glob.kind {
                GlobalKind::Bare { value, body } => {
                    if let ExprKind::Const(value) = &body.expr(*value).kind {
                        tyname_doc
                            .append(RcDoc::space())
                            .append(RcDoc::text("="))
                            .append(RcDoc::space())
                            .append(self.const_value(value))
                    } else {
                        tyname_doc
                    }
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
                        ", ",
                    )
                    .nest(1)
                    .group(),
                )
                .append(RcDoc::text(")")),
        );

        initial.append(sig_doc)
    }

    pub fn define_fns(&mut self) {
        for fun in &self.tir.fns {
            self.define_fn(fun);
        }
    }

    fn define_fn(&mut self, fun: &'db Fn) {
        let sig = &self.tir.fn_sigs[fun.sig];
        let fty = sig.ty;

        let mut state = FnState::new(&fun.body);

        for local in fun.params(self.tir).iter() {
            state.locals.insert(local.id, local.name);
        }

        let body = self.codegen_expr(&mut state, fun.value);

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

    fn codegen_expr(&mut self, state: &mut FnState<'db>, expr: ExprId) -> RcDoc<'a> {
        let expr = &state.body.expr(expr);

        // TODO:
        // if self.current_block_is_terminating() {
        //     return Self::undef_value(expr.ty.cty(self));
        // }

        match &expr.kind {
            ExprKind::Let { id, def_id: _, value } => {
                todo!();
                // let local = state.body.local(*id);
                // let ty = local.ty.cty(self);
                //
                // let ptr = self.build_stack_alloc(state, ty, &local.name);
                // let value = self.codegen_expr(state, *value);
                // self.bx.build_store(ptr, value);
                //
                // state.locals.insert(*id, Local::Alloca(ptr, ty));
                //
                // self.unit_value().into()
            }
            ExprKind::If { cond, then, otherwise } => {
                todo!();
                // let cond = self.codegen_expr(state, *cond).into_int_value();
                //
                // let then_block = self.append_block(state, "if_then");
                // let else_block = self.append_block(state, "if_else");
                // let merge_block = self.append_block(state, "if_merge");
                //
                // self.bx.build_conditional_branch(cond, then_block, else_block);
                //
                // self.start_block(state, then_block);
                // let then_value = self.codegen_br(state, *then);
                // self.bx.build_unconditional_branch(merge_block);
                //
                // self.start_block(state, else_block);
                // let else_value = self.codegen_br(state, *otherwise);
                //
                // self.bx.build_unconditional_branch(merge_block);
                // self.start_block(state, merge_block);
                //
                // let ty = expr.ty.cty(self);
                // match (then_value, else_value) {
                //     (Some(then_value), Some(else_value)) => {
                //         let phi = self.bx.build_phi(ty, "phi");
                //         phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                //         phi.as_basic_value()
                //     }
                //     (Some(then_value), None) => then_value,
                //     (None, Some(else_value)) => else_value,
                //     (None, None) => {
                //         self.build_unreachable();
                //         Self::undef_value(ty)
                //     }
                // }
            }
            ExprKind::Block { exprs } => {
                todo!();
                // let mut result = self.unit_value().as_basic_value_enum();

                // for expr in exprs {
                //     result = self.codegen_expr(state, *expr);
                // }

                // result
            }
            ExprKind::Return { value } => {
                todo!();
                // let value = self.codegen_expr(state, *value);
                // self.bx.build_return(Some(&value));
                // Self::undef_value(expr.ty.cty(self))
            }
            ExprKind::Call { callee, args } => {
                todo!();
                // let args: Vec<_> =
                //     args.iter().map(|arg| self.codegen_expr(state, *arg).into()).collect();
                //
                // // TODO: this doesn't take indirect calls (function pointers) into account
                // let callee =
                //     self.codegen_expr(state, *callee).as_any_value_enum().into_function_value();
                //
                // let result = self.bx.build_direct_call(callee, &args, "call");
                //
                // let result_value =
                //     result.try_as_basic_value().expect_left("expected a return value");
                //
                // // TODO: remove this debug printf call
                // {
                //     let printf = self.module.get_function("printf").unwrap_or_else(|| {
                //         self.module.add_function(
                //             "printf",
                //             self.layout.int_ty.fn_type(
                //                 &[self.context.ptr_type(AddressSpace::default()).into()],
                //                 true,
                //             ),
                //             Some(Linkage::External),
                //         )
                //     });
                //
                //     self.bx.build_direct_call(
                //         printf,
                //         &[
                //             self.bx
                //                 .build_global_string_ptr("result = %d\n\0", "fmt")
                //                 .as_pointer_value()
                //                 .into(),
                //             if let BasicValueEnum::IntValue(v) = result_value {
                //                 self.bx
                //                     .build_int_cast_sign_flag(
                //                         v,
                //                         self.layout.int_ty,
                //                         false,
                //                         "cast_to_i64",
                //                     )
                //                     .into()
                //             } else {
                //                 result_value.into()
                //             },
                //         ],
                //         "printf_call",
                //     );
                // }
                //
                // result_value
            }
            ExprKind::Binary { lhs, rhs, op } => {
                todo!();
                // let ty = expr.ty;
                // let lhs = self.codegen_expr(state, *lhs).into_int_value();
                //
                // match op {
                //     BinOp::And => {
                //         // if lhs { rhs } else { false }
                //         let then_block = self.append_block(state, "and_then");
                //         let else_block = self.append_block(state, "and_else");
                //         let merge_block = self.append_block(state, "and_merge");
                //
                //         self.bx.build_conditional_branch(lhs, then_block, else_block);
                //
                //         self.start_block(state, then_block);
                //         let then_value = self.codegen_expr(state, *rhs);
                //         self.bx.build_unconditional_branch(merge_block);
                //
                //         self.start_block(state, else_block);
                //         let else_value = self.bool_value(false);
                //
                //         self.bx.build_unconditional_branch(merge_block);
                //         self.start_block(state, merge_block);
                //
                //         let ty = expr.ty.cty(self);
                //         let phi = self.bx.build_phi(ty, "phi");
                //         phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                //         phi.as_basic_value()
                //     }
                //     BinOp::Or => {
                //         // if !lhs { rhs } else { false }
                //         let then_block = self.append_block(state, "or_then");
                //         let else_block = self.append_block(state, "or_else");
                //         let merge_block = self.append_block(state, "or_merge");
                //
                //         self.bx.build_conditional_branch(lhs, then_block, else_block);
                //
                //         self.start_block(state, then_block);
                //         let then_value = self.bool_value(true);
                //         self.bx.build_unconditional_branch(merge_block);
                //
                //         self.start_block(state, else_block);
                //         let else_value = self.codegen_expr(state, *rhs);
                //
                //         self.bx.build_unconditional_branch(merge_block);
                //         self.start_block(state, merge_block);
                //
                //         let ty = expr.ty.cty(self);
                //         let phi = self.bx.build_phi(ty, "phi");
                //         phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                //         phi.as_basic_value()
                //     }
                //     _ => {
                //         let rhs = self.codegen_expr(state, *rhs).into_int_value();
                //
                //         match op {
                //             BinOp::Add => self.bx.build_int_add(lhs, rhs, "result").into(),
                //             BinOp::Sub => self.bx.build_int_sub(lhs, rhs, "result").into(),
                //             BinOp::Mul => self.bx.build_int_mul(lhs, rhs, "result").into(),
                //             BinOp::Div => {
                //                 if ty.is_uint() {
                //                     self.bx.build_int_unsigned_div(lhs, rhs, "result").into()
                //                 } else {
                //                     self.bx.build_int_signed_div(lhs, rhs, "result").into()
                //                 }
                //             }
                //             BinOp::Rem => {
                //                 if ty.is_uint() {
                //                     self.bx.build_int_unsigned_rem(lhs, rhs, "result").into()
                //                 } else {
                //                     self.bx.build_int_signed_rem(lhs, rhs, "result").into()
                //                 }
                //             }
                //             BinOp::Shl => self.bx.build_left_shift(lhs, rhs, "result").into(),
                //             BinOp::Shr => {
                //                 self.bx.build_right_shift(lhs, rhs, ty.is_int(), "result").into()
                //             }
                //             BinOp::BitAnd => self.bx.build_and(lhs, rhs, "result").into(),
                //             BinOp::BitOr => self.bx.build_or(lhs, rhs, "result").into(),
                //             BinOp::BitXor => self.bx.build_xor(lhs, rhs, "result").into(),
                //             BinOp::Cmp(op) => {
                //                 let pred = get_int_predicate(*op, ty.is_int());
                //                 self.bx.build_int_compare(pred, lhs, rhs, "result").into()
                //             }
                //             BinOp::And | BinOp::Or => unreachable!(),
                //         }
                //     }
                // }
            }
            ExprKind::Unary { value, op } => {
                todo!();
                // let value = self.codegen_expr(state, *value).into_int_value();
                // match op {
                //     UnOp::Neg => self.bx.build_int_neg(value, "result").into(),
                //     UnOp::Not => self.bx.build_not(value, "result").into(),
                // }
            }
            ExprKind::Cast { value, target } => {
                todo!();
                // let source_ty = state.body.expr(*value).ty;
                // let target_ty = *target;
                //
                // let value = self.codegen_expr(state, *value);
                //
                // match (source_ty.cty(self), target_ty.cty(self)) {
                //     (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(target)) => self
                //         .bx
                //         .build_int_cast_sign_flag(
                //             value.into_int_value(),
                //             target,
                //             target_ty.is_uint(),
                //             "cast_result",
                //         )
                //         .into(),
                //     (source, target) => panic!(
                //         "unexpected types in cast: {} : {} and {} : {}",
                //         source,
                //         source_ty.display(self.db),
                //         target,
                //         target_ty.display(self.db)
                //     ),
                // }
            }
            ExprKind::Index { value, index } => {
                todo!();
                // let gepped_ty = state.body.expr(*value).ty.llpointee(self);
                // let pointee_ty = expr.ty.cty(self);
                // let ptr = self.codegen_expr(state, *value).into_pointer_value();
                //
                // let gep =
                //     self.bx.build_struct_gep(gepped_ty, ptr, *index as u32, "index_gap").unwrap();
                //
                // self.bx.build_load(pointee_ty, gep, "load_index_gep")
            }
            ExprKind::Id(id) => match id {
                Id::Fn(fid) => {
                    todo!();
                    // self.function(*fid).as_global_value().as_pointer_value().as_basic_value_enum()
                }
                Id::Global(gid) => {
                    todo!();
                    // let ptr = self.global(*gid).as_pointer_value();
                    // let glob = &self.tir.globals[*gid];
                    // self.bx.build_load(glob.ty.cty(self), ptr, &format!("load_{}", glob.name))
                }
                Id::Local(lid) => {
                    todo!();
                    // match state.local(*lid) {
                    //     Local::Alloca(ptr, ty) => self.bx.build_load(
                    //         ty,
                    //         ptr,
                    //         &format!("load_{}", state.body.local(*lid).name),
                    //     ),
                    //     Local::Value(value) => value,
                    // }
                }
            },
            ExprKind::Const(value) => {
                todo!();
                // self.const_value(value, expr.ty)
            }
        }
    }

    fn codegen_br(&mut self, state: &mut FnState<'db>, expr: ExprId) {
        // let value = self.codegen_expr(state, expr);
        // self.current_block_is_terminating().not().then_some(value)
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
