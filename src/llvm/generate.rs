use std::{collections::HashMap, ops::Not};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicTypeEnum, IntType, StructType},
    values::{AnyValue, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, IntPredicate,
};

use crate::{
    ast::{BinOp, CmpOp, UnOp},
    db::{Db, DefId},
    llvm::{inkwell_ext::ContextExt, ty::LlvmTy},
    tir::{ExprId, ExprKind, Fn, FnSigId, Id, Tir},
};

pub struct Generator<'db, 'cx> {
    pub db: &'db mut Db,
    pub tir: &'db Tir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub bx: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,
    pub unit_ty: StructType<'cx>,

    pub functions: HashMap<FnSigId, FunctionValue<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum Local<'cx> {
    Alloca(PointerValue<'cx>, BasicTypeEnum<'cx>),
    Value(BasicValueEnum<'cx>),
}

#[derive(Debug, Clone)]
pub struct FnState<'db, 'cx> {
    pub f: &'db Fn,
    pub function_value: FunctionValue<'cx>,
    pub prologue_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
    // TODO: LocalId
    pub locals: HashMap<DefId, Local<'cx>>,
}

impl<'db, 'cx> FnState<'db, 'cx> {
    pub fn new(
        f: &'db Fn,
        function_value: FunctionValue<'cx>,
        prologue_block: BasicBlock<'cx>,
        first_block: BasicBlock<'cx>,
    ) -> Self {
        Self {
            f,
            function_value,
            prologue_block,
            current_block: first_block,
            locals: HashMap::new(),
        }
    }

    #[track_caller]
    fn local(&self, id: DefId) -> Local<'cx> {
        self.locals.get(&id).copied().unwrap_or_else(|| panic!("local {} to be declared", id))
    }
}

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn run(&mut self) {
        self.predefine_all();
        self.define_all();
        self.codegen_start_function();
    }

    pub fn codegen_start_function(&mut self) {
        let function_value = self.module.add_function(
            "main",
            self.context.i32_type().fn_type(&[], false),
            Some(Linkage::External),
        );

        let entry_block = self.context.append_basic_block(function_value, "entry");
        self.bx.position_at_end(entry_block);

        let main_function_value = self.function(self.tir.main_fn.expect("to have a main function"));

        self.bx.build_direct_call(main_function_value, &[], "call_main");

        if !self.current_block_is_terminating() {
            self.bx.build_return(Some(&self.context.i32_type().const_zero()));
        }
    }

    pub fn predefine_all(&mut self) {
        for fun in self.tir.fns.iter() {
            let sig = &self.tir.sigs[fun.sig];
            let llvm_ty = sig.ty.as_fn().expect("a function type").llty(self);

            let function = self.module.add_function(&sig.name, llvm_ty, Some(Linkage::Private));
            self.functions.insert(fun.sig, function);
        }
    }

    pub fn define_all(&mut self) {
        for fun in self.tir.fns.iter() {
            self.codegen_fn(fun);
        }
    }

    fn codegen_fn(&mut self, fun: &'db Fn) -> BasicValueEnum<'cx> {
        let sig = &self.tir.sigs[fun.sig];
        let fty = sig.ty;

        let function_value = self.function(fun.sig);

        let sig = &self.tir.sigs[fun.sig];

        let prologue_block = self.context.append_basic_block(function_value, "prologue");
        let start_block = self.context.append_basic_block(function_value, "start");

        let mut state = FnState::new(fun, function_value, prologue_block, start_block);

        for (param, value) in sig.params.iter().zip(function_value.get_param_iter()) {
            // TODO: LocalId
            state.locals.insert(param.def_id, Local::Value(value));
        }

        self.start_block(&mut state, start_block);
        let body = self.codegen_expr(&mut state, fun.body);

        if !self.current_block_is_terminating() {
            let ret_value =
                if fty.as_fn().unwrap().ret.is_unit() && !state.f.expr(fun.body).ty.is_unit() {
                    self.unit_value().as_basic_value_enum()
                } else {
                    body
                };

            self.bx.build_return(Some(&ret_value));
        }

        self.bx.position_at_end(prologue_block);
        self.bx.build_unconditional_branch(start_block);

        function_value.as_global_value().as_pointer_value().into()
    }

    fn codegen_expr(&mut self, state: &mut FnState<'db, 'cx>, expr: ExprId) -> BasicValueEnum<'cx> {
        let expr = &state.f.expr(expr);

        if self.current_block_is_terminating() {
            return Self::undef_value(expr.ty.llty(self));
        }

        match &expr.kind {
            ExprKind::Let { def_id, value } => {
                // TODO: LocalId
                let def = &self.db[*def_id];
                let ty = def.ty.llty(self);
                let ptr = self.build_stack_alloc(state, ty, &def.qpath.standard_full_name());

                let value = self.codegen_expr(state, *value);
                self.bx.build_store(ptr, value);

                // TODO: LocalId
                state.locals.insert(*def_id, Local::Alloca(ptr, ty));
                self.unit_value().into()
            }
            ExprKind::If { cond, then, otherwise } => {
                let cond = self.codegen_expr(state, *cond).into_int_value();

                let then_block = self.append_block(state, "if_then");
                let else_block = self.append_block(state, "if_else");
                let merge_block = self.append_block(state, "if_merge");

                self.bx.build_conditional_branch(cond, then_block, else_block);

                self.start_block(state, then_block);
                let then_value = self.codegen_br(state, *then);
                self.bx.build_unconditional_branch(merge_block);

                self.start_block(state, else_block);
                let else_value = if let Some(otherwise) = otherwise {
                    self.codegen_br(state, *otherwise)
                } else {
                    Some(self.unit_value().into())
                };

                self.bx.build_unconditional_branch(merge_block);
                self.start_block(state, merge_block);

                let ty = expr.ty.llty(self);
                match (then_value, else_value) {
                    (Some(then_value), Some(else_value)) => {
                        let phi = self.bx.build_phi(ty, "phi");
                        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                        phi.as_basic_value()
                    }
                    (Some(then_value), None) => then_value,
                    (None, Some(else_value)) => else_value,
                    (None, None) => {
                        self.build_unreachable();
                        Self::undef_value(ty)
                    }
                }
            }
            ExprKind::Block { exprs } => {
                let mut result = self.unit_value().as_basic_value_enum();

                for expr in exprs {
                    result = self.codegen_expr(state, *expr);
                }

                result
            }
            ExprKind::Return { value } => {
                let value = self.codegen_expr(state, *value);
                self.bx.build_return(Some(&value));
                Self::undef_value(expr.ty.llty(self))
            }
            ExprKind::Call { callee, args } => {
                let callee_ty = state.f.expr(*callee).ty;

                let args: Vec<_> =
                    args.iter().map(|arg| self.codegen_expr(state, *arg).into()).collect();

                // TODO: this doesn't take indirect calls (function pointers) into account
                let callee =
                    self.codegen_expr(state, *callee).as_any_value_enum().into_function_value();

                // Don't call actually call the function if it's diverging
                if callee_ty.is_diverging() {
                    self.build_unreachable();
                    return Self::undef_value(expr.ty.llty(self));
                }

                let result = self.bx.build_direct_call(callee, &args, "call");

                let result_value =
                    result.try_as_basic_value().expect_left("expected a return value");

                // TODO: remove this debug printf call
                {
                    let printf = self.module.get_function("printf").unwrap_or_else(|| {
                        self.module.add_function(
                            "printf",
                            self.isize_ty.fn_type(
                                &[self.context.ptr_type(AddressSpace::default()).into()],
                                true,
                            ),
                            Some(Linkage::External),
                        )
                    });

                    self.bx.build_direct_call(
                        printf,
                        &[
                            self.bx
                                .build_global_string_ptr("result = %d\n\0", "fmt")
                                .as_pointer_value()
                                .into(),
                            if let BasicValueEnum::IntValue(v) = result_value {
                                self.bx
                                    .build_int_cast_sign_flag(
                                        v,
                                        self.isize_ty,
                                        false,
                                        "cast_to_i64",
                                    )
                                    .into()
                            } else {
                                result_value.into()
                            },
                        ],
                        "printf_call",
                    );
                }

                result_value
            }
            ExprKind::Binary { lhs, rhs, op } => {
                let ty = expr.ty;
                let lhs = self.codegen_expr(state, *lhs).into_int_value();

                match op {
                    BinOp::And => {
                        // if lhs { rhs } else { false }
                        let then_block = self.append_block(state, "and_then");
                        let else_block = self.append_block(state, "and_else");
                        let merge_block = self.append_block(state, "and_merge");

                        self.bx.build_conditional_branch(lhs, then_block, else_block);

                        self.start_block(state, then_block);
                        let then_value = self.codegen_expr(state, *rhs);
                        self.bx.build_unconditional_branch(merge_block);

                        self.start_block(state, else_block);
                        let else_value = self.bool_value(false);

                        self.bx.build_unconditional_branch(merge_block);
                        self.start_block(state, merge_block);

                        let ty = expr.ty.llty(self);
                        let phi = self.bx.build_phi(ty, "phi");
                        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                        phi.as_basic_value()
                    }
                    BinOp::Or => {
                        // if lhs { true } else { rhs }
                        let then_block = self.append_block(state, "or_then");
                        let else_block = self.append_block(state, "or_else");
                        let merge_block = self.append_block(state, "or_merge");

                        self.bx.build_conditional_branch(lhs, then_block, else_block);

                        self.start_block(state, then_block);
                        let then_value = self.bool_value(true);
                        self.bx.build_unconditional_branch(merge_block);

                        self.start_block(state, else_block);
                        let else_value = self.codegen_expr(state, *rhs);

                        self.bx.build_unconditional_branch(merge_block);
                        self.start_block(state, merge_block);

                        let ty = expr.ty.llty(self);
                        let phi = self.bx.build_phi(ty, "phi");
                        phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                        phi.as_basic_value()
                    }
                    _ => {
                        let rhs = self.codegen_expr(state, *rhs).into_int_value();

                        match op {
                            BinOp::Add => self.bx.build_int_add(lhs, rhs, "result").into(),
                            BinOp::Sub => self.bx.build_int_sub(lhs, rhs, "result").into(),
                            BinOp::Mul => self.bx.build_int_mul(lhs, rhs, "result").into(),
                            BinOp::Div => {
                                if ty.is_uint() {
                                    self.bx.build_int_unsigned_div(lhs, rhs, "result").into()
                                } else {
                                    self.bx.build_int_signed_div(lhs, rhs, "result").into()
                                }
                            }
                            BinOp::Mod => {
                                if ty.is_uint() {
                                    self.bx.build_int_unsigned_rem(lhs, rhs, "result").into()
                                } else {
                                    self.bx.build_int_signed_rem(lhs, rhs, "result").into()
                                }
                            }
                            BinOp::Shl => self.bx.build_left_shift(lhs, rhs, "result").into(),
                            BinOp::Shr => {
                                self.bx.build_right_shift(lhs, rhs, ty.is_int(), "result").into()
                            }
                            BinOp::BitAnd => self.bx.build_and(lhs, rhs, "result").into(),
                            BinOp::BitOr => self.bx.build_or(lhs, rhs, "result").into(),
                            BinOp::BitXor => self.bx.build_xor(lhs, rhs, "result").into(),
                            BinOp::Cmp(op) => {
                                let pred = get_int_predicate(*op, ty.is_int());
                                self.bx.build_int_compare(pred, lhs, rhs, "result").into()
                            }
                            BinOp::And | BinOp::Or => unreachable!(),
                        }
                    }
                }
            }
            ExprKind::Unary { value, op } => {
                let value = self.codegen_expr(state, *value).into_int_value();
                match op {
                    UnOp::Neg => self.bx.build_int_neg(value, "result").into(),
                    UnOp::Not => self.bx.build_not(value, "result").into(),
                }
            }
            ExprKind::Cast { value, target } => {
                let source_ty = state.f.expr(*value).ty;
                let target_ty = *target;

                let value = self.codegen_expr(state, *value);

                match (source_ty.llty(self), target_ty.llty(self)) {
                    (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(target)) => self
                        .bx
                        .build_int_cast_sign_flag(
                            value.into_int_value(),
                            target,
                            target_ty.is_uint(),
                            "cast_result",
                        )
                        .into(),
                    (source, target) => panic!(
                        "unexpected types in cast: {} : {} and {} : {}",
                        source,
                        source_ty.display(self.db),
                        target,
                        target_ty.display(self.db)
                    ),
                }
            }
            ExprKind::Id { id } => match id {
                Id::Fn(fid) => {
                    self.function(*fid).as_global_value().as_pointer_value().as_basic_value_enum()
                }
                Id::Local(lid) => match state.local(*lid) {
                    Local::Alloca(p, ty) => {
                        self.bx.build_load(ty, p, &format!("load_{}", self.db[*lid].name))
                    }
                    Local::Value(v) => v,
                },
            },
            ExprKind::IntLit { value } => {
                expr.ty.llty(self).into_int_type().const_int(*value as u64, expr.ty.is_int()).into()
            }
            ExprKind::BoolLit { value } => self.bool_value(*value).into(),
            ExprKind::UnitLit => self.unit_value().into(),
        }
    }

    fn codegen_br(
        &mut self,
        state: &mut FnState<'db, 'cx>,
        expr: ExprId,
    ) -> Option<BasicValueEnum<'cx>> {
        let value = self.codegen_expr(state, expr);
        self.current_block_is_terminating().not().then_some(value)
    }

    #[track_caller]
    fn function(&self, id: FnSigId) -> FunctionValue<'cx> {
        self.functions
            .get(&id)
            .copied()
            .unwrap_or_else(|| panic!("function {} to be declared", self.tir.sigs[id].name))
    }
}

trait Codegen<'db, 'cx> {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FnState<'db, 'cx>);
}

fn get_int_predicate(op: CmpOp, is_signed: bool) -> IntPredicate {
    match op {
        CmpOp::Eq => IntPredicate::EQ,
        CmpOp::Ne => IntPredicate::NE,
        CmpOp::Lt => {
            if is_signed {
                IntPredicate::SLT
            } else {
                IntPredicate::ULT
            }
        }
        CmpOp::Le => {
            if is_signed {
                IntPredicate::SLE
            } else {
                IntPredicate::ULE
            }
        }
        CmpOp::Gt => {
            if is_signed {
                IntPredicate::SGT
            } else {
                IntPredicate::UGT
            }
        }
        CmpOp::Ge => {
            if is_signed {
                IntPredicate::SGE
            } else {
                IntPredicate::UGE
            }
        }
    }
}
