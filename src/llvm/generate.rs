use std::collections::HashMap;

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
    tir::{Expr, ExprId, ExprKind, Fn, Tir},
    ty::Ty,
};

pub struct Generator<'db, 'cx> {
    pub db: &'db mut Db,
    pub tir: &'db Tir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub bx: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,
    pub unit_ty: StructType<'cx>,

    pub def_values: HashMap<DefId, DefValue<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum DefValue<'cx> {
    Function(FunctionValue<'cx>),
    Alloca(PointerValue<'cx>, BasicTypeEnum<'cx>),
    Value(BasicValueEnum<'cx>),
}

impl<'cx> DefValue<'cx> {
    pub fn as_function_value(self) -> FunctionValue<'cx> {
        match self {
            DefValue::Function(f) => f,
            DefValue::Alloca(..) | DefValue::Value(..) => {
                panic!("expected Function, found {self:?}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnState<'cx> {
    pub f: &'cx Fn,
    pub function_value: FunctionValue<'cx>,
    pub prologue_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
}

impl<'cx> FnState<'cx> {
    pub fn new(
        f: &'cx Fn,
        function_value: FunctionValue<'cx>,
        prologue_block: BasicBlock<'cx>,
        first_block: BasicBlock<'cx>,
    ) -> Self {
        Self { f, function_value, prologue_block, current_block: first_block }
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

        let main_function = self.db.main_function().expect("to have a main function");
        let main_function_value = self.def_value(main_function.id).as_function_value();

        self.bx.build_direct_call(main_function_value, &[], "call_main");

        if !self.current_block_is_terminating() {
            self.bx.build_return(Some(&self.context.i32_type().const_zero()));
        }
    }

    pub fn predefine_all(&mut self) {
        for fun in &self.tir.functions {
            let fun_info = &self.db[fun.id];
            let name = fun_info.qpath.standard_full_name();
            let llvm_ty = fun_info.ty.as_fn().expect("a function type").llvm_ty(self);

            let function = self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
            self.def_values.insert(fun.id, DefValue::Function(function));
        }
    }

    pub fn define_all(&mut self) {
        for fun in &self.tir.functions {
            self.codegen_fn(fun);
        }
    }

    fn codegen_fn(&mut self, fun: &Fn) -> BasicValueEnum<'cx> {
        let fun_info = &self.db[fun.id];

        let function_value = self.def_values.get(&fun.id).map_or_else(
            || panic!("function {} to be declared", fun_info.qpath.standard_full_name()),
            |f| f.as_function_value(),
        );

        for (param, value) in fun.sig.params.iter().zip(function_value.get_param_iter()) {
            self.def_values.insert(param.id, DefValue::Value(value));
        }

        let prologue_block = self.context.append_basic_block(function_value, "prologue");
        let start_block = self.context.append_basic_block(function_value, "start");

        let mut state = FnState::new(fun, function_value, prologue_block, start_block);

        self.codegen_expr(&mut state, fun.body);

        self.bx.position_at_end(prologue_block);
        self.bx.build_unconditional_branch(start_block);

        function_value.as_global_value().as_pointer_value().into()
    }

    #[track_caller]
    fn def_value(&self, id: DefId) -> DefValue<'cx> {
        *self.def_values.get(&id).expect("id in def_value to be defined")
    }

    fn codegen_expr(&mut self, state: &mut FnState, expr: ExprId) -> BasicValueEnum<'cx> {
        let expr = &state.f.expr(expr);

        if self.current_block_is_terminating() {
            return Self::undef_value(expr.ty.llvm_ty(self));
        }

        match &expr.kind {
            ExprKind::Let { id, value } => {
                let def = &self.db[*id];
                let ty = def.ty.llvm_ty(self);
                let ptr = self.build_stack_alloc(state, ty, &def.qpath.full_c_name());

                let value = self.codegen_expr(state, *value);
                self.bx.build_store(ptr, value);

                self.def_values.insert(*id, DefValue::Alloca(ptr, ty));
                self.unit_value().into()
            }
            ExprKind::If { cond, then, otherwise } => {}
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
                Self::undef_value(expr.ty.llvm_ty(self))
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
                    return Self::undef_value(expr.ty.llvm_ty(self));
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
                let lhs = self.codegen_expr(state, *lhs).into_int_value();
                let rhs = self.codegen_expr(state, *rhs).into_int_value();
                let ty = expr.ty;

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
                    BinOp::Shr => self.bx.build_right_shift(lhs, rhs, ty.is_int(), "result").into(),
                    BinOp::BitAnd => self.bx.build_and(lhs, rhs, "result").into(),
                    BinOp::BitOr => self.bx.build_or(lhs, rhs, "result").into(),
                    BinOp::BitXor => self.bx.build_xor(lhs, rhs, "result").into(),
                    BinOp::And => todo!(),
                    BinOp::Or => todo!(),
                    BinOp::Cmp(op) => {
                        let pred = get_int_predicate(*op, ty.is_int());
                        self.bx.build_int_compare(pred, lhs, rhs, "result").into()
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

                match (source_ty.llvm_ty(self), target_ty.llvm_ty(self)) {
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
            ExprKind::Name { id } => match self.def_value(*id) {
                DefValue::Function(f) => {
                    f.as_global_value().as_pointer_value().as_basic_value_enum()
                }
                DefValue::Alloca(p, ty) => {
                    self.bx.build_load(ty, p, &format!("load_{}", self.db[*id].name))
                }
                DefValue::Value(v) => v,
            },
            ExprKind::IntLit { value } => expr
                .ty
                .llvm_ty(self)
                .into_int_type()
                .const_int(*value as u64, expr.ty.is_int())
                .into(),
            ExprKind::BoolLit { value } => {
                self.context.bool_type().const_int(u64::from(*value), false).into()
            }
            ExprKind::UnitLit => self.unit_value().into(),
        }
    }
}

trait Codegen<'db, 'cx> {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FnState<'cx>);
}

impl<'db, 'cx> Codegen<'db, 'cx> for Br {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FnState<'cx>) {
        cx.bx.build_unconditional_branch(state.block(self.target));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BrIf {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FnState<'cx>) {
        cx.bx.build_conditional_branch(
            state.value(self.cond).into_int_value(),
            state.block(self.b1),
            state.block(self.b2),
        );
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Phi {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FnState<'cx>) {
        let ty = state.value_ty(cx, self.value).llvm_ty(cx);
        let phi = cx.bx.build_phi(ty, "phi");

        for (blk, value) in &*self.phi_values {
            let value = state.value(*value);
            let bb = state.block(*blk);
            phi.add_incoming(&[(&value, bb)]);
        }

        state.set_value(self.value, phi.as_basic_value());
    }
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
