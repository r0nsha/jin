use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::IntType,
    values::{AnyValue, BasicValue, BasicValueEnum, FunctionValue},
    AddressSpace, IntPredicate,
};

use crate::{
    ast::{BinOpKind, CmpOp},
    db::{Db, DefId},
    llvm::{inkwell_ext::ContextExt, ty::LlvmTy},
    mir::{
        BinOp, Block, BlockId, BoolLit, Br, BrIf, Call, Function, Inst, IntLit, Load, Mir, Phi,
        Return, UnitLit, Unreachable, ValueId,
    },
    ty::Ty,
};

pub struct Generator<'db, 'cx> {
    pub db: &'db mut Db,
    pub mir: &'db Mir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub bx: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,

    pub def_values: HashMap<DefId, DefValue<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum DefValue<'cx> {
    Function(FunctionValue<'cx>),
    Variable(BasicValueEnum<'cx>),
}

impl<'cx> DefValue<'cx> {
    pub fn as_function_value(self) -> FunctionValue<'cx> {
        match self {
            DefValue::Function(f) => f,
            DefValue::Variable(..) => panic!("expected Function, found {self:?}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionState<'cx> {
    pub id: DefId,
    pub function_value: FunctionValue<'cx>,
    pub prologue_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
    blocks: HashMap<BlockId, BasicBlock<'cx>>,
    values: HashMap<ValueId, BasicValueEnum<'cx>>,
}

impl<'cx> FunctionState<'cx> {
    pub fn new(
        id: DefId,
        function_value: FunctionValue<'cx>,
        prologue_block: BasicBlock<'cx>,
        blocks: HashMap<BlockId, BasicBlock<'cx>>,
    ) -> Self {
        let current_block = *blocks.get(&BlockId::first()).expect("to have a start block");

        Self {
            id,
            function_value,
            prologue_block,
            current_block,
            blocks,
            values: HashMap::default(),
        }
    }

    pub fn function<'db>(&self, cx: &Generator<'db, 'cx>) -> &'db Function {
        cx.mir.functions.get(&self.id).expect("FunctionState.id to be valid")
    }

    pub fn block(&self, id: BlockId) -> BasicBlock<'cx> {
        *self.blocks.get(&id).expect("to be a valid BlockId")
    }

    pub fn value(&self, id: ValueId) -> BasicValueEnum<'cx> {
        *self.values.get(&id).expect("to be a valid ValueId")
    }

    pub fn set_value(&mut self, id: ValueId, value: BasicValueEnum<'cx>) {
        self.values.insert(id, value);
    }

    pub fn value_ty<'db>(&self, cx: &Generator<'db, 'cx>, id: ValueId) -> Ty {
        self.function(cx).value(id).expect("value to exist").ty
    }
}

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn run(&mut self) {
        self.declare_all_functions();
        self.define_all_functions();
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

    pub fn declare_all_functions(&mut self) {
        for fun in self.mir.functions.values() {
            let id = fun.id();
            let fun_info = &self.db[id];
            let name = fun_info.qpath.standard_full_name();
            let llvm_ty = fun_info.ty.as_fn().expect("a function type").llvm_ty(self);

            let function = self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
            self.def_values.insert(id, DefValue::Function(function));
        }
    }

    pub fn define_all_functions(&mut self) {
        for fun in self.mir.functions.values() {
            self.codegen_function(fun);
        }
    }

    fn codegen_function(&mut self, fun: &Function) -> BasicValueEnum<'cx> {
        let id = fun.id();
        let fun_info = &self.db[id];

        let function_value = self.def_values.get(&fun.id()).map_or_else(
            || panic!("function {} to be declared", fun_info.qpath.standard_full_name()),
            |f| f.as_function_value(),
        );

        for (param, value) in fun.params().iter().zip(function_value.get_param_iter()) {
            self.def_values.insert(param.id(), DefValue::Variable(value));
        }

        let prologue_block = self.context.append_basic_block(function_value, "decls");
        self.bx.position_at_end(prologue_block);

        let mut blocks = HashMap::default();

        for blk in fun.blocks() {
            let bb = self.context.append_basic_block(function_value, &blk.name);
            blocks.insert(blk.id, bb);
        }

        self.bx.build_unconditional_branch(blocks[&BlockId::first()]);

        let mut state = FunctionState::new(id, function_value, prologue_block, blocks);

        for blk in fun.blocks() {
            blk.codegen(self, &mut state);
        }

        function_value.as_global_value().as_pointer_value().into()
    }

    #[track_caller]
    fn def_value(&self, id: DefId) -> DefValue<'cx> {
        *self.def_values.get(&id).expect("id in def_value to be defined")
    }
}

trait Codegen<'db, 'cx> {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>);
}

impl<'db, 'cx> Codegen<'db, 'cx> for Block {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let bb = state.block(self.id);
        cx.start_block(state, bb);

        for inst in &self.instructions {
            inst.codegen(cx, state);
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Inst {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if cx.current_block_is_terminating() {
            return;
        }

        match self {
            Self::Return(inner) => inner.codegen(cx, state),
            Self::Br(inner) => inner.codegen(cx, state),
            Self::BrIf(inner) => inner.codegen(cx, state),
            Self::Phi(inner) => inner.codegen(cx, state),
            Self::Call(inner) => inner.codegen(cx, state),
            Self::Load(inner) => inner.codegen(cx, state),
            Self::BinOp(inner) => inner.codegen(cx, state),
            Self::IntLit(inner) => inner.codegen(cx, state),
            Self::BoolLit(inner) => inner.codegen(cx, state),
            Self::UnitLit(inner) => inner.codegen(cx, state),
            Self::Unreachable(inner) => inner.codegen(cx, state),
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Return {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        cx.bx.build_return(Some(&state.value(self.value)));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Br {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        cx.bx.build_unconditional_branch(state.block(self.target));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BrIf {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        cx.bx.build_conditional_branch(
            state.value(self.cond).into_int_value(),
            state.block(self.b1),
            state.block(self.b2),
        );
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Phi {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
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

impl<'db, 'cx> Codegen<'db, 'cx> for Call {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        // TODO: this doesn't take indirect calls (function pointers) into account
        let callee = state.value(self.callee).as_any_value_enum().into_function_value();

        let args: Vec<_> = self.args.iter().map(|v| state.value(*v).into()).collect();

        // Don't call actually call the function if it's diverging
        if state.value_ty(cx, self.callee).is_diverging() {
            cx.build_unreachable();
            return;
        }

        let result = cx.bx.build_direct_call(callee, &args, "call");

        let result_value = result.try_as_basic_value().expect_left("expected a return value");

        // TODO: remove this debug printf call
        {
            let printf = cx.module.get_function("printf").unwrap_or_else(|| {
                cx.module.add_function(
                    "printf",
                    cx.isize_ty
                        .fn_type(&[cx.context.ptr_type(AddressSpace::default()).into()], true),
                    Some(Linkage::External),
                )
            });

            cx.bx.build_direct_call(
                printf,
                &[
                    cx.bx
                        .build_global_string_ptr("result = %d\n\0", "fmt")
                        .as_pointer_value()
                        .into(),
                    result_value.into(),
                ],
                "printf_call",
            );
        }

        state.set_value(self.value, result_value);
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Load {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let value = match cx.def_value(self.id) {
            DefValue::Function(f) => f.as_global_value().as_pointer_value().as_basic_value_enum(),
            DefValue::Variable(v) => v,
        };

        state.set_value(self.value, value);
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BinOp {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        const NAME: &str = "result";

        let lhs = state.value(self.lhs).into_int_value();
        let rhs = state.value(self.rhs).into_int_value();

        let result = match self.op {
            BinOpKind::Add => cx.bx.build_int_add(lhs, rhs, NAME),
            BinOpKind::Sub => cx.bx.build_int_sub(lhs, rhs, NAME),
            BinOpKind::Mul => cx.bx.build_int_mul(lhs, rhs, NAME),
            // TODO: unsigned
            BinOpKind::Div => cx.bx.build_int_signed_div(lhs, rhs, NAME),
            // TODO: unsigned
            BinOpKind::Mod => cx.bx.build_int_signed_rem(lhs, rhs, NAME),
            BinOpKind::Shl => cx.bx.build_left_shift(lhs, rhs, NAME),
            // TODO: unsigned
            BinOpKind::Shr => cx.bx.build_right_shift(lhs, rhs, true, NAME),
            BinOpKind::BitAnd => cx.bx.build_and(lhs, rhs, NAME),
            BinOpKind::BitOr => cx.bx.build_or(lhs, rhs, NAME),
            BinOpKind::BitXor => cx.bx.build_xor(lhs, rhs, NAME),
            BinOpKind::And => todo!(),
            BinOpKind::Or => todo!(),
            BinOpKind::Cmp(op) => {
                let pred = get_int_predicate(op);
                cx.bx.build_int_compare(pred, lhs, rhs, NAME)
            }
        };

        state.set_value(self.value, result.into());
    }
}

fn get_int_predicate(op: CmpOp) -> IntPredicate {
    // TODO: unsigned
    match op {
        CmpOp::Eq => IntPredicate::EQ,
        CmpOp::Ne => IntPredicate::NE,
        CmpOp::Lt => IntPredicate::SLT,
        CmpOp::Le => IntPredicate::SLE,
        CmpOp::Gt => IntPredicate::SGT,
        CmpOp::Ge => IntPredicate::SGE,
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for IntLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let ty = state.value_ty(cx, self.value).llvm_ty(cx).into_int_type();

        // TODO: unsigned integers
        let value = ty.const_int(self.lit as u64, true);
        state.set_value(self.value, value.into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BoolLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let value = cx.context.bool_type().const_int(u64::from(self.lit), false);
        state.set_value(self.value, value.into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for UnitLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        state.set_value(self.value, cx.unit_value().into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Unreachable {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        cx.build_unreachable();
        let ty = state.value_ty(cx, self.value);
        state.set_value(self.value, Generator::undef_value(ty.llvm_ty(cx)));
    }
}
