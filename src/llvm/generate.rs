use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::IntType,
    values::{BasicValue, BasicValueEnum, CallableValue, FunctionValue},
    AddressSpace, IntPredicate,
};

use crate::{
    ast::{BinaryOp, CmpOp},
    db::{Database, SymbolId, TypeId},
    llvm::ty::LlvmType,
    mir::{
        Binary, Block, BlockId, BoolLit, Br, BrIf, Call, Function, Inst, IntLit, Load, Mir, Phi,
        Return, UnitLit, Unreachable, ValueId,
    },
};

pub struct Generator<'db, 'cx> {
    pub db: &'db mut Database,
    pub mir: &'db Mir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub builder: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,

    pub symbol_values: HashMap<SymbolId, SymbolValue<'cx>>,
}

#[derive(Debug, Clone, Copy)]
pub enum SymbolValue<'cx> {
    Function(FunctionValue<'cx>),
    Variable(BasicValueEnum<'cx>),
}

impl<'cx> SymbolValue<'cx> {
    pub fn as_function_value(self) -> FunctionValue<'cx> {
        match self {
            SymbolValue::Function(f) => f,
            SymbolValue::Variable(_) => panic!("expected Function, got {self:?} instead"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionState<'cx> {
    pub id: SymbolId,
    pub function_value: FunctionValue<'cx>,
    pub prologue_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
    blocks: HashMap<BlockId, BasicBlock<'cx>>,
    values: HashMap<ValueId, BasicValueEnum<'cx>>,
}

impl<'cx> FunctionState<'cx> {
    pub fn new(
        id: SymbolId,
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

    pub fn value_ty<'db>(&self, cx: &Generator<'db, 'cx>, id: ValueId) -> TypeId {
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
        self.builder.position_at_end(entry_block);

        let main_function = self.db.main_function().expect("to have a main function");
        let main_function_value = self.symbol_value(main_function.id).as_function_value();

        self.builder.build_call(main_function_value, &[], "call_main");

        if !self.current_block_is_terminating() {
            self.builder.build_return(Some(&self.context.i32_type().const_zero()));
        }
    }

    pub fn declare_all_functions(&mut self) {
        for fun in self.mir.functions.values() {
            let id = fun.id();
            let fun_info = &self.db[id];
            let name = fun_info.qname.standard_full_name();
            let llvm_ty = fun_info
                .ty
                .llvm_type(self)
                .into_pointer_type()
                .get_element_type()
                .into_function_type();

            let function = self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
            self.symbol_values.insert(id, SymbolValue::Function(function));
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

        let function_value = self.symbol_values.get(&fun.id()).map_or_else(
            || panic!("function {} to be declared", fun_info.qname.standard_full_name()),
            |f| f.as_function_value(),
        );

        for (param, value) in fun.params().iter().zip(function_value.get_param_iter()) {
            self.symbol_values.insert(param.id(), SymbolValue::Variable(value));
        }

        let prologue_block = self.context.append_basic_block(function_value, "decls");
        self.builder.position_at_end(prologue_block);

        let mut blocks = HashMap::default();

        for blk in fun.blocks() {
            let bb = self.context.append_basic_block(function_value, &blk.name);
            blocks.insert(blk.id, bb);
        }

        self.builder.build_unconditional_branch(blocks[&BlockId::first()]);

        let mut state = FunctionState::new(id, function_value, prologue_block, blocks);

        for blk in fun.blocks() {
            blk.codegen(self, &mut state);
        }

        function_value.as_global_value().as_pointer_value().into()
    }

    #[track_caller]
    fn symbol_value(&self, id: SymbolId) -> SymbolValue<'cx> {
        *self.symbol_values.get(&id).expect("id in def_value to be defined")
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
        match self {
            Self::Return(inner) => inner.codegen(cx, state),
            Self::Br(inner) => inner.codegen(cx, state),
            Self::BrIf(inner) => inner.codegen(cx, state),
            Self::Phi(inner) => inner.codegen(cx, state),
            Self::Call(inner) => inner.codegen(cx, state),
            Self::Load(inner) => inner.codegen(cx, state),
            Self::Binary(inner) => inner.codegen(cx, state),
            Self::IntLit(inner) => inner.codegen(cx, state),
            Self::BoolLit(inner) => inner.codegen(cx, state),
            Self::UnitLit(inner) => inner.codegen(cx, state),
            Self::Unreachable(inner) => inner.codegen(cx, state),
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Return {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if cx.current_block_is_terminating() {
            return;
        }

        cx.builder.build_return(Some(&state.value(self.value)));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Br {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if cx.current_block_is_terminating() {
            return;
        }

        cx.builder.build_unconditional_branch(state.block(self.target));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BrIf {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if cx.current_block_is_terminating() {
            return;
        }

        cx.builder.build_conditional_branch(
            state.value(self.cond).into_int_value(),
            state.block(self.b1),
            state.block(self.b2),
        );
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Phi {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let ty = state.value_ty(cx, self.value).llvm_type(cx);
        let phi = cx.builder.build_phi(ty, "phi");

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
        let callee = state.value(self.callee).into_pointer_value();

        let args: Vec<_> = self.args.iter().map(|v| state.value(*v).into()).collect();

        let result = cx.builder.build_call(
            CallableValue::try_from(callee).expect("a callable pointer value"),
            &args,
            "call",
        );

        let result_value = result.try_as_basic_value().expect_left("expected a return value");

        // TODO: remove this debug printf call
        {
            let printf = cx.module.get_function("printf").unwrap_or_else(|| {
                cx.module.add_function(
                    "printf",
                    cx.isize_ty.fn_type(
                        &[cx.context.i8_type().ptr_type(AddressSpace::default()).into()],
                        true,
                    ),
                    Some(Linkage::External),
                )
            });

            cx.builder.build_call(
                printf,
                &[
                    cx.builder
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
        let value = match cx.symbol_value(self.id) {
            SymbolValue::Function(f) => {
                f.as_global_value().as_pointer_value().as_basic_value_enum()
            }
            SymbolValue::Variable(v) => v,
        };

        state.set_value(self.value, value);
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Binary {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        const NAME: &str = "result";

        let lhs = state.value(self.lhs).into_int_value();
        let rhs = state.value(self.rhs).into_int_value();

        let result = match self.op {
            BinaryOp::Add => cx.builder.build_int_add(lhs, rhs, NAME),
            BinaryOp::Sub => cx.builder.build_int_sub(lhs, rhs, NAME),
            BinaryOp::Mul => cx.builder.build_int_mul(lhs, rhs, NAME),
            // TODO: unsigned
            BinaryOp::Div => cx.builder.build_int_signed_div(lhs, rhs, NAME),
            // TODO: unsigned
            BinaryOp::Mod => cx.builder.build_int_signed_rem(lhs, rhs, NAME),
            BinaryOp::Shl => cx.builder.build_left_shift(lhs, rhs, NAME),
            // TODO: unsigned
            BinaryOp::Shr => cx.builder.build_right_shift(lhs, rhs, true, NAME),
            BinaryOp::BitAnd => cx.builder.build_and(lhs, rhs, NAME),
            BinaryOp::BitOr => cx.builder.build_or(lhs, rhs, NAME),
            BinaryOp::BitXor => cx.builder.build_xor(lhs, rhs, NAME),
            BinaryOp::And => todo!(),
            BinaryOp::Or => todo!(),
            BinaryOp::Cmp(op) => {
                let pred = get_int_predicate(op);
                cx.builder.build_int_compare(pred, lhs, rhs, NAME)
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
        let ty = state.value_ty(cx, self.value).llvm_type(cx).into_int_type();

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
        if !cx.current_block_is_terminating() {
            cx.builder.build_unreachable();
        }

        let ty = state.value_ty(cx, self.value);
        state.set_value(self.value, Generator::undef_value(ty.llvm_type(cx)));
    }
}
