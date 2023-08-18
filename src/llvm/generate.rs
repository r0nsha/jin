use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::IntType,
    values::{BasicValueEnum, CallableValue, FunctionValue},
    AddressSpace, IntPredicate,
};

use crate::{
    ast::{BinaryOp, CmpOp},
    db::{Database, DefId},
    llvm::ty::LlvmType,
    mir::{
        Binary, Block, BlockId, BoolLit, Br, BrIf, Call, Function, Inst, IntLit, LoadGlobal, Mir,
        Phi, RegisterId, Return, UnitLit, Value,
    },
};

pub struct Generator<'db, 'cx> {
    pub db: &'db Database,
    pub mir: &'db Mir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub builder: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,

    pub functions: HashMap<DefId, FunctionValue<'cx>>,
}

#[derive(Debug, Clone)]
pub struct FunctionState<'cx> {
    pub id: DefId,
    pub function_value: FunctionValue<'cx>,
    pub prologue_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
    blocks: HashMap<BlockId, BasicBlock<'cx>>,
    registers: HashMap<RegisterId, BasicValueEnum<'cx>>,
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
            registers: HashMap::default(),
        }
    }

    pub fn function<'db>(&self, cx: &Generator<'db, 'cx>) -> &'db Function {
        cx.mir.functions.iter().find(|f| f.id() == self.id).expect("FunctionState.id to be valid")
    }

    pub fn block(&self, id: BlockId) -> BasicBlock<'cx> {
        *self.blocks.get(&id).expect("to be a valid BlockId")
    }

    pub fn register(&self, id: RegisterId) -> BasicValueEnum<'cx> {
        *self.registers.get(&id).expect("to be a valid RegisterId")
    }

    pub fn set_register(&mut self, id: RegisterId, value: BasicValueEnum<'cx>) {
        self.registers.insert(id, value);
    }
}

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn run(&mut self) {
        self.declare_all_functions();
        self.define_all_functions();
        self.codegen_start_function();
    }

    pub fn codegen_start_function(&mut self) {
        // let startup_fn_type = FunctionType {
        //     params: vec![
        //         FunctionTypeParam { name: ustr("argc"), ty: Type::u32(), default_value: None },
        //         FunctionTypeParam {
        //             name: ustr("argv"),
        //             ty: Type::u8().pointer_type(false).pointer_type(false),
        //             default_value: None,
        //         },
        //     ],
        //     return_type: Box::new(Type::u32()),
        //     varargs: None,
        //     kind: FunctionTypeKind::Orphan,
        // };

        let function_value = self.module.add_function(
            "main",
            self.context.i32_type().fn_type(
                &[
                    self.context.i32_type().into(),
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .ptr_type(AddressSpace::default())
                        .into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        function_value.get_nth_param(0).unwrap().set_name("argc");
        function_value.get_nth_param(1).unwrap().set_name("argv");

        let entry_block = self.context.append_basic_block(function_value, "entry");
        self.builder.position_at_end(entry_block);

        let main_function = self.db.main_function().expect("to have a main function");
        let main_function_value = self.function(main_function.id);

        self.builder.build_call(main_function_value, &[], "call_main");

        if !self.current_block_is_terminating() {
            self.builder.build_return(Some(&self.context.i32_type().const_zero()));
        }
    }

    pub fn declare_all_functions(&mut self) {
        for fun in &self.mir.functions {
            let id = fun.id();
            let fun_info = &self.db[id];
            let name = fun_info.qualified_name.standard_full_name();
            let llvm_ty = fun_info
                .ty
                .llvm_type(self)
                .into_pointer_type()
                .get_element_type()
                .into_function_type();

            let function = self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
            self.functions.insert(id, function);
        }
    }

    pub fn define_all_functions(&mut self) {
        for fun in &self.mir.functions {
            self.codegen_function(fun);
        }
    }

    fn codegen_function(&mut self, fun: &Function) -> BasicValueEnum<'cx> {
        let id = fun.id();
        let fun_info = &self.db[id];

        let function_value = *self.functions.get(&fun.id()).unwrap_or_else(|| {
            panic!("function {} to be declared", fun_info.qualified_name.standard_full_name())
        });

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

    fn function(&self, id: DefId) -> FunctionValue<'cx> {
        *self.functions.get(&id).expect("function to be declared")
    }

    fn value(&self, state: &FunctionState<'cx>, value: &Value) -> BasicValueEnum<'cx> {
        match value {
            Value::Def(id) => self.function(*id).as_global_value().as_pointer_value().into(),
            Value::Register(id) => state.register(*id),
        }
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
            Self::LoadGlobal(inner) => inner.codegen(cx, state),
            Self::Binary(inner) => inner.codegen(cx, state),
            Self::IntLit(inner) => inner.codegen(cx, state),
            Self::BoolLit(inner) => inner.codegen(cx, state),
            Self::UnitLit(inner) => inner.codegen(cx, state),
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Return {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if !cx.current_block_is_terminating() {
            cx.builder.build_return(Some(&cx.value(state, &self.value)));
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Br {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if !cx.current_block_is_terminating() {
            cx.builder.build_unconditional_branch(state.block(self.target));
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BrIf {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        if !cx.current_block_is_terminating() {
            cx.builder.build_conditional_branch(
                cx.value(state, &self.cond).into_int_value(),
                state.block(self.b1),
                state.block(self.b2),
            );
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Phi {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let ty = state.function(cx).register(self.register).unwrap().ty.llvm_type(cx);
        let phi = cx.builder.build_phi(ty, "phi");

        for (blk, value) in &*self.values {
            let value = cx.value(state, value);
            let bb = state.block(*blk);
            phi.add_incoming(&[(&value, bb)]);
        }

        state.set_register(self.register, phi.as_basic_value());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Call {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let callee = cx.value(state, &self.callee).into_pointer_value();

        let result = cx.builder.build_call(
            CallableValue::try_from(callee).expect("a callable pointer value"),
            &[],
            "call",
        );

        let result_value = result.try_as_basic_value().expect_left("expected a return value");

        let printf = cx.module.add_function(
            "printf",
            cx.isize_ty
                .fn_type(&[cx.context.i8_type().ptr_type(AddressSpace::default()).into()], true),
            Some(Linkage::External),
        );

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

        state.set_register(self.register, result_value);
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for LoadGlobal {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let value = cx.function(self.id).as_global_value().as_pointer_value();
        state.set_register(self.register, value.into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Binary {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        const NAME: &str = "result";

        let lhs = cx.value(state, &self.lhs).into_int_value();
        let rhs = cx.value(state, &self.rhs).into_int_value();

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

        state.set_register(self.register, result.into());
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
        let ty =
            state.function(cx).register(self.register).unwrap().ty.llvm_type(cx).into_int_type();

        // TODO: unsigned integers
        let value = ty.const_int(self.value as u64, true);
        state.set_register(self.register, value.into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for BoolLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let value = cx.context.bool_type().const_int(u64::from(self.value), false);
        state.set_register(self.register, value.into());
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for UnitLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        state.set_register(self.register, cx.unit_value().into());
    }
}
