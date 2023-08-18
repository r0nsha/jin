use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::IntType,
    values::{BasicValueEnum, CallableValue, FunctionValue},
    AddressSpace,
};

use crate::{
    db::{Database, DefinitionId},
    llvm::ty::LlvmType,
    mir::{
        Block, BlockId, Call, Function, Instruction, IntLit, Mir, RegisterId, Return, UnitLit,
        Value,
    },
};

pub struct Generator<'db, 'cx> {
    pub db: &'db Database,
    pub mir: &'db Mir,

    pub context: &'cx Context,
    pub module: &'db Module<'cx>,
    pub builder: &'db Builder<'cx>,
    pub isize_ty: IntType<'cx>,

    pub functions: HashMap<DefinitionId, FunctionValue<'cx>>,
}

#[derive(Debug, Clone)]
pub struct FunctionState<'cx> {
    pub id: DefinitionId,
    pub function_value: FunctionValue<'cx>,
    pub decl_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
    blocks: HashMap<BlockId, BasicBlock<'cx>>,
    registers: HashMap<RegisterId, BasicValueEnum<'cx>>,
}

impl<'cx> FunctionState<'cx> {
    pub fn new(
        id: DefinitionId,
        function_value: FunctionValue<'cx>,
        decl_block: BasicBlock<'cx>,
        blocks: HashMap<BlockId, BasicBlock<'cx>>,
    ) -> Self {
        let current_block = *blocks.get(&BlockId::first()).expect("to have a start block");

        Self {
            id,
            function_value,
            decl_block,
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
        *self.registers.get(&id).expect("to be a valid BlockId")
    }

    pub fn set_register(&mut self, id: RegisterId, value: BasicValueEnum<'cx>) {
        self.registers.insert(id, value);
    }
}

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn run(&mut self) {
        self.gen_start_function();
    }

    pub fn gen_start_function(&mut self) {
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

        self.declare_all_functions();
        self.define_all_functions();

        // TODO: Codegen the entry point function
        // Codegen the entry point function
        // let entry_point_function = self.db.main_function().expect("to have a main function");

        // TODO: Call the entry point function
        // Call the entry point function
        // let entry_point_function_value = *self.functions.get(&entry_point_function.id).unwrap();
        // let entry_point_function_type =
        //     self.db[entry_point_function.ty].as_function().expect("to be a function type");
        // self.gen_function_call(
        //     &mut state,
        //     entry_point_function_value,
        //     &entry_point_function_type,
        //     vec![],
        //     &entry_point_function_type.return_type,
        // );

        if self.current_block().get_terminator().is_none() {
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

        let decl_block = self.context.append_basic_block(function_value, "decls");

        let mut blocks = HashMap::default();

        for blk in fun.blocks() {
            let bb = self.context.append_basic_block(function_value, &blk.name);
            blocks.insert(blk.id, bb);
        }

        let mut state = FunctionState::new(id, function_value, decl_block, blocks);

        self.builder.build_unconditional_branch(state.block(BlockId::first()));

        for blk in fun.blocks() {
            blk.codegen(self, &mut state);
        }

        function_value.as_global_value().as_pointer_value().into()
    }

    fn function(&self, id: DefinitionId) -> FunctionValue<'cx> {
        *self.functions.get(&id).expect("function to be declared")
    }

    fn get_value(&self, state: &FunctionState<'cx>, value: &Value) -> BasicValueEnum<'cx> {
        match value {
            Value::Definition(id) => self.function(*id).as_global_value().as_pointer_value().into(),
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

impl<'db, 'cx> Codegen<'db, 'cx> for Instruction {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        match self {
            Self::Return(inner) => inner.codegen(cx, state),
            Self::Call(inner) => inner.codegen(cx, state),
            Self::IAdd(inner) => {
                let left = cx.get_value(state, &inner.left).into_int_value();
                let right = cx.get_value(state, &inner.right).into_int_value();
                let result = cx.builder.build_int_add(left, right, "iadd");
                state.set_register(inner.register, result.into());
            }
            Self::IntLit(inner) => inner.codegen(cx, state),
            Self::UnitLit(inner) => inner.codegen(cx, state),
        }
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Return {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        cx.builder.build_return(Some(&cx.get_value(state, &self.value)));
    }
}

impl<'db, 'cx> Codegen<'db, 'cx> for Call {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        let callee = cx.get_value(state, &self.callee).into_pointer_value();

        let result = cx.builder.build_call(
            CallableValue::try_from(callee).expect("a callable pointer value"),
            &[],
            "call",
        );

        state.set_register(
            self.register,
            result.try_as_basic_value().expect_left("expected a return value"),
        );
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

impl<'db, 'cx> Codegen<'db, 'cx> for UnitLit {
    fn codegen(&self, cx: &mut Generator<'db, 'cx>, state: &mut FunctionState<'cx>) {
        state.set_register(self.register, cx.unit_value().into());
    }
}
