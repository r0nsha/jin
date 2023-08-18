use std::collections::HashMap;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{AnyType, IntType},
    values::FunctionValue,
    AddressSpace,
};

use crate::{
    db::{Database, DefinitionId},
    llvm::ty::LlvmType,
    mir::Mir,
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

#[derive(Clone)]
pub struct FunctionState<'cx> {
    pub function: FunctionValue<'cx>,
    pub decl_block: BasicBlock<'cx>,
    pub current_block: BasicBlock<'cx>,
}

impl<'cx> FunctionState<'cx> {
    pub fn new(
        function: FunctionValue<'cx>,
        decl_block: BasicBlock<'cx>,
        entry_block: BasicBlock<'cx>,
    ) -> Self {
        Self { function, decl_block, current_block: entry_block }
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

        let function = self.module.add_function(
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

        function.get_nth_param(0).unwrap().set_name("argc");
        function.get_nth_param(1).unwrap().set_name("argv");

        let decl_block = self.context.append_basic_block(function, "decls");
        let entry_block = self.context.append_basic_block(function, "entry");

        let mut state = FunctionState::new(function, decl_block, entry_block);

        self.start_block(&mut state, entry_block);

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

        self.start_block(&mut state, decl_block);

        self.builder.build_unconditional_branch(entry_block);
    }

    pub fn declare_all_functions(&mut self) {
        for fun in &self.mir.functions {
            let fun_info = &self.db[fun.id()];
            let name = fun_info.qualified_name.standard_full_name();
            let llvm_ty = fun_info
                .ty
                .llvm_type(self)
                .into_pointer_type()
                .get_element_type()
                .into_function_type();

            self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
        }
    }

    pub fn define_all_functions(&mut self) {
        for fun in &self.mir.functions {
            let fun_info = &self.db[fun.id()];
            let name = fun_info.qualified_name.standard_full_name();
            let llvm_ty = fun_info
                .ty
                .llvm_type(self)
                .into_pointer_type()
                .get_element_type()
                .into_function_type();

            self.module.add_function(&name, llvm_ty, Some(Linkage::Private));
        }
    }
}
