use std::collections::HashMap;

use inkwell::{
    builder::Builder, context::Context, module::Module, types::IntType, values::FunctionValue,
    AddressSpace,
};

use crate::{
    db::{Database, DefinitionId},
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
                        .ptr_type(AddressSpace::Generic)
                        .ptr_type(AddressSpace::Generic)
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

        let root_module_info = self.workspace.get_root_module_info();

        let mut state = FunctionState::new(
            *root_module_info,
            function,
            startup_fn_type,
            None,
            decl_block,
            entry_block,
        );

        state.push_scope();

        self.start_block(&mut state, entry_block);

        self.startup_function_state = Some(state.clone());

        // Codegen the entry point function
        let entry_point_function = self.cache.entry_point_function().unwrap();

        self.gen_function(entry_point_function.id, None);

        // Call the entry point function
        let entry_point_function_value = *self.functions.get(&entry_point_function.id).unwrap();

        let entry_point_function_type = entry_point_function.ty.normalize(self.tcx).into_function();

        self.gen_function_call(
            &mut state,
            entry_point_function_value,
            &entry_point_function_type,
            vec![],
            &entry_point_function_type.return_type,
        );

        // TODO: if this is DLL Main, return 1 instead of 0

        if self.current_block().get_terminator().is_none() {
            self.builder.build_return(Some(&self.context.i32_type().const_zero()));
        }

        self.start_block(&mut state, decl_block);

        state.pop_scope();

        self.builder.build_unconditional_branch(entry_block);
    }
}
