use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    common::IdVec,
    hir::{self, BindingId, ModuleId},
    span::Span,
};

use super::{CheckContext, CheckError, CheckResult};

pub(super) fn create_modules_and_global_scope(
    cx: &mut CheckContext,
    modules: &[ast::Module],
) -> CheckResult<()> {
    // for module in modules {
    //     let module_id = cx.cache.insert_module(hir::Module::from(module));
    //     let module_full_name = cx.cache.get_module(module_id).unwrap().name().clone();
    //
    //     let mut global_bindings = UstrMap::default();
    //     let mut defined_names = UstrMap::<Span>::default();
    //
    //     for binding in &module.bindings {
    //         let qualified_name = module_full_name.clone().child(binding.name());
    //
    //         let ty = cx.typecx.fresh_var(binding.span);
    //
    //         let binding_id = cx.cache.insert_binding_info(hir::BindingInfo {
    //             module_id,
    //             id: BindingId::null(),
    //             qualified_name,
    //             vis: ast::Vis::Public,
    //             scope: hir::BindingScope::Global,
    //             ty,
    //             span: binding.span,
    //         });
    //
    //         let name = binding.name();
    //         let span = binding.span;
    //
    //         if let Some(prev_span) = defined_names.insert(name, span) {
    //             return Err(CheckError::DuplicateName {
    //                 name,
    //                 prev_span,
    //                 dup_span: span,
    //             });
    //         } else {
    //             global_bindings.insert(name, binding_id);
    //         }
    //     }
    //
    //     cx.global_scope.0.insert(module_id, global_bindings);
    // }
    //
    // Ok(())
    todo!()
}

#[derive(Debug)]
pub(crate) struct GlobalScope(IdVec<ModuleId, UstrMap<BindingId>>);

impl GlobalScope {
    pub(crate) fn new() -> Self {
        Self(IdVec::new())
    }

    pub(crate) fn find_binding(&self, module_id: ModuleId, name: Ustr) -> Option<BindingId> {
        self.0
            .get(module_id)
            .and_then(|bindings| bindings.get(&name))
            .copied()
    }
}
