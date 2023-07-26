use slotmap::SlotMap;
use ustr::Ustr;

use crate::span::SourceCache;

#[derive(Debug)]
pub struct State {
    pub options: CompilerOptions,
    pub source_cache: SourceCache,
    pub binding_cache: BindingCache,
}

impl State {
    pub fn new(options: CompilerOptions) -> Self {
        Self {
            options,
            source_cache: SourceCache::new(),
        }
    }
}

#[derive(Debug)]
pub struct CompilerOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_typed_ast: bool,
}

#[derive(Debug)]
pub struct BindingCache(SlotMap<BindingKey, BindingInfo>);

slotmap::new_key_type! {
    pub struct BindingKey;
}

#[derive(Debug)]
pub struct BindingInfo {
    name: Ustr,
    qualified_name: Ustr,
    scope: BindingScope,
    uses: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingScope {
    Global,
    Scope(usize),
}

impl BindingScope {
    pub fn next(self) -> Self {
        match self {
            BindingScope::Global => BindingScope::Scope(1),
            BindingScope::Scope(n) => BindingScope::Scope(n + 1),
        }
    }

    pub fn prev(self) -> Self {
        match self {
            BindingScope::Global => panic!("BindingScope::Global has no previous scope"),
            BindingScope::Scope(n) if n == 1 => BindingScope::Global,
            BindingScope::Scope(n) => BindingScope::Scope(n - 1),
        }
    }
}
