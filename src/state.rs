use crate::span::SourceCache;

#[derive(Debug)]
pub struct State {
    pub options: CompilerOptions,
    pub source_cache: SourceCache,
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
    pub time: bool,
}
