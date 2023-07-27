use std::{
    env,
    path::{Path, PathBuf},
};

use crate::span::SourceCache;

#[derive(Debug)]
pub struct State {
    root_dir: PathBuf,
    pub options: CompilerOptions,
    pub source_cache: SourceCache,
}

impl State {
    pub fn new(options: CompilerOptions) -> Self {
        Self {
            root_dir: env::current_dir().unwrap(),
            options,
            source_cache: SourceCache::new(),
        }
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }
}

#[derive(Debug)]
pub struct CompilerOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_typed_ast: bool,
}
