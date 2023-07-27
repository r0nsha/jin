use std::{
    io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::span::{Source, SourceCache, SourceId};

#[derive(Debug)]
pub struct State {
    root_dir: PathBuf,
    pub root_source_id: SourceId,
    options: CompilerOptions,
    pub source_cache: SourceCache,
}

impl State {
    pub fn new(options: CompilerOptions, root_file: PathBuf) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut source_cache = SourceCache::new();
        let root_source_id = source_cache.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            root_source_id,
            options,
            source_cache,
        })
    }

    pub fn root_source_id(&self) -> SourceId {
        self.root_source_id
    }

    pub fn root_source(&self) -> &Source {
        self.source_cache.get(self.root_source_id).unwrap()
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn options(&self) -> &CompilerOptions {
        &self.options
    }
}

#[derive(Debug)]
pub struct CompilerOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_typed_ast: bool,
}
