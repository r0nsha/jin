use std::{
    io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::span::{Source, SourceCache, SourceId};

#[derive(Debug)]
pub struct State {
    root_dir: PathBuf,
    root_source_id: SourceId,
    build_options: BuildOptions,
    pub source_cache: SourceCache,
}

impl State {
    pub fn new(build_options: BuildOptions, root_file: PathBuf) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut source_cache = SourceCache::new();
        let root_source_id = source_cache.insert_file(absolute_path.to_path_buf())?;

        Ok(Self {
            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            root_source_id,
            build_options,
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

    pub fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }
}

#[derive(Debug)]
pub struct BuildOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_hir: bool,
}
