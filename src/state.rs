use std::{
    io,
    path::{Path, PathBuf},
};

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use path_absolutize::Absolutize;

use crate::{
    diagnostics::{Diagnostic, Diagnostics},
    span::{Source, SourceCache, SourceId},
};

#[derive(Debug)]
pub(crate) struct State {
    build_options: BuildOptions,
    root_dir: PathBuf,
    root_source_id: SourceId,
    pub(crate) source_cache: SourceCache,
    pub(crate) diagnostics: Diagnostics,
}

impl State {
    pub(crate) fn new(build_options: BuildOptions, root_file: PathBuf) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut source_cache = SourceCache::new();
        let root_source_id = source_cache.insert_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,
            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            root_source_id,
            source_cache,
            diagnostics: Diagnostics::new(),
        })
    }

    pub(crate) fn root_source_id(&self) -> SourceId {
        self.root_source_id
    }

    pub(crate) fn root_source(&self) -> &Source {
        self.source_cache.get(self.root_source_id).unwrap()
    }

    pub(crate) fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub(crate) fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }
}

#[derive(Debug)]
pub(crate) struct BuildOptions {
    pub(crate) print_times: bool,
    pub(crate) print_ast: bool,
    pub(crate) print_hir: bool,
}

#[derive(Debug)]
pub(crate) struct Database {}
