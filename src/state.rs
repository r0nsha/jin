use std::{
    io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    database::Database,
    diagnostics::Diagnostics,
    span::{Source, SourceId, Sources},
};

#[derive(Debug)]
pub(crate) struct State {
    build_options: BuildOptions,
    root_dir: PathBuf,
    root_source_id: SourceId,
    pub(crate) db: Database,
    pub(crate) sources: Sources,
    pub(crate) diagnostics: Diagnostics,
}

impl State {
    pub(crate) fn new(build_options: BuildOptions, root_file: PathBuf) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut sources = Sources::new();
        let root_source_id = sources.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,
            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            root_source_id,
            db: Database::new(),
            sources,
            diagnostics: Diagnostics::new(),
        })
    }

    pub(crate) fn root_source_id(&self) -> SourceId {
        self.root_source_id
    }

    pub(crate) fn root_source(&self) -> &Source {
        self.sources.get(self.root_source_id).unwrap()
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
