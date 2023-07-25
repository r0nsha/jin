use std::{fs, io, path::PathBuf};

use slotmap::SlotMap;

#[derive(Debug)]
pub struct State {
    pub file_cache: FileCache,
}

impl State {
    pub fn new() -> Self {
        Self {
            file_cache: FileCache::new(),
        }
    }
}

#[derive(Debug)]
pub struct FileCache(SlotMap<FileKey, Source>);

impl FileCache {
    fn new() -> Self {
        Self(SlotMap::with_key())
    }

    pub fn add(&mut self, path: PathBuf) -> io::Result<FileKey> {
        let source = Source::try_from(path)?;
        Ok(self.0.insert(source))
    }
}

#[derive(Debug)]
pub struct Source {
    path: PathBuf,
    contents: String,
    source: ariadne::Source,
}

impl TryFrom<PathBuf> for Source {
    type Error = io::Error;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let contents = fs::read_to_string(value)?;
        let source = ariadne::Source::from(contents);

        Ok(Self {
            path: value,
            contents,
            source,
        })
    }
}

slotmap::new_key_type! {
    pub struct FileKey;
}
