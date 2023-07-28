use std::{
    fs, io,
    path::{Path, PathBuf},
};

use codespan_reporting::files;
use miette::NamedSource;
use slotmap::{Key, SlotMap};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    source_id: SourceId,
    start: u32,
    end: u32,
}

impl Span {
    pub fn new(source_id: SourceId, start: u32, end: u32) -> Self {
        Self {
            source_id,
            start,
            end,
        }
    }

    pub fn unknown() -> Self {
        Self {
            source_id: SourceId::null(),
            start: 0,
            end: 0,
        }
    }

    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn end(&self) -> u32 {
        self.end
    }

    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    pub fn contains(&self, other: Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    pub fn merge(&self, other: Self) -> Self {
        assert!(self.source_id == other.source_id);

        Self {
            source_id: self.source_id,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        let start = span.start as usize;
        let end = span.end as usize;
        Self::from(start..end)
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Debug)]
pub struct SourceCache(SlotMap<SourceId, Source>);

impl SourceCache {
    pub fn new() -> Self {
        Self(SlotMap::with_key())
    }

    pub fn insert_file(&mut self, path: PathBuf) -> io::Result<SourceId> {
        let mut source = Source::try_from(path)?;

        Ok(self.0.insert_with_key(|id| {
            source.id = id;
            source
        }))
    }

    pub fn get(&self, id: SourceId) -> Option<&Source> {
        self.0.get(id)
    }
}

impl<'a> files::Files<'a> for SourceCache {
    type FileId = SourceId;

    type Name = String;

    type Source = String;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .map(|source| source.path().display().to_string())
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .map(|source| source.contents().to_string())
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        todo!()
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    id: SourceId,
    path: PathBuf,
    contents: String,
}

impl Source {
    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn contents(&self) -> &str {
        self.contents.as_ref()
    }
}

impl TryFrom<PathBuf> for Source {
    type Error = io::Error;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let contents = fs::read_to_string(&value)?;

        Ok(Self {
            id: SourceId::null(),
            path: value,
            contents,
        })
    }
}

impl From<&Source> for NamedSource {
    fn from(source: &Source) -> Self {
        Self::new(
            source.path().to_string_lossy().to_string(),
            source.contents().to_string(),
        )
    }
}

slotmap::new_key_type! {
    pub struct SourceId;
}
