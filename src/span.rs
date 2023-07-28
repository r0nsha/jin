use std::{
    fs, io, ops,
    path::{Path, PathBuf},
};

use codespan_reporting::files::{self, line_starts};
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

#[derive(Debug, Clone)]
pub struct Source {
    id: SourceId,
    path: PathBuf,
    contents: String,
    line_starts: Vec<usize>,
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

    fn line_start(&self, line_index: usize) -> Result<usize, files::Error> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => Ok(self.line_starts.get(line_index).cloned().unwrap()),
            Ordering::Equal => Ok(self.contents.len()),
            Ordering::Greater => Err(files::Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

impl TryFrom<PathBuf> for Source {
    type Error = io::Error;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let contents = fs::read_to_string(&value)?;
        let line_starts = line_starts(&contents).collect();

        Ok(Self {
            id: SourceId::null(),
            path: value,
            contents,
            line_starts,
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

impl<'a> files::Files<'a> for Source {
    type FileId = SourceId;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        Ok(self.path().to_string_lossy().as_ref())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        Ok(self.contents())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, files::Error> {
        Ok(self
            .line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<ops::Range<usize>, files::Error> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

impl<'a> files::Files<'a> for SourceCache {
    type FileId = SourceId;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .and_then(|source| source.name(id))
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .and_then(|source| source.source(id))
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .and_then(|source| source.line_index(id, byte_index))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<ops::Range<usize>, files::Error> {
        self.get(id)
            .ok_or(files::Error::FileMissing)
            .and_then(|source| source.line_range(id, line_index))
    }
}

slotmap::new_key_type! {
    pub struct SourceId;
}
