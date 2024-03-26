use std::{fs, io, ops};

use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::files::{self, line_starts};
use compiler_data_structures::{
    index_vec::{IndexVec, IndexVecExt, Key as _},
    new_key_type,
};
use rustc_hash::FxHashMap;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    source_id: SourceId,
    start: u32,
    end: u32,
}

impl Span {
    pub fn unknown() -> Self {
        Self { source_id: SourceId::null(), start: 0, end: 0 }
    }

    pub fn new(source_id: SourceId, start: u32, end: u32) -> Self {
        Self { source_id, start, end }
    }

    pub fn initial(source_id: SourceId) -> Self {
        Self { source_id, start: 0, end: 0 }
    }

    pub fn uniform(source_id: SourceId, n: u32) -> Self {
        Self { source_id, start: n, end: n }
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

    pub fn tail(&self) -> Span {
        let mut new = *self;
        new.start = self.end;
        new
    }

    #[allow(unused)]
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

impl core::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}, {}..{})", self.source_id, self.start, self.end)
    }
}

pub trait Spanned
where
    Self: Sized,
{
    fn span(&self) -> Span;
}

#[derive(Debug)]
pub struct Sources {
    sources: IndexVec<SourceId, Source>,
    path_to_source: FxHashMap<Utf8PathBuf, SourceId>,
}

new_key_type! {
    pub struct SourceId;
}

impl Sources {
    pub fn new() -> Self {
        Self { sources: IndexVec::new(), path_to_source: FxHashMap::default() }
    }

    pub fn load_file(&mut self, path: Utf8PathBuf) -> io::Result<SourceId> {
        if let Some(id) = self.path_to_source.get(&path) {
            return Ok(*id);
        }

        let mut source = Source::try_from(path.clone())?;
        let id = self.sources.push_with_key(|id| {
            source.id = id;
            source
        });
        self.path_to_source.insert(path, id);

        Ok(id)
    }

    pub fn get(&self, id: SourceId) -> Option<&Source> {
        self.sources.get(id)
    }

    pub fn find_by_path(&self, path: &Utf8Path) -> Option<&Source> {
        self.path_to_source.get(path).and_then(|id| self.get(*id))
    }
}

impl ops::Index<SourceId> for Sources {
    type Output = Source;

    fn index(&self, index: SourceId) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl Default for Sources {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    id: SourceId,
    path: Utf8PathBuf,
    contents: String,
    line_starts: Vec<usize>,
}

impl Source {
    #[inline]
    pub fn id(&self) -> SourceId {
        self.id
    }

    #[inline]
    pub fn path(&self) -> &Utf8Path {
        &self.path
    }

    pub fn file_name(&self) -> String {
        self.path.file_stem().expect("a source to be a file").to_string()
    }

    #[inline]
    pub fn contents(&self) -> &str {
        self.contents.as_ref()
    }

    fn line_start(&self, line_index: usize) -> Result<usize, files::Error> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => {
                Ok(self.line_starts.get(line_index).copied().expect("line index to be found"))
            }
            Ordering::Equal => Ok(self.contents.len()),
            Ordering::Greater => Err(files::Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }
}

impl TryFrom<Utf8PathBuf> for Source {
    type Error = io::Error;

    fn try_from(value: Utf8PathBuf) -> Result<Self, Self::Error> {
        let contents = fs::read_to_string(&value)?;
        let line_starts = line_starts(&contents).collect();

        Ok(Self { id: SourceId::null(), path: value, contents, line_starts })
    }
}

impl<'a> files::Files<'a> for Source {
    type FileId = SourceId;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, _id: Self::FileId) -> Result<Self::Name, files::Error> {
        Ok(self.path().as_str())
    }

    fn source(&'a self, _id: Self::FileId) -> Result<Self::Source, files::Error> {
        Ok(self.contents())
    }

    fn line_index(&'a self, _id: Self::FileId, byte_index: usize) -> Result<usize, files::Error> {
        Ok(self.line_starts.binary_search(&byte_index).unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        _id: Self::FileId,
        line_index: usize,
    ) -> Result<ops::Range<usize>, files::Error> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Ok(line_start..next_line_start)
    }
}

impl<'a> files::Files<'a> for Sources {
    type FileId = SourceId;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
        self.get(id).ok_or(files::Error::FileMissing).and_then(|source| source.name(id))
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
        self.get(id).ok_or(files::Error::FileMissing).and_then(|source| source.source(id))
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
