use std::{
    fmt, fs, io,
    path::{Path, PathBuf},
};

use miette::NamedSource;
use slotmap::{Key, SlotMap};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    source_key: SourceKey,
    start: u32,
    end: u32,
}

impl Span {
    pub fn new(source_key: SourceKey, start: u32, end: u32) -> Self {
        Self {
            source_key,
            start,
            end,
        }
    }

    pub fn unknown() -> Self {
        Self {
            source_key: SourceKey::null(),
            start: 0,
            end: 0,
        }
    }

    pub fn source_key(&self) -> SourceKey {
        self.source_key
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
        assert!(self.source_key == other.source_key);

        Self {
            source_key: self.source_key,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        (span.start as usize..span.end as usize).into()
    }
}

#[derive(Debug)]
pub struct SourceCache(SlotMap<SourceKey, Source>);

impl SourceCache {
    pub fn new() -> Self {
        Self(SlotMap::with_key())
    }

    pub fn add_file(&mut self, path: PathBuf) -> io::Result<SourceKey> {
        let mut source = Source::try_from(path)?;

        Ok(self.0.insert_with_key(|key| {
            source.key = key;
            source
        }))
    }

    pub fn get(&self, key: SourceKey) -> Option<&Source> {
        self.0.get(key)
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    key: SourceKey,
    path: PathBuf,
    contents: String,
}

impl Source {
    pub fn key(&self) -> SourceKey {
        self.key
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
            key: SourceKey::null(),
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
    pub struct SourceKey;
}
