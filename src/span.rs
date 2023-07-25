use std::{fs, io, path::PathBuf};

use slotmap::SlotMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    source_key: SourceKey,
    low: u32,
    high: u32,
}

impl Span {
    pub fn new(file_key: SourceKey, low: u32, high: u32) -> Self {
        Self {
            source_key: file_key,
            low,
            high,
        }
    }

    pub fn unknown() -> Self {
        Self {
            source_key: SourceKey::default(),
            low: 0,
            high: 0,
        }
    }

    pub fn file_key(&self) -> SourceKey {
        self.source_key
    }

    pub fn low(&self) -> u32 {
        self.low
    }

    pub fn high(&self) -> u32 {
        self.high
    }

    pub fn len(&self) -> u32 {
        self.high - self.low
    }

    pub fn contains(&self, other: Self) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn merge(&self, other: Self) -> Self {
        assert!(self.source_key == other.source_key);

        Self {
            source_key: self.source_key,
            low: self.low.min(other.low),
            high: self.high.min(other.high),
        }
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
}

#[derive(Debug)]
pub struct Source {
    key: SourceKey,
    path: PathBuf,
    contents: String,
    source: ariadne::Source,
}

impl Source {
    pub fn key(&self) -> SourceKey {
        self.key
    }

    pub fn path(&self) -> &PathBuf {
        &self.path
    }

    pub fn contents(&self) -> &str {
        self.contents.as_ref()
    }

    pub fn source(&self) -> &ariadne::Source {
        &self.source
    }
}

impl TryFrom<PathBuf> for Source {
    type Error = io::Error;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let contents = fs::read_to_string(value)?;
        let source = ariadne::Source::from(contents);

        Ok(Self {
            key: SourceKey::default(),
            path: value,
            contents,
            source,
        })
    }
}

slotmap::new_key_type! {
    pub struct SourceKey;
}
