use crate::state::FileKey;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    file_key: FileKey,
    low: u32,
    high: u32,
}

impl Span {
    pub fn unknown() -> Self {
        Self {
            file_key: FileKey::default(),
            low: 0,
            high: 0,
        }
    }

    pub fn file_key(&self) -> FileKey {
        self.file_key
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

    pub fn subspan(&self, low: u32, high: u32) -> Self {
        assert!(high >= low);
        assert!(self.low + high <= self.high);

        Self {
            file_key: self.file_key,
            low: self.low + low,
            high: self.low + high,
        }
    }

    pub fn contains(&self, other: Self) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn merge(&self, other: Self) -> Self {
        assert!(self.file_key == other.file_key);

        Self {
            file_key: self.file_key,
            low: self.low.min(other.low),
            high: self.high.min(other.high),
        }
    }
}
