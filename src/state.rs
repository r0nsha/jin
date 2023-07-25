use crate::span::SourceCache;

#[derive(Debug)]
pub struct State {
    pub source_cache: SourceCache,
}

impl State {
    pub fn new() -> Self {
        Self {
            source_cache: SourceCache::new(),
        }
    }
}
