use crate::span::SourceMap;

#[derive(Debug)]
pub struct State {
    pub source_map: SourceMap,
}

impl State {
    pub fn new() -> Self {
        Self {
            source_map: SourceMap::new(),
        }
    }
}
