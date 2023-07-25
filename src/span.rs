use std::{cmp::Ordering, fmt, sync::Arc};

use miette::{MietteError, MietteSpanContents, SourceCode, SourceOffset, SourceSpan, SpanContents};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    low: u32,
    high: u32,
}

impl Span {
    pub fn unknown() -> Self {
        Self { low: 0, high: 0 }
    }

    pub fn new(low: u32, high: u32) -> Self {
        Self { low, high }
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

    pub fn subspan(&self, low: u32, high: u32) -> Span {
        assert!(high >= low);
        assert!(self.low + high <= self.high);

        Span {
            low: self.low + low,
            high: self.low + high,
        }
    }

    pub fn contains(&self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            low: self.low.min(other.low),
            high: self.high.min(other.high),
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        // Self::new(value.offset() as u32, (value.len() - value.offset()) as u32)
        Self::from(value.low() as usize..value.high() as usize)
    }
}

#[derive(Debug)]
pub struct SourceMap {
    sources: Vec<Arc<Source>>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self { sources: vec![] }
    }

    pub fn add_source(&mut self, name: String, source: String) -> Arc<Source> {
        let low = self.end_pos() + 1;
        let high = low as u64 + source.len() as u64;

        let mut lines = vec![low];
        lines.extend(
            source
                .match_indices('\n')
                .map(|(p, _)| low as u32 + (p + 1) as u32),
        );

        let source = Arc::new(Source {
            span: Span::new(low, high as u32),
            name,
            source,
            lines,
        });

        self.sources.push(source.clone());

        source
    }

    fn end_pos(&self) -> u32 {
        self.sources.last().map(|x| x.span.high).unwrap_or(0)
    }

    pub fn find_source(&self, pos: u32) -> &Arc<Source> {
        self.sources
            .binary_search_by(|source| {
                if source.span.high < pos {
                    Ordering::Less
                } else if source.span.low > pos {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .ok()
            .map(|i| &self.sources[i])
            .expect("Mapping unknown source location")
    }

    pub fn look_up_pos(&self, pos: u32) -> Loc {
        let source = self.find_source(pos);
        let position = source.find_line_col(pos);

        Loc {
            source: source.clone(),
            position,
        }
    }

    pub fn look_up_span(&self, span: Span) -> SpanLoc {
        let source = self.find_source(span.low);
        let begin = source.find_line_col(span.low);
        let end = source.find_line_col(span.high);
        SpanLoc {
            source: source.clone(),
            begin,
            end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    span: Span,
    name: String,
    source: String,
    lines: Vec<u32>,
}

impl Source {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn source(&self) -> &str {
        self.source.as_ref()
    }

    pub fn find_line(&self, pos: u32) -> usize {
        assert!(pos >= self.span.low);
        assert!(pos <= self.span.high);

        match self.lines.binary_search(&pos) {
            Ok(i) => i,
            Err(i) => i - 1,
        }
    }

    pub fn find_line_col(&self, pos: u32) -> LineCol {
        let line = self.find_line(pos);
        let line_span = self.line_span(line);
        let byte_col = pos - line_span.low;
        let col = self.source_slice(line_span)[..byte_col as usize]
            .chars()
            .count();

        LineCol { line, col }
    }

    pub fn source_slice(&self, span: Span) -> &str {
        assert!(self.span.contains(span));
        &self.source[((span.low - self.span.low) as usize)..((span.high - self.span.low) as usize)]
    }

    pub fn line_span(&self, line: usize) -> Span {
        assert!(line < self.lines.len());
        Span {
            low: self.lines[line],
            high: *self.lines.get(line + 1).unwrap_or(&self.span.high),
        }
    }

    pub fn source_line(&self, line: usize) -> &str {
        self.source_slice(self.line_span(line))
            .trim_end_matches(&['\n', '\r'][..])
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }
}

impl SourceCode for Source {
    fn read_span<'a>(
        &'a self,
        span: &SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        let line_col = self.find_line_col(span.offset() as u32);

        println!("{:?}", line_col);

        Ok(Box::new(MietteSpanContents::new_named(
            self.name().to_string(),
            self.source().as_bytes(),
            *span,
            line_col.line(),
            line_col.col(),
            self.line_count(),
        )))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineCol {
    line: usize,
    col: usize,
}

impl LineCol {
    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Loc {
    pub source: Arc<Source>,
    pub position: LineCol,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}:{}:{}",
            self.source.name,
            self.position.line + 1,
            self.position.col + 1
        )
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SpanLoc {
    pub source: Arc<Source>,
    pub begin: LineCol,
    pub end: LineCol,
}

impl fmt::Display for SpanLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.begin == self.end {
            write!(
                f,
                "{}:{}:{}",
                self.source.name,
                self.begin.line + 1,
                self.begin.col + 1
            )
        } else {
            write!(
                f,
                "{}:{}:{}: {}:{}",
                self.source.name,
                self.begin.line + 1,
                self.begin.col + 1,
                self.end.line + 1,
                self.end.col + 1
            )
        }
    }
}
