use phf::phf_map;
use std::ops::Range;
use std::str::Chars;

static ESCAPES: phf::Map<char, char> = phf_map! {
    '"' => '"',
    '\'' => '\'',
    'n' => '\n',
    'r' => '\r',
    't' => '\t',
    '\\' => '\\',
    '0' => '\0',
};

pub fn unescape(input: &str) -> Result<String, UnescapeError> {
    Unescape::new(input.chars()).unescape()
}

struct Unescape<'a> {
    chars: Chars<'a>,
    res: String,
    pos: u32,
}

impl<'a> Unescape<'a> {
    fn new(chars: Chars<'a>) -> Self {
        Self { chars, res: String::new(), pos: 0 }
    }

    fn unescape(mut self) -> Result<String, UnescapeError> {
        while let Some(ch) = self.next() {
            let ch = match ch {
                '\\' => self.seq()?,
                ch => ch,
            };

            self.res.push(ch);
        }

        Ok(self.res)
    }

    fn seq(&mut self) -> Result<char, UnescapeError> {
        let start = self.pos - 1;

        if let Some(ch) = self.next() {
            if let Some(&esc) = ESCAPES.get(&ch) {
                return Ok(esc);
            }
        }

        Err(UnescapeError::InvalidEscape(start..self.pos))
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        self.chars.next().map(|ch| {
            self.pos += 1;
            ch
        })
    }
}

#[derive(Debug)]
pub enum UnescapeError {
    InvalidEscape(Range<u32>),
}
