use phf::phf_map;
use std::ops::Range;
use std::str::Chars;

static UNESCAPES: phf::Map<char, char> = phf_map! {
    '"' => '"',
    '\'' => '\'',
    'n' => '\n',
    'r' => '\r',
    't' => '\t',
    '\\' => '\\',
    '0' => '\0',
};

pub fn unescape(input: &str) -> Result<String, UnescapeError> {
    Unescape::run(input)
}

struct Unescape<'a> {
    chars: Chars<'a>,
    res: String,
    pos: u32,
}

impl<'a> Unescape<'a> {
    fn run(s: &'a str) -> Result<String, UnescapeError> {
        let this = Self { chars: s.chars(), res: String::with_capacity(s.len()), pos: 0 };
        this.unescape()
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
            if let Some(&esc) = UNESCAPES.get(&ch) {
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

static ESCAPES: phf::Map<char, &'static str> = phf_map! {
    '"' => r#"\""#,
    '\'' => r#"\'"#,
    '\n' => r#"\n"#,
    '\r' => r#"\r"#,
    '\t' => r#"\t"#,
    '\\' => r#"\\"#,
    '\0' => r#"\0"#,
};

pub fn escape(input: &str) -> String {
    Escape::run(input)
}

struct Escape<'a> {
    chars: Chars<'a>,
    res: String,
}

impl<'a> Escape<'a> {
    fn run(s: &'a str) -> String {
        let this = Self { chars: s.chars(), res: String::with_capacity(s.len()) };
        this.escape()
    }

    fn escape(mut self) -> String {
        for ch in self.chars {
            if let Some(unesc) = ESCAPES.get(&ch) {
                self.res.push_str(unesc);
            } else {
                self.res.push(ch);
            }
        }

        self.res
    }
}
