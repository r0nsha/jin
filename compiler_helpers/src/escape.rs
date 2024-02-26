use phf::phf_map;
use std::str::Chars;

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
