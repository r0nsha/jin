use ustr::{ustr, Ustr};

use crate::span::{Source, Span};

pub fn tokenize(source: &Source) -> Vec<Token> {
    Lexer::new(source).scan()
}

struct Lexer<'a> {
    source: &'a Source,
    source_bytes: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a Source) -> Self {
        Self {
            source,
            source_bytes: source.source().as_bytes(),
            pos: 0,
        }
    }

    fn scan(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(token) = self.next_token() {
            tokens.push(token);
        }

        tokens
    }

    fn next_token(&mut self) -> Option<Token> {
        let start = self.pos;

        let ch = self.bump()?;

        let kind = match ch {
            '/' => {
                if let Some('/') = self.peek() {
                    self.advance();
                    self.comment();
                    return self.next_token();
                } else {
                    todo!("unknown character /")
                }
            }
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenCurly,
            '}' => TokenKind::CloseCurly,
            '=' => TokenKind::Eq,
            ch if ch.is_ascii_alphabetic() || ch == '_' => self.ident(start),
            ch if ch.is_ascii_digit() => self.numeric(start),
            ch if ch.is_ascii_whitespace() => return self.next_token(),
            // TODO: diagnostic
            c => todo!("unknown character {c}"),
        };

        Some(Token {
            kind,
            span: self.source.span().subspan(start as u32, self.pos as u32),
        })
    }

    fn ident(&mut self, start: usize) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                return match self.range(start) {
                    "fn" => TokenKind::Fn,
                    "return" => TokenKind::Return,
                    str => TokenKind::Ident(ustr(str)),
                };
            }
        }

        unreachable!()
    }

    fn numeric(&mut self, start: usize) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch == '_' && self.peek_offset(1).map_or(false, |c| c.is_ascii_digit()) {
                self.advance();
                self.advance();
            } else {
                // TODO: diagnostic
                return TokenKind::Int(self.range(start).replace('_', "").parse().unwrap());
            }
        }

        // TODO: diagnostic when number ends with _

        unreachable!()
    }

    fn comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                return;
            } else {
                self.advance()
            }
        }
    }

    #[inline]
    fn advance(&mut self) {
        self.pos += 1;
    }

    fn range(&self, start: usize) -> &str {
        &self.source.source()[start..start + (self.pos - start)]
    }

    fn peek(&self) -> Option<char> {
        self.source_bytes.get(self.pos).map(|c| *c as char)
    }

    fn peek_offset(&self, offset: usize) -> Option<char> {
        self.source_bytes.get(self.pos + offset).map(|c| *c as char)
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek();
        self.advance();
        ch
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn ident(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(ident) => ident,
            kind => panic!("expected Ident, got {kind:?}"),
        }
    }

    pub fn int(&self) -> usize {
        match self.kind {
            TokenKind::Int(value) => value,
            kind => panic!("expected Int, got {kind:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    // Delimiters
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,

    // Symbols
    Eq,

    // Ident & Keywords
    Ident(Ustr),
    Fn,
    Return,

    // Values
    Int(usize),
}
