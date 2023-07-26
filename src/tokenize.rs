use std::{
    fmt::{self, Write},
    mem,
};

use miette::Diagnostic;
use thiserror::Error;
use ustr::{ustr, Ustr};

use crate::{
    span::{Source, SourceId, Span, Spanned},
    state::State,
    util::ErrExt,
    CompilerResult,
};

pub fn tokenize(state: &State, source: &Source) -> CompilerResult<Vec<Token>> {
    Lexer::new(source)
        .scan()
        .map_err(|err| err.with_source_code(state))
}

struct Lexer<'a> {
    source_key: SourceId,
    source_contents: &'a str,
    source_bytes: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a Source) -> Self {
        Self {
            source_key: source.key(),
            source_contents: source.contents(),
            source_bytes: source.contents().as_bytes(),
            pos: 0,
        }
    }

    fn scan(mut self) -> TokenizeResult<Vec<Token>> {
        let mut tokens = vec![];

        while let Some(token) = self.next_token()? {
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> TokenizeResult<Option<Token>> {
        let start = self.pos;

        match self.bump() {
            Some(ch) => {
                let kind = match ch {
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '{' => TokenKind::OpenCurly,
                    '}' => TokenKind::CloseCurly,
                    '=' => TokenKind::Eq,
                    '/' if matches!(self.peek(), Some('/')) => {
                        self.advance();
                        self.comment();
                        return self.next_token();
                    }
                    ch if ch.is_ascii_alphabetic() || ch == '_' => self.ident(start),
                    ch if ch.is_ascii_digit() => self.numeric(start),
                    ch if ch.is_ascii_whitespace() => return self.next_token(),
                    ch => {
                        let span = self.create_span(start as u32);

                        return Err(TokenizeError::InvalidChar { ch, span });
                        // return Err(create_report(ReportKind::Error, &span)
                        //     .with_message(format!("unknown character {ch}"))
                        //     .with_label(Label::new(span).with_color(Color::Red))
                        //     .finish());
                    }
                };

                Ok(Some(Token {
                    kind,
                    span: self.create_span(start as u32),
                }))
            }
            None => Ok(None),
        }
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
                return TokenKind::Int(self.range(start).replace('_', "").parse().unwrap());
            }
        }

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
        &self.source_contents[start..start + (self.pos - start)]
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

    fn create_span(&self, start: u32) -> Span {
        Span::new(self.source_key, start, self.pos as u32)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn as_ident(&self) -> Ustr {
        match self.kind {
            TokenKind::Ident(ident) => ident,
            kind => panic!("expected Ident, got {kind:?}"),
        }
    }

    pub fn as_int(&self) -> usize {
        match self.kind {
            TokenKind::Int(value) => value,
            kind => panic!("expected Int, got {kind:?}"),
        }
    }

    pub fn kind_eq(&self, other: TokenKind) -> bool {
        mem::discriminant(&self.kind) == mem::discriminant(&other)
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OpenParen => f.write_char('('),
            Self::CloseParen => f.write_char(')'),
            Self::OpenCurly => f.write_char('{'),
            Self::CloseCurly => f.write_char('}'),
            Self::Eq => f.write_char('='),
            Self::Ident(_) => f.write_str("identifier"),
            Self::Fn => f.write_str("fn"),
            Self::Return => f.write_str("return"),
            Self::Int(_) => f.write_str("int literal"),
        }
    }
}

type TokenizeResult<T> = CompilerResult<T, TokenizeError>;

#[derive(Error, Diagnostic, Debug)]
enum TokenizeError {
    #[error("invalid character {ch}")]
    #[diagnostic(code(tokenize::invalid_char))]
    InvalidChar {
        ch: char,
        #[label]
        span: Span,
    },
}

impl Spanned for TokenizeError {
    fn span(&self) -> Span {
        match self {
            TokenizeError::InvalidChar { span, .. } => *span,
        }
    }
}
