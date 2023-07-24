use std::ops::Range;

use ustr::{ustr, Ustr};

use crate::span::{Source, Span};

pub fn tokenize(source: &Source) -> Tokens {
    let mut lexer = Lexer::new(source);

    lexer.run();

    Tokens {
        tokens: lexer.tokens,
    }
}

struct Lexer<'a> {
    source: &'a Source,
    source_bytes: &'a [u8],
    source_len: usize,
    pos: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a Source) -> Self {
        Self {
            source,
            source_bytes: source.source().as_bytes(),
            source_len: source.source().len(),
            pos: 0,
            tokens: vec![],
        }
    }

    fn run(&mut self) {
        while let Some(token) = self.next_token() {
            dbg!(token);
            self.advance();
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        let start = self.pos;

        let ch = self.peek()?;

        let kind = match ch {
            'a'..='z' | 'A'..='Z' | '_' => self.ident(start),
            ' ' | '\t' | '\r' | '\n' => return self.next_token(),
            // TODO: diagnostic
            c => todo!("unknown character {c}"),
        };

        Some(Token {
            kind,
            span: self.source.span().subspan(start as u32, self.pos as u32),
        })
    }

    fn ident(&mut self, start: usize) -> TokenKind {
        while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = self.peek() {
            self.advance();
        }

        dbg!(self.pos);
        let str = self.range(start);

        return match str {
            "fn" => TokenKind::Fn,
            str => TokenKind::Ident(ustr(str)),
        };
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn range(&self, start: usize) -> &str {
        &self.source.source()[start..start + (self.pos - start)]
    }

    pub fn peek(&self) -> Option<char> {
        self.source_bytes.get(self.pos).map(|c| *c as char)
    }
}

#[derive(Debug)]
pub struct Tokens {
    tokens: Vec<Token>,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Ident(Ustr),
    Fn,
}
