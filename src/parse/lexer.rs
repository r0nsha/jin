use ustr::ustr;

use crate::{
    ast::token::{Token, TokenKind},
    diagnostics::{Diagnostic, Label},
    span::{Source, SourceId, Span},
};

pub fn tokenize(source: &Source) -> Result<Vec<Token>, Diagnostic> {
    let tokens = Lexer::new(source).scan()?;
    Ok(tokens)
}

struct Lexer<'s> {
    source_id: SourceId,
    source_contents: &'s str,
    source_bytes: &'s [u8],
    pos: usize,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s Source) -> Self {
        Self {
            source_id: source.id(),
            source_contents: source.contents(),
            source_bytes: source.contents().as_bytes(),
            pos: 0,
        }
    }

    fn scan(mut self) -> TokenizeResult<Vec<Token>> {
        let mut tokens = vec![];

        while let Some(token) = self.eat_token()? {
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn eat_token(&mut self) -> TokenizeResult<Option<Token>> {
        let start = self.pos;

        match self.bump() {
            Some(ch) => {
                let kind = match ch {
                    ch if ch.is_ascii_alphabetic() || ch == '_' => self.ident(start),
                    ch if ch.is_ascii_digit() => self.numeric(start),
                    ch if ch.is_ascii_whitespace() => return self.eat_token(),
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '{' => TokenKind::OpenCurly,
                    '}' => TokenKind::CloseCurly,
                    ',' => TokenKind::Comma,
                    '=' => {
                        if self.eat('=') {
                            TokenKind::EqEq
                        } else {
                            TokenKind::Eq
                        }
                    }
                    '!' if self.eat('=') => TokenKind::BangEq,
                    '*' => TokenKind::Star,
                    '/' => {
                        if self.eat('/') {
                            self.next();
                            self.eat_comment();
                            return self.eat_token();
                        }

                        TokenKind::FwSlash
                    }
                    '%' => TokenKind::Percent,
                    '+' => TokenKind::Plus,
                    '-' => TokenKind::Minus,
                    '<' => {
                        if self.eat('<') {
                            TokenKind::LtLt
                        } else if self.eat('=') {
                            TokenKind::LtEq
                        } else {
                            TokenKind::Lt
                        }
                    }
                    '>' => {
                        if self.eat('>') {
                            TokenKind::GtGt
                        } else if self.eat('=') {
                            TokenKind::GtEq
                        } else {
                            TokenKind::Gt
                        }
                    }
                    '&' => {
                        if self.eat('&') {
                            TokenKind::AmpAmp
                        } else {
                            TokenKind::Amp
                        }
                    }
                    '|' => {
                        if self.eat('|') {
                            TokenKind::PipePipe
                        } else {
                            TokenKind::Pipe
                        }
                    }
                    '^' => TokenKind::Caret,
                    ch => {
                        let span = self.create_span(start.try_into().unwrap());
                        return Err(TokenizeError::InvalidChar { ch, span });
                    }
                };

                Ok(Some(Token { kind, span: self.create_span(start.try_into().unwrap()) }))
            }
            None => Ok(None),
        }
    }

    fn ident(&mut self, start: usize) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.next();
            } else {
                return match self.range(start) {
                    "return" => TokenKind::Return,
                    "fn" => TokenKind::Fn,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    str => TokenKind::Ident(ustr(str)),
                };
            }
        }

        unreachable!()
    }

    fn numeric(&mut self, start: usize) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.next();
            } else if ch == '_' && self.peek_offset(1).map_or(false, |c| c.is_ascii_digit()) {
                self.next();
                self.next();
            } else {
                return TokenKind::Int(self.range(start).replace('_', "").parse().unwrap());
            }
        }

        unreachable!()
    }

    fn eat_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                return;
            }

            self.next();
        }
    }

    #[inline]
    fn next(&mut self) {
        self.pos += 1;
    }

    fn range(&self, start: usize) -> &str {
        &self.source_contents[start..start + (self.pos - start)]
    }

    fn peek(&self) -> Option<char> {
        self.source_bytes.get(self.pos).map(|c| *c as char)
    }

    fn eat(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn peek_offset(&self, offset: usize) -> Option<char> {
        self.source_bytes.get(self.pos + offset).map(|c| *c as char)
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek();
        self.next();
        ch
    }

    fn create_span(&self, start: u32) -> Span {
        Span::new(self.source_id, start, self.pos.try_into().unwrap())
    }
}
type TokenizeResult<T> = Result<T, TokenizeError>;

#[derive(Debug)]
enum TokenizeError {
    InvalidChar { ch: char, span: Span },
}

impl From<TokenizeError> for Diagnostic {
    fn from(err: TokenizeError) -> Self {
        match err {
            TokenizeError::InvalidChar { ch, span } => Self::error("tokenize::invalid_char")
                .with_message(format!("invalid character {ch}"))
                .with_label(Label::primary(span)),
        }
    }
}
