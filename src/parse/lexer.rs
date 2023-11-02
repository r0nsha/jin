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
    pos: u32,
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
                    DQUOTE => self.eat_str(start + 1)?,
                    '#' => TokenKind::Hash,
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '[' => TokenKind::OpenBracket,
                    ']' => TokenKind::CloseBracket,
                    '{' => TokenKind::OpenCurly,
                    '}' => TokenKind::CloseCurly,
                    ',' => TokenKind::Comma,
                    '.' => TokenKind::Dot,
                    ':' => TokenKind::Colon,
                    '@' => TokenKind::At,
                    '=' => {
                        if self.eat('=') {
                            TokenKind::EqEq
                        } else {
                            TokenKind::Eq
                        }
                    }
                    '!' => {
                        if self.eat('=') {
                            TokenKind::BangEq
                        } else {
                            TokenKind::Bang
                        }
                    }
                    '*' => TokenKind::Star,
                    '/' => {
                        if self.eat('/') {
                            self.eat_comment();
                            return self.eat_token();
                        }

                        TokenKind::FwSlash
                    }
                    '%' => TokenKind::Percent,
                    '+' => TokenKind::Plus,
                    '-' => {
                        if self.eat('>') {
                            TokenKind::Arrow
                        } else {
                            TokenKind::Minus
                        }
                    }
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
                        let span = self.create_span(start);
                        return Err(TokenizeError::InvalidChar(ch, span));
                    }
                };

                Ok(Some(Token { kind, span: self.create_span(start) }))
            }
            None => Ok(None),
        }
    }

    fn ident(&mut self, start: u32) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.next();
            } else {
                return match self.range(start) {
                    "_" => TokenKind::Underscore,
                    "return" => TokenKind::Return,
                    "fn" => TokenKind::Fn,
                    "let" => TokenKind::Let,
                    "extern" => TokenKind::Extern,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "as" => TokenKind::As,
                    "import" => TokenKind::Import,
                    str => TokenKind::Ident(ustr(str)),
                };
            }
        }

        unreachable!()
    }

    fn numeric(&mut self, start: u32) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.next();
            } else if ch == '_' && self.peek_offset(1).map_or(false, |c| c.is_ascii_digit()) {
                self.next();
                self.next();
            } else {
                return TokenKind::Int(
                    self.range(start).replace('_', "").parse().expect("to be a valid integer"),
                );
            }
        }

        unreachable!()
    }

    fn eat_str(&mut self, start: u32) -> Result<TokenKind, TokenizeError> {
        loop {
            match self.bump() {
                Some('"') => {
                    let str = self.range(start);
                    // Removes the ending double-quote
                    let str = &str[..str.len() - 1];

                    // TODO: unescaping
                    // let stripped_str = unescaper::unescape(str).map_err(|err| match err {
                    //     unescaper::Error::IncompleteStr(pos) => TokenizeError::EscapeIncompleteStr(
                    //         Span::uniform(self.source_id, start + pos as u32),
                    //     ),
                    //     unescaper::Error::InvalidChar { char, pos } => {
                    //         TokenizeError::EscapeInvalidChar(
                    //             char,
                    //             Span::uniform(self.source_id, start + pos as u32),
                    //         )
                    //     }
                    //     unescaper::Error::ParseIntError { pos, .. } => {
                    //         TokenizeError::EscapeParseIntError(Span::uniform(
                    //             self.source_id,
                    //             start + pos as u32,
                    //         ))
                    //     }
                    // })?;

                    return Ok(TokenKind::Str(ustr(str)));
                }
                Some(_) => (),
                None => break,
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

    fn range(&self, start: u32) -> &str {
        let start = start as usize;
        let end = start + (self.pos as usize - start);
        &self.source_contents[start..end]
    }

    fn peek(&self) -> Option<char> {
        self.source_bytes.get(self.pos as usize).map(|c| *c as char)
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
        self.source_bytes.get(self.pos as usize + offset).map(|c| *c as char)
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek();
        self.next();
        ch
    }

    fn create_span(&self, start: u32) -> Span {
        Span::new(self.source_id, start, self.pos)
    }
}

type TokenizeResult<T> = Result<T, TokenizeError>;

#[derive(Debug)]
enum TokenizeError {
    InvalidChar(char, Span),
    EscapeIncompleteStr(Span),
    EscapeInvalidChar(char, Span),
    EscapeParseIntError(Span),
}

impl From<TokenizeError> for Diagnostic {
    fn from(err: TokenizeError) -> Self {
        match err {
            TokenizeError::InvalidChar(ch, span) => Self::error("parse::invalid_char")
                .with_message(format!("invalid character {ch}"))
                .with_label(Label::primary(span)),
            TokenizeError::EscapeIncompleteStr(span) => Self::error("parse::escape_incomplete_str")
                .with_message("incomplete string in escape sequence")
                .with_label(Label::primary(span)),
            TokenizeError::EscapeInvalidChar(ch, span) => Self::error("parse::escape_invalid_char")
                .with_message(format!("invalid character {ch} in escape sequence"))
                .with_label(Label::primary(span)),
            TokenizeError::EscapeParseIntError(span) => Self::error("parse::escape_invalid_int")
                .with_message("invalid integer in escape sequence")
                .with_label(Label::primary(span)),
        }
    }
}

const DQUOTE: char = '"';
