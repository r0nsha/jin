use ustr::ustr;

use crate::{
    ast::token::{Token, TokenKind},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    span::{Source, SourceId, Span},
};

pub fn tokenize(source: &Source) -> DiagnosticResult<Vec<Token>> {
    Lexer::new(source).scan()
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

    fn scan(mut self) -> DiagnosticResult<Vec<Token>> {
        let mut tokens = vec![];

        while let Some(token) = self.eat_token()? {
            tokens.push(token);
        }

        Ok(tokens)
    }

    #[allow(clippy::too_many_lines)]
    fn eat_token(&mut self) -> DiagnosticResult<Option<Token>> {
        let start = self.pos;

        match self.bump() {
            Some(ch) => {
                let kind = match ch {
                    ch if ch.is_ascii_alphabetic() || ch == '_' => {
                        self.ident(start)
                    }
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
                    '.' => {
                        if self.eat('.') {
                            TokenKind::DotDot
                        } else {
                            TokenKind::Dot
                        }
                    }
                    ':' => TokenKind::Colon,
                    '@' => TokenKind::At,
                    '?' => TokenKind::QuestionMark,
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
                    '*' => {
                        if self.eat('=') {
                            TokenKind::StarEq
                        } else {
                            TokenKind::Star
                        }
                    }
                    '/' => {
                        if self.eat('=') {
                            TokenKind::FwSlashEq
                        } else if self.eat('/') {
                            self.eat_comment();
                            return self.eat_token();
                        } else {
                            TokenKind::FwSlash
                        }
                    }
                    '%' => {
                        if self.eat('=') {
                            TokenKind::PercentEq
                        } else {
                            TokenKind::Percent
                        }
                    }
                    '+' => {
                        if self.eat('=') {
                            TokenKind::PlusEq
                        } else {
                            TokenKind::Plus
                        }
                    }
                    '-' => {
                        if self.eat('=') {
                            TokenKind::MinusEq
                        } else if self.eat('>') {
                            TokenKind::Arrow
                        } else {
                            TokenKind::Minus
                        }
                    }
                    '<' => {
                        if self.eat('<') {
                            if self.eat('=') {
                                TokenKind::LtLtEq
                            } else {
                                TokenKind::LtLt
                            }
                        } else if self.eat('=') {
                            TokenKind::LtEq
                        } else {
                            TokenKind::Lt
                        }
                    }
                    '>' => {
                        if self.eat('>') {
                            if self.eat('=') {
                                TokenKind::GtGtEq
                            } else {
                                TokenKind::GtGt
                            }
                        } else if self.eat('=') {
                            TokenKind::GtEq
                        } else {
                            TokenKind::Gt
                        }
                    }
                    '&' => {
                        if self.eat('=') {
                            TokenKind::AmpEq
                        } else if self.eat('&') {
                            TokenKind::AmpAmp
                        } else {
                            TokenKind::Amp
                        }
                    }
                    '|' => {
                        if self.eat('=') {
                            TokenKind::PipeEq
                        } else if self.eat('|') {
                            TokenKind::PipePipe
                        } else {
                            TokenKind::Pipe
                        }
                    }
                    '^' => {
                        if self.eat('=') {
                            TokenKind::CaretEq
                        } else {
                            TokenKind::Caret
                        }
                    }
                    ch => {
                        let span = self.create_span(start);
                        return Err(Diagnostic::error()
                            .with_message(format!("invalid character {ch}"))
                            .with_label(Label::primary(span)));
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
                return match self.range_from(start) {
                    "_" => TokenKind::Underscore,
                    "return" => TokenKind::Return,
                    "fn" => TokenKind::Fn,
                    "let" => TokenKind::Let,
                    "type" => TokenKind::Type,
                    "extern" => TokenKind::Extern,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "match" => TokenKind::Match,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "as" => TokenKind::As,
                    "import" => TokenKind::Import,
                    "loop" => TokenKind::Loop,
                    "break" => TokenKind::Break,
                    "mut" => TokenKind::Mut,
                    "imm" => TokenKind::Imm,
                    str => TokenKind::Ident(ustr(str)),
                };
            }
        }

        unreachable!()
    }

    fn numeric(&mut self, start: u32) -> TokenKind {
        self.eat_int_aux();

        if self.peek() == Some('.')
            && self.peek_offset(1).map_or(false, |c| c.is_ascii_digit())
        {
            self.next();
            self.eat_int_aux();
            TokenKind::Float(ustr(self.range_from(start)))
        } else {
            TokenKind::Int(ustr(self.range_from(start)))
        }
    }

    fn eat_int_aux(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '_' {
                self.next();
            } else {
                return;
            }
        }

        unreachable!()
    }

    fn eat_str(&mut self, start: u32) -> DiagnosticResult<TokenKind> {
        loop {
            match self.bump() {
                Some('"') => {
                    let str = self.range_from(start);
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

    fn range_from(&self, start: u32) -> &str {
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

const DQUOTE: char = '"';
