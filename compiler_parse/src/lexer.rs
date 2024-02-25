use compiler_helpers::escape;
use ustr::ustr;

use compiler_core::{
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    span::{Source, SourceId, Span},
};

use crate::token::{Kw, Token, TokenKind};

pub fn tokenize(source: &Source) -> DiagnosticResult<Vec<Token>> {
    Lexer::new(source).scan()
}

struct Lexer<'s> {
    source_id: SourceId,
    source_contents: &'s str,
    source_bytes: &'s [u8],
    pos: u32,
    encountered_nl: bool,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s Source) -> Self {
        Self {
            source_id: source.id(),
            source_contents: source.contents(),
            source_bytes: source.contents().as_bytes(),
            pos: 0,
            encountered_nl: false,
        }
    }

    fn scan(mut self) -> DiagnosticResult<Vec<Token>> {
        let mut tokens = vec![];

        while let Some(tok) = self.eat_token()? {
            self.auto_insert_semi(&mut tokens, &tok);
            tokens.push(tok);
            self.encountered_nl = false;
        }

        Self::auto_insert_semi_eof(&mut tokens);

        Ok(tokens)
    }

    #[inline]
    fn auto_insert_semi(&self, tokens: &mut Vec<Token>, tok: &Token) {
        if self.encountered_nl && tok.kind.is_after_semi() {
            if let Some(before_tok) = Self::can_auto_insert_semi_after_last(tokens) {
                Self::insert_semi(tokens, before_tok.span)
            }
        }
    }

    #[inline]
    fn auto_insert_semi_eof(tokens: &mut Vec<Token>) {
        if let Some(before_tok) = Self::can_auto_insert_semi_after_last(tokens) {
            Self::insert_semi(tokens, before_tok.span.tail());
        }
    }

    #[inline]
    fn insert_semi(tokens: &mut Vec<Token>, span: Span) {
        let semi = Token { kind: TokenKind::Semi(true), span: span.tail() };
        tokens.push(semi);
    }

    #[inline]
    fn can_auto_insert_semi_after_last(tokens: &[Token]) -> Option<&Token> {
        tokens.last().and_then(|t| t.kind.is_before_semi().then_some(t))
    }

    #[allow(clippy::too_many_lines)]
    fn eat_token(&mut self) -> DiagnosticResult<Option<Token>> {
        let start = self.pos;

        match self.bump() {
            Some(ch) => {
                let kind = match ch {
                    ch if ch.is_ascii_alphabetic() || ch == '_' => {
                        if ch == 'b' && self.eat('\'') {
                            self.eat_char(CharKind::Byte, start + 2)?
                        } else {
                            self.eat_ident(start)
                        }
                    }
                    ch if ch.is_ascii_digit() => self.eat_number(start),
                    ch if ch.is_ascii_whitespace() => {
                        if ch == '\n' {
                            self.encountered_nl = true;
                        }
                        return self.eat_token();
                    }
                    '"' => self.eat_str(start + 1)?,
                    '\'' => self.eat_char(CharKind::Char, start + 1)?,
                    '#' => {
                        self.eat_comment();
                        return self.eat_token();
                    }
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
                    ':' => {
                        if self.eat('=') {
                            TokenKind::Walrus
                        } else {
                            TokenKind::Colon
                        }
                    }
                    ';' => TokenKind::Semi(false),
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
                        return Err(Diagnostic::error(format!("invalid character {ch}"))
                            .with_label(Label::primary(span, "invalid character")));
                    }
                };

                Ok(Some(Token { kind, span: self.create_span(start) }))
            }
            None => Ok(None),
        }
    }

    fn eat_ident(&mut self, start: u32) -> TokenKind {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.next();
            } else {
                let s = self.range(start);

                return if s == "_" {
                    TokenKind::Underscore
                } else if let Ok(kw) = Kw::try_from(s) {
                    TokenKind::Kw(kw)
                } else {
                    TokenKind::Ident(ustr(s))
                };
            }
        }

        unreachable!()
    }

    fn eat_number(&mut self, start: u32) -> TokenKind {
        if self.peek_offset(-1) == Some('0') {
            match self.peek() {
                Some('x' | 'X') => return self.eat_number_hex(),
                Some('o' | 'O') => return self.eat_number_octal(),
                Some('b' | 'B') => return self.eat_number_binary(),
                _ => (),
            }
        }

        self.eat_number_decimal();

        if self.peek() == Some('.') && self.peek_offset(1).map_or(false, |c| c.is_ascii_digit()) {
            self.next();
            self.eat_number_decimal();
            return TokenKind::Float(self.number_range(start).parse().unwrap());
        }

        TokenKind::Int(self.number_range(start).parse().unwrap())
    }

    fn eat_number_decimal(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() || ch == '_' {
                self.next();
            } else {
                break;
            }
        }
    }

    fn eat_number_hex(&mut self) -> TokenKind {
        self.next();
        let start = self.pos;

        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' | 'a'..='f' | 'A'..='F' | '_' => self.next(),
                _ => break,
            }
        }

        self.int_range_radix(start, 16)
    }

    fn eat_number_octal(&mut self) -> TokenKind {
        self.next();
        let start = self.pos;

        while let Some(ch) = self.peek() {
            match ch {
                '0'..='7' | '_' => self.next(),
                _ => break,
            }
        }

        self.int_range_radix(start, 8)
    }

    fn eat_number_binary(&mut self) -> TokenKind {
        self.next();
        let start = self.pos;

        while let Some(ch) = self.peek() {
            match ch {
                '0' | '1' | '_' => self.next(),
                _ => break,
            }
        }

        self.int_range_radix(start, 2)
    }

    fn int_range_radix(&self, start: u32, radix: u32) -> TokenKind {
        let value = i128::from_str_radix(&self.number_range(start), radix).unwrap();
        TokenKind::Int(value)
    }

    fn eat_str(&mut self, start: u32) -> DiagnosticResult<TokenKind> {
        let s = self.eat_terminated_lit(start, '"', "string")?;
        let unescaped = escape::unescape(s).map_err(|e| self.unescape_err(e, start))?;
        Ok(TokenKind::Str(ustr(&unescaped)))
    }

    fn eat_char(&mut self, kind: CharKind, start: u32) -> DiagnosticResult<TokenKind> {
        let s = self.eat_terminated_lit(start, '\'', "char")?;
        let unescaped = escape::unescape(s).map_err(|e| self.unescape_err(e, start))?;

        let char_count = unescaped.chars().count();
        if char_count != 1 {
            return Err(Diagnostic::error("character literal can only contain one codepoint")
                .with_label(Label::primary(
                    self.create_span_range(start, self.pos - 1),
                    format!("contains {char_count} codepoints"),
                )));
        }

        let ch = unescaped.chars().nth(0).unwrap();

        match kind {
            CharKind::Char => Ok(TokenKind::Char(ch)),
            CharKind::Byte => {
                if !ch.is_ascii() {
                    return Err(Diagnostic::error(format!(
                        "non-ascii character `{ch}` in byte char"
                    ))
                    .with_label(Label::primary(self.create_span(self.pos), "non-ascii char")));
                }

                Ok(TokenKind::ByteChar(ch))
            }
        }
    }

    fn eat_terminated_lit<'a>(
        &'a mut self,
        start: u32,
        term: char,
        kind: &str,
    ) -> DiagnosticResult<&'a str> {
        let mut last: Option<char> = None;

        loop {
            match self.bump() {
                Some(ch) if last != Some('\\') && ch == term => break,
                Some(ch) => last = Some(ch),
                None => {
                    return Err(Diagnostic::error(format!(
                        "missing trailing `{term}` to end the {kind}"
                    ))
                    .with_label(Label::primary(
                        self.create_span(self.pos),
                        format!("unterminated {kind}"),
                    )));
                }
            }
        }

        let s = self.range(start);
        Ok(&s[..s.len() - 1])
    }

    fn unescape_err(&self, e: escape::UnescapeError, start: u32) -> Diagnostic {
        match e {
            escape::UnescapeError::InvalidEscape(r) => Diagnostic::error("invalid escape sequence")
                .with_label(Label::primary(
                    self.create_span_range(start + r.start, start + r.end),
                    "invalid sequence",
                )),
        }
    }

    fn eat_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                self.encountered_nl = true;
                return;
            }

            self.next();
        }
    }

    #[inline]
    fn next(&mut self) {
        self.pos += 1;
    }

    fn number_range(&self, start: u32) -> String {
        self.range(start).replace('_', "")
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

    fn peek_offset(&self, offset: i32) -> Option<char> {
        self.source_bytes.get(self.pos.saturating_add_signed(offset) as usize).map(|c| *c as char)
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek();
        self.next();
        ch
    }

    #[inline]
    fn create_span(&self, start: u32) -> Span {
        self.create_span_range(start, self.pos)
    }

    #[inline]
    fn create_span_range(&self, start: u32, end: u32) -> Span {
        Span::new(self.source_id, start, end)
    }
}

#[derive(Debug)]
enum CharKind {
    Char,
    Byte,
}
