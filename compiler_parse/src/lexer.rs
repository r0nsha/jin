use phf::phf_map;
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
    modes: Vec<Mode>,
    parens: usize,
    parens_stack: Vec<usize>,
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    Default,
    Str,
}

impl<'s> Lexer<'s> {
    fn new(source: &'s Source) -> Self {
        Self {
            source_id: source.id(),
            source_contents: source.contents(),
            source_bytes: source.contents().as_bytes(),
            pos: 0,
            encountered_nl: false,
            modes: vec![],
            parens: 0,
            parens_stack: vec![],
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

    fn eat_token(&mut self) -> DiagnosticResult<Option<Token>> {
        match self.modes.last() {
            Some(Mode::Str) => self.eat_token_str(),
            _ => self.eat_token_default(),
        }
    }

    fn eat_token_default(&mut self) -> DiagnosticResult<Option<Token>> {
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
                    '"' => {
                        self.modes.push(Mode::Str);
                        TokenKind::StrOpen
                    }
                    '\'' => self.eat_char(CharKind::Char, start + 1)?,
                    '#' => {
                        self.eat_comment();
                        return self.eat_token();
                    }
                    '(' => self.open_paren(),
                    ')' => self.close_paren(),
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

    fn open_paren(&mut self) -> TokenKind {
        self.parens += 1;
        TokenKind::OpenParen
    }

    fn close_paren(&mut self) -> TokenKind {
        self.parens = self.parens.saturating_sub(1);

        if self.parens_stack.last().copied() == Some(self.parens) {
            self.parens_stack.pop();
            self.modes.pop();
            TokenKind::StrExprClose
        } else {
            TokenKind::CloseParen
        }
    }

    fn eat_token_str(&mut self) -> DiagnosticResult<Option<Token>> {
        let start = self.pos - 1;

        match self.peek() {
            Some('"') => {
                self.next();
                self.modes.pop();
                Ok(Some(Token { kind: TokenKind::StrClose, span: self.create_span(start) }))
            }
            Some('\\') if self.peek_offset(1) == Some('(') => {
                self.next();
                self.next();
                Ok(Some(self.eat_str_expr_open()))
            }
            Some(_) => Ok(Some(self.eat_str_text(start)?)),
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

    fn eat_char(&mut self, kind: CharKind, start: u32) -> DiagnosticResult<TokenKind> {
        let mut buf = Vec::<u8>::new();

        loop {
            match self.bump() {
                Some('\\') => buf.push(self.eat_unescaped_char()? as u8),
                Some('\'') => break,
                Some(ch) => buf.push(ch as u8),
                None => {
                    return Err(Diagnostic::error(
                        "missing trailing ' to end the character literal",
                    )
                    .with_label(Label::primary(
                        self.create_span(self.pos),
                        "unterminated character",
                    )))
                }
            }
        }

        // SAFETY: the buf is constructed from utf8-encoded chars,
        // so it remains utf8-encoded.
        let s = unsafe { String::from_utf8_unchecked(buf) };

        let char_count = s.chars().count();
        if char_count != 1 {
            return Err(Diagnostic::error("character literal can only contain one codepoint")
                .with_label(Label::primary(
                    self.create_span_range(start, self.pos - 1),
                    format!("contains {char_count} codepoints"),
                )));
        }

        let ch = s.chars().nth(0).unwrap();

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

    fn eat_str_text(&mut self, start: u32) -> DiagnosticResult<Token> {
        let mut buf = Vec::<u8>::new();

        loop {
            match self.peek() {
                Some('\\') => {
                    if self.peek_offset(1) == Some('(') {
                        break;
                    }

                    self.next();
                    let ch = self.eat_unescaped_char()? as u8;
                    buf.push(ch);
                }
                Some('"') => break,
                Some(ch) => {
                    self.next();
                    buf.push(ch as u8)
                }
                None => {
                    return Err(Diagnostic::error("missing trailing \" to end the string")
                        .with_label(Label::primary(
                            self.create_span(self.pos),
                            "unterminated string",
                        )))
                }
            }
        }

        // SAFETY: the buf is constructed from utf8-encoded chars,
        // so it remains utf8-encoded.
        let s = unsafe { String::from_utf8_unchecked(buf) };
        Ok(Token { kind: TokenKind::StrText(ustr(&s)), span: self.create_span(start) })
    }

    fn eat_str_expr_open(&mut self) -> Token {
        self.modes.push(Mode::Default);
        self.parens_stack.push(self.parens);
        self.parens += 1;
        Token { kind: TokenKind::StrExprOpen, span: self.create_span(self.pos - 2) }
    }

    fn eat_unescaped_char(&mut self) -> DiagnosticResult<char> {
        if let Some(&esc) = self.bump().and_then(|ch| UNESCAPES.get(&ch)) {
            Ok(esc)
        } else {
            Err(Diagnostic::error("invalid escape sequence")
                .with_label(Label::primary(self.create_span(self.pos), "invalid sequence")))
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

static UNESCAPES: phf::Map<char, char> = phf_map! {
    '"' => '"',
    '\'' => '\'',
    'n' => '\n',
    'r' => '\r',
    't' => '\t',
    '\\' => '\\',
    '0' => '\0',
};
