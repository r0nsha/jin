use std::cmp::Ordering;

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

struct Lexer<'a> {
    source_id: SourceId,
    source_contents: &'a str,
    source_bytes: &'a [u8],
    pos: u32,

    // Layout state
    indents: Vec<u32>,
    line: u32,
    col: u32,

    // String interpolation state
    modes: Vec<Mode>,
    curlies: usize,
    curly_stack: Vec<usize>,
}

#[derive(Debug, Clone, Copy)]
enum Mode {
    Default,
    Str,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a Source) -> Self {
        Self {
            source_id: source.id(),
            source_contents: source.contents(),
            source_bytes: source.contents().as_bytes(),
            pos: 0,
            indents: vec![0],
            line: 1,
            col: 1,
            modes: vec![Mode::Default],
            curlies: 0,
            curly_stack: vec![],
        }
    }

    fn scan(mut self) -> DiagnosticResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.source_contents.len() / 2);
        let mut last_line = self.line;

        while let Some(tok) = self.eat_token()? {
            let is_new_line = self.line > last_line;
            let col = self.col - tok.span.len();

            let mut curr_tok = tok;

            // Brace insertion
            if is_new_line {
                if col > self.layout_indent() && !Self::is_expr_cont(&tokens, &tok) {
                    let t = Token { kind: TokenKind::OpenCurly, span: tok.span.head() };
                    curr_tok = t;
                    tokens.push(t)
                }

                if col < self.layout_indent() && tok.kind != TokenKind::CloseCurly {
                    let t = Token { kind: TokenKind::CloseCurly, span: tok.span.head() };
                    curr_tok = t;
                    tokens.push(t)
                }
            }

            // Layout stack indentation
            if tokens.is_empty() || tokens.last().map_or(false, |t| t.kind == TokenKind::OpenCurly)
            {
                if curr_tok.kind != TokenKind::CloseCurly && col < self.layout_indent() {
                    return Err(Diagnostic::error(format!(
                        "line must be indented more than its \
                         enclosing layout (column {})",
                        self.layout_indent()
                    ))
                    .with_label(Label::primary(tok.span, "insufficient indentation")));
                }

                self.indents.push(col);
            }

            if curr_tok.kind == TokenKind::CloseCurly {
                self.indents.pop();
            }

            // Semicolon insertion
            if is_new_line {
                match (col.cmp(&self.layout_indent()), tokens.last().map(|t| t.kind)) {
                    (Ordering::Less, _) => {
                        return Err(Diagnostic::error(format!(
                            "line must be indented the same as or more \
                             than its layout (column {})",
                            self.layout_indent()
                        ))
                        .with_label(Label::primary(tok.span, "invalid indentation")));
                    }
                    (Ordering::Equal, Some(TokenKind::Semi(_))) => (),
                    (Ordering::Equal, _) if !Self::is_expr_cont(&tokens, &curr_tok) => {
                        let span = tokens.last().map_or(tok.span.head(), |t| t.span.tail());
                        tokens.push(Token { kind: TokenKind::Semi(true), span: span.head() });
                    }
                    _ => (),
                }
            }

            last_line = self.line;
            tokens.push(tok);
        }

        if let Some(span) = tokens.last().map(|t| t.span) {
            if self.layout_indent() > 1 {
                tokens.push(Token { kind: TokenKind::CloseCurly, span: span.tail() })
            }

            tokens.push(Token { kind: TokenKind::Semi(true), span: span.tail() });
        }

        Ok(tokens)
    }

    fn is_expr_cont(tokens: &[Token], tok: &Token) -> bool {
        tok.kind.is_start_cont() || tokens.last().map_or(false, |t| t.kind.is_end_cont())
    }

    #[track_caller]
    fn layout_indent(&self) -> u32 {
        *self.indents.last().unwrap()
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
                            self.eat_ident(start)?
                        }
                    }
                    ch if ch.is_ascii_digit() => match (ch, self.peek()) {
                        ('0', Some('x' | 'X')) => self.eat_number_hex(),
                        ('0', Some('o' | 'O')) => self.eat_number_octal(),
                        ('0', Some('b' | 'B')) => self.eat_number_binary(),
                        _ => self.eat_number(start),
                    },
                    ch if ch.is_ascii_whitespace() => return self.eat_token(),
                    '`' => self.eat_raw_ident()?,
                    '"' => {
                        self.modes.push(Mode::Str);
                        TokenKind::StrOpen
                    }
                    '\'' => self.eat_char(CharKind::Char, start + 1)?,
                    '(' => TokenKind::OpenParen,
                    ')' => TokenKind::CloseParen,
                    '[' => TokenKind::OpenBrack,
                    ']' => TokenKind::CloseBrack,
                    '{' => self.open_curly(),
                    '}' => self.close_curly(),
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
                        if self.eat('/') {
                            self.eat_comment();
                            return self.eat_token();
                        }

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

    fn open_curly(&mut self) -> TokenKind {
        self.curlies += 1;
        TokenKind::OpenCurly
    }

    fn close_curly(&mut self) -> TokenKind {
        self.curlies = self.curlies.saturating_sub(1);

        if self.curly_stack.last().copied() == Some(self.curlies) {
            self.curly_stack.pop();
            self.modes.pop();
            TokenKind::StrExprClose
        } else {
            TokenKind::CloseCurly
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
            Some('{') => {
                self.next();
                self.modes.push(Mode::Default);
                self.curly_stack.push(self.curlies);
                self.curlies += 1;
                Ok(Some(Token {
                    kind: TokenKind::StrExprOpen,
                    span: self.create_span(self.pos - 2),
                }))
            }
            Some(_) => Ok(Some(self.eat_str_text(start)?)),
            None => Ok(None),
        }
    }

    fn eat_ident(&mut self, start: u32) -> DiagnosticResult<TokenKind> {
        let mut last_hyphen = false;

        while let Some(ch) = self.peek() {
            match ch {
                '0'..='9' if last_hyphen => {
                    return Err(Diagnostic::error(
                        "to avoid confusion with the `-` operator, a digit cannot follow a hyphen",
                    )
                    .with_label(Label::primary(
                        self.create_span_range(self.pos - 1, self.pos + 1),
                        "a hyphen followed by a digit",
                    )));
                }
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    last_hyphen = false;
                    self.next();
                }
                '-' => {
                    last_hyphen = true;
                    self.next();
                }
                _ => break,
            }
        }

        if last_hyphen {
            return Err(Diagnostic::error("identifier cannot end with a hyphen").with_label(
                Label::primary(self.create_span(start), "identifier with a trailing hyphen"),
            ));
        }

        let s = self.range(start);

        if s == "_" {
            Ok(TokenKind::Underscore)
        } else {
            Ok(Kw::try_from(s).map(TokenKind::Kw).unwrap_or_else(|s| TokenKind::Ident(ustr(s))))
        }
    }

    fn eat_number(&mut self, start: u32) -> TokenKind {
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
                Some('"' | '{') => break,
                Some('\\') => {
                    self.next();
                    let ch = self.eat_unescaped_char()? as u8;
                    buf.push(ch);
                }
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

    fn eat_unescaped_char(&mut self) -> DiagnosticResult<char> {
        if let Some(&esc) = self.bump().and_then(|ch| UNESCAPES.get(&ch)) {
            Ok(esc)
        } else {
            Err(Diagnostic::error("invalid escape sequence")
                .with_label(Label::primary(self.create_span(self.pos), "invalid sequence")))
        }
    }

    fn eat_raw_ident(&mut self) -> DiagnosticResult<TokenKind> {
        let start = self.pos;

        loop {
            match self.bump() {
                Some('`') => break,
                Some(_) => (),
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

        let s = self.range(start);
        Ok(TokenKind::Ident(ustr(&s[..s.len() - 1])))
    }

    fn eat_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.next();
        }
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

    fn next(&mut self) {
        match self.peek() {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
            }
            Some(ch) => {
                self.col += unicode_width::UnicodeWidthChar::width(ch).unwrap_or(0) as u32;
            }
            None => (),
        }
        self.pos += 1;
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
    '{' => '{',
    '0' => '\0',
};
