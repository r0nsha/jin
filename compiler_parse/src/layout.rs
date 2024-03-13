use std::cmp::Ordering;

use codespan_reporting::files::Location;
use compiler_core::diagnostics::{Diagnostic, DiagnosticResult, Label};
use compiler_core::span::{Source, Span};

use crate::token::{Token, TokenKind};

pub(crate) fn apply(source: &Source, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
    if input.is_empty() {
        return Ok(input);
    }

    let mut layout = Layout::new(source, input.capacity());
    layout.apply(input)?;
    Ok(layout.tokens)
}

struct Layout<'a> {
    source: &'a Source,
    tokens: Vec<Token>,
    indents: Vec<Indent>,
    last_line: u32,
}

impl<'a> Layout<'a> {
    const DEFAULT_COL: u32 = 1;

    fn new(source: &'a Source, capacity: usize) -> Self {
        Self { source, tokens: Vec::with_capacity(capacity), indents: vec![], last_line: 1 }
    }

    fn apply(&mut self, input: Vec<Token>) -> DiagnosticResult<()> {
        for tok in input {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let (line, col) = (line_number as u32, column_number as u32);
            let newline = line > self.last_line;

            // Brace insertion
            if newline && col > self.layout_indent() && !self.is_expr_cont(&tok) {
                self.push_open_curly(tok.span.leading());
            }

            if newline
                && col < self.layout_indent()
                && !matches!(tok.kind, TokenKind::OpenCurly(_) | TokenKind::CloseCurly(_))
            {
                let mut last_indent = self.layout_indent_with_span();

                while col < self.layout_indent() {
                    self.push_semi(tok.span.leading());
                    self.push_close_curly(tok.span.leading());
                    last_indent = self.indents.pop().unwrap_or(self.default_indent());
                }

                if col > self.layout_indent() {
                    return Err(insufficient_layout(col, last_indent, tok.span));
                }
            }

            // Layout stack indentation
            if self.last_token().map_or(false, |t| matches!(t.kind, TokenKind::OpenCurly(_))) {
                if !matches!(tok.kind, TokenKind::CloseCurly(_)) && col < self.layout_indent() {
                    return Err(insufficient_layout(col, self.layout_indent_with_span(), tok.span));
                }

                self.indents.push((col, tok.span));
            }

            if matches!(tok.kind, TokenKind::CloseCurly(_)) {
                self.indents.pop();
            }

            // Semicolon insertion
            if newline && !self.tokens.is_empty() {
                match col.cmp(&self.layout_indent()) {
                    Ordering::Less => {
                        return Err(insufficient_layout(
                            col,
                            self.layout_indent_with_span(),
                            tok.span,
                        ));
                    }
                    Ordering::Equal | Ordering::Greater if !self.is_expr_cont(&tok) => {
                        let span = self.last_token().map_or(tok.span, |t| t.span);
                        self.push_semi(span.trailing());
                    }
                    _ => (),
                }
            }

            self.last_line = line;
            self.push(tok);
        }

        if let Some(tok) = self.last_token().copied() {
            while self.indents.pop().is_some() {
                self.push_close_curly(tok.span.trailing());
            }

            self.push_semi(tok.span.trailing());
        }

        Ok(())
    }

    fn is_expr_cont(&self, tok: &Token) -> bool {
        tok.kind.is_start_cont() || self.last_token().map_or(false, |t| t.kind.is_end_cont())
    }

    #[track_caller]
    fn layout_indent(&self) -> u32 {
        self.indents.last().map(|(i, _)| i).copied().unwrap_or(Self::DEFAULT_COL)
    }

    #[track_caller]
    fn layout_indent_with_span(&self) -> Indent {
        self.indents.last().copied().unwrap_or(self.default_indent())
    }

    #[inline]
    fn last_token(&self) -> Option<&Token> {
        self.tokens.last()
    }

    fn push_open_curly(&mut self, span: Span) {
        self.push(Token { kind: TokenKind::OpenCurly(true), span });
    }

    fn push_close_curly(&mut self, span: Span) {
        self.push(Token { kind: TokenKind::CloseCurly(true), span });
    }

    fn push_semi(&mut self, span: Span) {
        if self.last_token().map_or(false, |t| matches!(t.kind, TokenKind::Semi(_))) {
            return;
        }

        self.push(Token { kind: TokenKind::Semi(true), span });
    }

    #[inline]
    fn push(&mut self, tok: Token) {
        self.tokens.push(tok);
    }

    #[inline]
    fn default_indent(&self) -> Indent {
        (Self::DEFAULT_COL, Span::new(self.source.id(), 0, 0))
    }
}

type Indent = (u32, Span);

fn insufficient_layout(col: u32, (indent, layout_span): Indent, span: Span) -> Diagnostic {
    Diagnostic::error(
        "line must be indented the same as or more \
         than its enclosing layout",
    )
    .with_label(Label::primary(span, format!("too little indentation (column {col})")))
    .with_label(Label::secondary(layout_span, format!("enclosing layout on column {}", indent)))
}
