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
    indents: Vec<u32>,
    last_line: u32,
}

impl<'a> Layout<'a> {
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
                self.push_open_brace(tok.span);
            }

            if newline
                && col < self.layout_indent()
                && !matches!(tok.kind, TokenKind::OpenCurly | TokenKind::CloseCurly)
            {
                while col < self.layout_indent() {
                    self.push_semi(tok.span);
                    self.push_close_brace(tok.span);
                    self.indents.pop();
                }
            }

            // Layout stack indentation
            if self.last_token().map_or(false, |t| t.kind == TokenKind::OpenCurly) {
                if tok.kind != TokenKind::CloseCurly && col < self.layout_indent() {
                    return Err(Diagnostic::error(format!(
                        "line must be indented more than its \
                         enclosing layout (column {})",
                        self.layout_indent()
                    ))
                    .with_label(Label::primary(tok.span, "insufficient indentation")));
                }

                self.indents.push(col);
            }

            if tok.kind == TokenKind::CloseCurly {
                self.indents.pop();
            }

            // Semicolon insertion
            if newline && !self.tokens.is_empty() {
                match col.cmp(&self.layout_indent()) {
                    Ordering::Less => {
                        return Err(Diagnostic::error(format!(
                            "line must be indented the same as or more \
                             than its layout (column {})",
                            self.layout_indent()
                        ))
                        .with_label(Label::primary(tok.span, "invalid indentation")));
                    }
                    Ordering::Equal | Ordering::Greater if !self.is_expr_cont(&tok) => {
                        let span = self.last_token().map_or(tok.span, |t| t.span);
                        self.push_semi(span);
                    }
                    _ => (),
                }
            }

            self.last_line = line;
            self.push(tok);
        }

        if let Some(tok) = self.last_token().copied() {
            while self.indents.pop().is_some() {
                self.push_close_brace(tok.span);
            }

            self.push_semi(tok.span);
        }

        Ok(())
    }

    fn is_expr_cont(&self, tok: &Token) -> bool {
        tok.kind.is_start_cont() || self.last_token().map_or(false, |t| t.kind.is_end_cont())
    }

    #[track_caller]
    fn layout_indent(&self) -> u32 {
        self.indents.last().copied().unwrap_or(1)
    }

    #[inline]
    fn last_token(&self) -> Option<&Token> {
        self.tokens.last()
    }

    fn push_open_brace(&mut self, span: Span) {
        self.push(Token { kind: TokenKind::OpenCurly, span });
    }

    fn push_close_brace(&mut self, span: Span) {
        self.push(Token { kind: TokenKind::CloseCurly, span });
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
}
