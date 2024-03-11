use std::cmp::Ordering;

use codespan_reporting::files::Location;
use compiler_core::diagnostics::{Diagnostic, DiagnosticResult, Label};
use compiler_core::span::{Source, Span};

use crate::token::{Token, TokenKind};

pub(crate) fn apply(source: &Source, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
    Layout::new(source, input.capacity()).apply(input)
}

struct Layout<'a> {
    source: &'a Source,
    tokens: Vec<Token>,
    indents: Vec<u32>,
}

impl<'a> Layout<'a> {
    fn new(source: &'a Source, capacity: usize) -> Self {
        Self { source, tokens: Vec::with_capacity(capacity), indents: vec![] }
    }

    fn apply(mut self, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
        if input.is_empty() {
            return Ok(input);
        }

        let mut last_line = 1;

        for tok in input {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let (line, col) = (line_number as u32, column_number as u32);
            let is_new_line = line > last_line;

            let mut curr_tok = tok;

            // Brace insertion
            if is_new_line {
                if col > self.layout_indent() && !self.is_expr_cont(&tok) {
                    let t = Token { kind: TokenKind::OpenCurly, span: tok.span };
                    curr_tok = t;
                    self.tokens.push(t);
                }

                if col < self.layout_indent()
                    && !matches!(tok.kind, TokenKind::OpenCurly | TokenKind::CloseCurly)
                {
                    self.push_semi(tok.span);
                    let t = Token { kind: TokenKind::CloseCurly, span: tok.span };
                    curr_tok = t;
                    self.tokens.push(t);
                }
            }

            // Layout stack indentation
            if self.last_token().map_or(false, |t| t.kind == TokenKind::OpenCurly) {
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
                match col.cmp(&self.layout_indent()) {
                    Ordering::Less => {
                        return Err(Diagnostic::error(format!(
                            "line must be indented the same as or more \
                             than its layout (column {})",
                            self.layout_indent()
                        ))
                        .with_label(Label::primary(tok.span, "invalid indentation")));
                    }
                    Ordering::Equal if !self.is_expr_cont(&tok) => {
                        let span = self.last_token().map_or(tok.span, |t| t.span);
                        self.push_semi(span);
                    }
                    _ => (),
                }
            }

            last_line = line;
            self.tokens.push(tok);
        }

        if let Some(span) = self.last_token().map(|t| t.span) {
            while self.indents.pop().is_some() {
                self.push_close_brace(span);
            }

            self.push_semi(span);
        }

        Ok(self.tokens)
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
        self.tokens.push(Token { kind: TokenKind::OpenCurly, span });
    }

    fn push_close_brace(&mut self, span: Span) {
        self.tokens.push(Token { kind: TokenKind::CloseCurly, span });
    }

    fn push_semi(&mut self, span: Span) {
        if self.last_token().map_or(false, |t| matches!(t.kind, TokenKind::Semi(_))) {
            return;
        }

        self.tokens.push(Token { kind: TokenKind::Semi(true), span });
    }
}
