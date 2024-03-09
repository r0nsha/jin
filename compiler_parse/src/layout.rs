use codespan_reporting::files::Location;
use compiler_core::diagnostics::{Diagnostic, DiagnosticResult, Label};
use compiler_core::span::{Source, Span};

use crate::token::{Token, TokenKind};

pub(crate) fn apply(source: &Source, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
    Layout::new(source).apply(input)
}

struct Layout<'a> {
    source: &'a Source,
    indents: Vec<usize>,
    line: usize,
}

impl<'a> Layout<'a> {
    fn new(source: &'a Source) -> Self {
        Self { source, indents: vec![1], line: 0 }
    }

    fn apply(mut self, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
        if input.is_empty() {
            return Ok(input);
        }

        let mut new_tokens = Vec::with_capacity(input.len() * 2);
        let mut is_curly_balanced = true;

        for (i, tok) in input.iter().enumerate() {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let last_tok = if i > 0 { input.get(i - 1) } else { None };
            let is_first_tok = i == 0;
            let is_first_line_tok = is_first_tok || line_number > self.line;

            let layout_indent = self.layout_indent();

            // Brace insertion
            if is_first_line_tok {
                let is_expr_cont = is_first_line_tok
                    && (tok.kind.is_start_cont()
                        || last_tok.map_or(false, |t| t.kind.is_end_cont()));

                // Open curly insertion
                if column_number > layout_indent && !is_expr_cont {
                    is_curly_balanced = false;
                    push_open_curly(&mut new_tokens, tok.span);
                }

                // Close curly insertion
                if column_number < layout_indent && tok.kind != TokenKind::CloseCurly {
                    is_curly_balanced = true;
                    push_close_curly(&mut new_tokens, tok.span);
                }
            }

            // Layout indent
            if is_first_tok || last_tok.map_or(false, |t| t.kind == TokenKind::OpenCurly) {
                if tok.kind != TokenKind::CloseCurly && column_number < layout_indent {
                    return Err(Diagnostic::error(format!(
                        "line must be indented more then its\
                                enclosing layout (column {layout_indent})"
                    ))
                    .with_label(Label::primary(tok.span, "insufficient indentation")));
                }

                self.indent(column_number);
            }

            // Layout dedent
            if tok.kind == TokenKind::CloseCurly {
                self.dedent();
            }

            // TODO: semicolon insertion
            // TODO: insert semicolon before any curly
            // TODO: insert semicolon after all tokens

            self.line = line_number;
            new_tokens.push(*tok);
        }

        if !is_curly_balanced {
            let span = new_tokens.last().unwrap().span;
            push_close_curly(&mut new_tokens, span);
        }

        Ok(new_tokens)
    }

    #[track_caller]
    fn layout_indent(&self) -> usize {
        self.indents.last().copied().unwrap()
    }

    fn indent(&mut self, col: usize) {
        self.indents.push(col);
    }

    fn dedent(&mut self) {
        self.indents.pop();
    }
}

fn push_open_curly(tokens: &mut Vec<Token>, span: Span) {
    tokens.push(Token { kind: TokenKind::OpenCurly, span: span.tail() });
}

fn push_close_curly(tokens: &mut Vec<Token>, span: Span) {
    tokens.push(Token { kind: TokenKind::CloseCurly, span: span.head() });
}
