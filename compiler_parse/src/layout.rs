use codespan_reporting::files::Location;
use compiler_core::diagnostics::DiagnosticResult;
use compiler_core::span::Source;

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
        Self { source, indents: vec![], line: 0 }
    }

    fn apply(mut self, input: Vec<Token>) -> DiagnosticResult<Vec<Token>> {
        let mut new_tokens = Vec::with_capacity(input.len() * 2);

        // TODO: brace insertion
        // TODO: semicolon insertion
        // TODO: insert semicolon before any curly
        // TODO: insert semicolon after all tokens
        for (i, tok) in input.iter().enumerate() {
            let Location { line_number, column_number } = self.source.span_location(tok.span);
            let is_first_tok = i == 0;
            let is_first_line_tok = line_number>self.line;
                let is_expr_cont= is_first_line_tok && !tok.kind.is_after_semi() ||

            // Layout indent
            if is_first_tok || input[i - 1].kind == TokenKind::OpenCurly {
                if tok.kind != TokenKind::CloseCurly && column_number <= self.curr_indent() {
                    todo!("column_number must be larger than curr_indent")
                }

                self.indent(column_number);
            }

            // Layout dedent
            if tok.kind == TokenKind::CloseCurly {
                self.dedent();
            }

            // Open curly insertion
            if is_first_line_tok && column_number > self.curr_indent() {
                if  !is_expr_cont {
                    new_tokens.push(Token { kind: TokenKind::OpenCurly, span: tok.span.tail() });
                }
            }

            // Close curly insertion

            self.line = line_number;
            new_tokens.push(*tok);
        }

        Ok(new_tokens)
    }

    fn curr_indent(&self) -> usize {
        self.indents.last().copied().unwrap_or_default()
    }

    fn indent(&mut self, col: usize) {
        self.indents.push(col);
    }

    fn dedent(&mut self) {
        self.indents.pop();
    }
}
