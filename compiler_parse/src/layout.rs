use compiler_core::span::Source;

use crate::token::Token;

pub(crate) fn apply(source: &Source, input: Vec<Token>) -> Vec<Token> {
    let mut new_tokens = Vec::with_capacity(input.len() * 2);
    let mut indents: Vec<u32> = vec![0];

    for (i, tok) in input.into_iter().enumerate() {
        // if i==0 ||
        new_tokens.push(tok);
    }

    new_tokens
}
