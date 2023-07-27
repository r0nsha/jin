use crate::{parser, span::SourceId, state::State, tokenize, CompilerResult};

use super::Module;

pub fn gen(state: &State, root_source_id: SourceId) -> CompilerResult<Vec<Module>> {
    // TODO: handle error

    let mut modules = vec![];

    let tokens = tokenize::tokenize(&state, root_source_id)?;
    let module = parser::parse(&state, tokens)?;

    modules.push(module);

    Ok(modules)
}
