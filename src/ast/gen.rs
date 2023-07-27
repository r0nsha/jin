use crate::{parser, span::SourceId, state::State, tokenize, CompilerResult};

use super::Module;

pub fn gen(state: &State) -> CompilerResult<Vec<Module>> {
    // TODO: handle error

    let mut modules = vec![];

    let tokens = tokenize::tokenize(&state, state.root_source_id)?;
    let module = parser::parse(&state, state.root_source_id, tokens)?;

    modules.push(module);

    Ok(modules)
}
