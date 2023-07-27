use crate::{parser, state::State, tokenize, CompilerResult};

use super::Module;

pub fn gen(state: &State) -> CompilerResult<Vec<Module>> {
    let mut modules = vec![];

    let root_source = state.root_source();
    let tokens = tokenize::tokenize(&state, root_source)?;
    let module = parser::parse(&state, root_source, tokens)?;

    modules.push(module);

    Ok(modules)
}
