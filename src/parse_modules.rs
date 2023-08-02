use crate::{ast::Module, diagnostics::Diagnostic, parser, span::Source, state::State, tokenize};

pub(crate) fn parse_modules(state: &mut State) -> Vec<Module> {
    let mut modules = vec![];

    let root_source = state.root_source();

    match parse_module(state, root_source) {
        Ok(module) => modules.push(module),
        Err(diag) => state.diagnostics.add(diag),
    }

    modules
}

fn parse_module(state: &State, source: &Source) -> Result<Module, Diagnostic> {
    let tokens = tokenize::tokenize(&state, source)?;
    let module = parser::parse(&state, source, tokens)?;
    Ok(module)
}
