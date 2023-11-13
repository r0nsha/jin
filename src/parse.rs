mod lexer;
pub mod parse_modules;
mod parser;
mod import;
mod errors;

pub use parse_modules::parse_module_tree;
