//! Fe to Yul compiler.

use crate::errors::CompileError;
use fe_parser as parser;

mod mappers;
mod namespace;
mod runtime;

/// Compiles Fe to Yul.
pub fn compile(src: &str) -> Result<String, CompileError> {
    let tokens = parser::get_parse_tokens(src)?;
    let vyp_module = parser::parsers::file_input(&tokens[..])?.1.node;

    // TODO: Handle multiple contracts in one Fe module cleanly.
    if let Some(first_contract) = mappers::module::module(&vyp_module)?.values().next() {
        return Ok(first_contract.to_string());
    }

    Err(CompileError::static_str("unable to parse tokens."))
}
