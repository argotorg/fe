use std::fmt;

use crate::yul::errors::YulError;

pub use module::emit_module_yul;

mod control_flow;
mod expr;
mod function;
mod module;
mod statements;
mod util;

#[derive(Debug)]
pub enum EmitModuleError {
    MirLower(mir::MirLowerError),
    Yul(YulError),
}

impl fmt::Display for EmitModuleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EmitModuleError::MirLower(err) => write!(f, "{err}"),
            EmitModuleError::Yul(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for EmitModuleError {}
