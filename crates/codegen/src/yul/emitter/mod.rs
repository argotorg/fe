use std::fmt;

use crate::yul::errors::YulError;

pub use module::{
    TestMetadata, TestModuleOutput, emit_module_yul, emit_module_yul_with_layout,
    emit_test_module_yul, emit_test_module_yul_with_layout,
};

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
