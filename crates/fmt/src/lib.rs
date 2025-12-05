//! Core formatting infrastructure for the Fe language.

mod ast;
mod config;
mod context;
mod format;
#[cfg(test)]
mod tests;

pub use crate::config::{Config, TrailingComma};
pub use crate::context::RewriteContext;
pub use crate::format::{FormatError, format_file, format_str};
