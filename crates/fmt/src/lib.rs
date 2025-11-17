//! Core formatting infrastructure for the Fe language.

mod ast;
mod config;
mod context;
mod format;
mod rewrite;
mod shape;
#[cfg(test)]
mod tests;

pub use crate::config::Config;
pub use crate::context::RewriteContext;
pub use crate::format::{FormatError, format_file, format_str};
pub use crate::rewrite::Rewrite;
pub use crate::shape::{Indent, Shape};
