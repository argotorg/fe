//! Core formatting infrastructure for the Fe language.

mod ast;
mod config;
mod context;
mod format;
mod list;
mod rewrite;
mod shape;
#[cfg(test)]
mod tests;

pub use crate::config::Config;
pub use crate::context::RewriteContext;
pub use crate::format::{FormatError, format_file, format_str};
pub use crate::list::{ListFormatting, ListTactic, format_list};
pub use crate::rewrite::{Rewrite, RewriteExt};
pub use crate::shape::{Indent, Shape};
