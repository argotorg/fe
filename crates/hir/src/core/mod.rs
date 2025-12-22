//! Core module grouping HIR definition, lowering, spans, and visitor.
//!
//! This is the new physical home for non-analysis modules. The crate root
//! re-exports these as `crate::hir_def`, `crate::lower`, `crate::span`, and
//! `crate::visitor` for backwards compatibility.

pub mod adt_lower;
pub mod hir_def;
pub mod lower;
pub mod print;
pub mod semantic;
pub mod span;
pub mod visitor;
