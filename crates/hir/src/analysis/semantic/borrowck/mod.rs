mod boundary;
mod callsite;
mod check;
mod control;
mod diagnostics;
mod events;
mod facts;
mod inventory;
mod ir;
mod memory;
mod solver;
mod summary;

pub use boundary::check_semantic_boundaries;
pub(crate) use callsite::provisional_call_site_provider_refinements;
pub use check::{
    SemanticAnalysisError, SemanticBorrowAnalysisPass, check_semantic_borrows,
    collect_semantic_borrow_diagnostic_vouchers, semantic_borrow_summary,
};
pub(crate) use diagnostics::{
    checker_name, normalized_body_error_to_diag, normalized_body_verify_error_to_diag,
    normalized_layout_plan_verify_error_to_diag, resolve_local_source_span,
    smir_lowering_admission_diag, span_for_origin_from_body,
};
pub use facts::*;
pub use ir::*;
