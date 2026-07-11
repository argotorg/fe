mod analyses;
mod callsite;
mod canon;
mod check;
mod diagnostics;
mod facts;
mod ir;
mod noesc;
mod normalize;
mod verify;

pub(crate) use callsite::provisional_call_site_provider_refinements;
pub use check::{
    SemanticBorrowAnalysisPass, check_semantic_borrows,
    collect_semantic_borrow_diagnostic_vouchers, semantic_borrow_summary,
};
pub(crate) use diagnostics::{checker_name, resolve_local_source_span, span_for_origin_from_body};
pub use facts::*;
pub use ir::*;
pub use noesc::{check_semantic_noesc, check_semantic_noesc_voucher};
pub use normalize::{normalize_semantic_body, normalize_semantic_body_for_layout_evidence};
pub use verify::verify_normalized_semantic_body;
