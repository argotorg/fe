mod analyses;
mod callsite;
mod canon;
mod check;
mod definite_init;
mod diagnostics;
mod facts;
mod ir;
mod noesc;
mod normalize;
mod verify;

pub(crate) use callsite::provisional_call_site_provider_refinements;
pub use check::{
    SemanticBorrowAnalysisPass, check_semantic_borrows,
    collect_semantic_borrow_diagnostic_vouchers, normalized_cfg_reachable_blocks,
    normalized_cfg_successors, normalized_cfg_successors_and_reachable, semantic_borrow_summary,
};
pub(crate) use check::{cfg_reachable_blocks, normalized_cfg_successor_indices};
pub(crate) use diagnostics::{checker_name, resolve_local_source_span, span_for_origin_from_body};
pub use facts::*;
pub use ir::*;
pub use noesc::{check_semantic_noesc, check_semantic_noesc_voucher};
pub(crate) use normalize::{
    normalize_provisional_semantic_body_for_never_return_analysis,
    normalize_semantic_body_for_analysis,
};
pub use normalize::{normalize_semantic_body, normalize_semantic_body_for_layout_evidence};
pub use verify::verify_normalized_semantic_body;
