pub mod access;
mod admission;
mod ir;
pub mod layout_plan;
mod normalize;
mod verify;

pub(crate) use admission::normalize_semantic_body_provisional;
pub use admission::{
    AdmittedSemanticBodyId, SemanticBodyAdmission, normalize_runtime_semantic_body,
    normalize_semantic_body, semantic_body_admission,
};
pub use ir::*;
pub use layout_plan::*;
pub use normalize::{NormalizeError, NormalizedArtifacts, normalize_raw_body};
pub(crate) use verify::normalized_cfg;
pub use verify::{NormalizedBodyVerifyError, verify_normalized_body};
