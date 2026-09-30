mod canonicalize;
mod machine;
mod outcome;
mod primitive;
mod request;
mod service;

pub use canonicalize::canonicalize_semantic_consts;
pub(crate) use canonicalize::{
    canonicalize_semantic_const_refs, canonicalize_semantic_consts_for_runtime,
};
pub use machine::{
    CtfeConfig, CtfeError, eval_body_owner_const, eval_body_owner_const_with_args,
    eval_const_instance, eval_const_ref,
};
pub use outcome::{
    BlockedInfo, ConstDemandKind, ConstDependency, EvalFailure, EvalOutcome, FoldAttempt,
    FoldMissReason,
};
pub(crate) use primitive::{
    PrimitiveFault, execute_scalar_cast, execute_source_int_binary, execute_source_int_unary,
    int_in_range,
};
pub use request::{
    ConstComputationId, ConstDesc, ConstEntry, ConstRepr, ConstUsePolicy, VerifiedConstValueId,
};
pub use service::{
    const_computation_for_instance, describe_const_computation, force_const_computation,
    force_const_description, specialize_const_computation, specialize_const_description,
};

pub(crate) use service::force_const_term_value;
