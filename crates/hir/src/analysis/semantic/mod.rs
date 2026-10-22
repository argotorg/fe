pub mod access;
pub mod capability;
pub mod consts;
pub mod ctfe;
pub mod definite_assignment;
pub mod diagnostics;
pub mod instance;
pub mod ir;
pub mod lower;
pub mod normalized;
mod verify;

pub use access::*;
pub use consts::*;
pub use ctfe::*;
pub use definite_assignment::contract_init_assigned_fields;
pub use diagnostics::{
    BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
    SemanticDiagnosticLabel, SemanticDiagnosticSpan, SemanticNormalizationFailure,
};
pub(crate) use instance::{CallSiteProviderRefinement, RefinedInput};
pub use instance::{
    EffectProviderSubst, FinalizedGeneratedCall, GenericSubst, ImplEnv, InstantiatedEffectEnv,
    RootSemanticInstanceError, SemanticEffectEnvInstantiationError, SemanticInstance,
    SemanticInstanceKey, TypedBodyTemplate, core_method_callee_key, generated_callee,
    generated_callee_key, identity_semantic_instance_key, instantiate_typed_body,
    instantiated_effect_env, resolved_provider_binding_for_instance_effect,
    root_semantic_instance_key, typed_body_template, validate_instantiated_effect_env_key,
};
pub(crate) use instance::{
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    semantic_instance_base_assumptions_for_key,
};
pub use ir::*;
pub use lower::{
    effect_param_site, lower_to_smir, owner_effect_bindings, same_owner_effect_binding,
};
pub use normalized::*;
pub use verify::{SemanticVerifyError, verify_semantic_body};
