mod const_ref;
mod semantic;
mod template;

pub use const_ref::{
    FinalizedGeneratedCall, core_method_callee_key, generated_callee, generated_callee_key,
};
pub(crate) use const_ref::{
    provisional_semantic_callee_key, resolve_semantic_const_ref,
    semantic_callee_key_with_effect_providers,
};
pub use semantic::{
    CallSiteLowering, ForLoopCallSites, InstantiatedEffectEnv, RootSemanticInstanceError,
    SemanticEffectEnvInstantiationError, SemanticInstance, SemanticInstanceKey,
    identity_semantic_instance_key, instantiated_effect_env,
    resolved_provider_binding_for_instance_effect, root_semantic_instance_key,
    validate_instantiated_effect_env_key,
};
pub(crate) use semantic::{
    CallSiteProviderRefinement, LayoutDemandBody, RefinedInput, SemanticBodyAdmissionError,
};
pub(crate) use semantic::{
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    row_expansion_for_key, semantic_instance_base_assumptions_for_key,
};
pub use template::{
    EffectProviderSubst, GenericSubst, ImplEnv, TypedBodyTemplate, instantiate_typed_body,
    typed_body_template,
};
