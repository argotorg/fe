pub(crate) use const_ref::opened_operand_application as known_opening_application;
mod const_ref;
mod semantic;
mod template;

pub use const_ref::generated_callee_key;
pub(crate) use const_ref::{
    checked_operand_callee, declaration_reaches_trait_goal, has_unbound_evidence_calls,
    known_operand_variant, opened_assoc_const_ref, provisional_semantic_callee_key,
    recover_known_operand, resolve_semantic_const_ref, resolve_semantic_const_ref_in_instance,
    semantic_callee_key_with_effect_providers, semantic_const_key_for_assoc_const,
};
pub use semantic::{
    CallSiteLowering, ForLoopCallSites, InstantiatedEffectEnv, ReceiverLoweringPlan,
    RootSemanticInstanceError, SemanticEffectEnvInstantiationError, SemanticInstance,
    SemanticInstanceKey, get_or_build_semantic_instance, identity_semantic_instance_key,
    instantiated_effect_env, resolved_provider_binding_for_instance_effect,
    root_semantic_instance_key, semantic_layout_bundle_signature,
    validate_instantiated_effect_env_key,
};
pub(crate) use semantic::{CallSiteProviderRefinement, SemanticBodyAdmissionError};
pub(crate) use semantic::{
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    resolved_effect_binding_ty_for_instance_effect, semantic_instance_base_assumptions_for_key,
};
pub use template::{
    EffectProviderSubst, GenericSubst, ImplEnv, TypedBodyTemplate, instantiate_typed_body,
    typed_body_template,
};
