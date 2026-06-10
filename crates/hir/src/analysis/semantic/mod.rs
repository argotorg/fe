pub mod borrowck;
pub mod consts;
pub mod ctfe;
pub mod definite_assignment;
pub mod instance;
pub mod ir;
pub mod lower;
mod verify;

pub use borrowck::*;
pub use consts::*;
pub use ctfe::*;
pub use definite_assignment::contract_init_assigned_fields;
pub(crate) use instance::CallSiteProviderRefinement;
pub use instance::{
    EffectProviderSubst, GenericSubst, ImplEnv, InstantiatedEffectEnv, RootSemanticInstanceError,
    SemanticEffectEnvInstantiationError, SemanticInstance, SemanticInstanceKey, TypedBodyTemplate,
    get_or_build_semantic_instance, identity_semantic_instance_key, instantiate_typed_body,
    instantiate_with_generic_args, instantiated_effect_env,
    resolved_provider_binding_for_instance_effect, root_semantic_instance_key, typed_body_template,
    validate_instantiated_effect_env_key,
};
pub(crate) use instance::{
    provisional_provider_binding_for_instance_effect, provisional_provider_idx_for_requirement,
    semantic_instance_base_assumptions_for_key,
};
pub use ir::*;
pub use lower::{
    effect_param_site, lower_to_smir, owner_effect_bindings, same_owner_effect_binding,
};
pub use verify::{SemanticVerifyError, verify_semantic_body};
