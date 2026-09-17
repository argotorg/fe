mod body;
mod effects;
mod local_facts;
mod pattern;
mod place;

pub use body::lower_to_smir;
pub(crate) use body::{BindingRoleMode, lower_to_smir_with_call_sites};
pub use effects::{effect_param_site, owner_effect_bindings, same_owner_effect_binding};
pub(crate) use local_facts::layout_backing_source_path_is_prefix;
pub(crate) use pattern::enum_tag_ty;
