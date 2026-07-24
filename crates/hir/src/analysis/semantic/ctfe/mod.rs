mod canonicalize;
mod machine;

pub use canonicalize::canonicalize_semantic_consts;
pub(crate) use canonicalize::{
    canonicalize_provisional_semantic_consts_from_body, canonicalize_semantic_const_refs_from_body,
};
pub use machine::{
    CtfeConfig, CtfeError, eval_body_owner_const, eval_body_owner_const_with_args,
    eval_const_instance, eval_const_ref,
};
