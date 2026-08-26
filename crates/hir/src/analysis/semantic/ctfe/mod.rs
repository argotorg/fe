mod canonicalize;
mod machine;

pub use canonicalize::canonicalize_semantic_consts;
pub(crate) use canonicalize::canonicalize_semantic_consts_for_admission;
pub use machine::{
    CtfeConfig, CtfeError, eval_body_owner_const, eval_body_owner_const_with_args,
    eval_const_instance, eval_const_ref,
};
