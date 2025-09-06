use crate::{
    name_resolution::{method_selection::MethodCandidate, PathRes},
    ty::func_def::FuncDef,
};

/// Extract the underlying function definition for a resolved method PathRes.
/// Returns None if the PathRes is not a method.
pub fn method_func_def_from_res<'db>(
    res: &crate::name_resolution::PathRes<'db>,
) -> Option<FuncDef<'db>> {
    match res {
        PathRes::Method(_, cand) => match cand {
            MethodCandidate::InherentMethod(fd) => Some(*fd),
            MethodCandidate::TraitMethod(tm) | MethodCandidate::NeedsConfirmation(tm) => {
                Some(tm.method.0)
            }
        },
        _ => None,
    }
}
