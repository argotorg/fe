use common::ingot::IngotKind;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{NameDomain, PathRes, resolve_ident_to_bucket, resolve_path},
        ty::trait_resolution::PredicateListId,
    },
    hir_def::{IdentId, PathId, Trait, scope_graph::ScopeId},
};

/// Resolve a trait in the core library by an explicit trait path, excluding the "core" root segment.
pub fn resolve_core_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    segments: &[&str],
) -> Trait<'db> {
    let ingot = scope.top_mod(db).ingot(db);
    let root = if ingot.kind(db) == IngotKind::Core {
        IdentId::make_ingot(db)
    } else {
        IdentId::make_core(db)
    };

    // Build the module path (all segments except the last)
    let (module_segments, trait_name) = segments.split_at(segments.len() - 1);
    let trait_name = IdentId::new(db, trait_name[0].to_string());

    let mut module_path = PathId::from_ident(db, root);
    for seg in module_segments {
        module_path = module_path.push_ident(db, IdentId::new(db, seg.to_string()));
    }

    // Resolve the module path
    let assumptions = PredicateListId::empty_list(db);
    let Ok(PathRes::Mod(mod_scope)) = resolve_path(db, module_path, scope, assumptions, false)
    else {
        panic!("failed to resolve core trait module path: {segments:?}");
    };

    // Resolve the trait name within the module
    let bucket = resolve_ident_to_bucket(db, PathId::from_ident(db, trait_name), mod_scope);
    let nameres = bucket
        .pick(NameDomain::TYPE)
        .as_ref()
        .expect("failed to resolve core trait");
    nameres.trait_().expect("resolved name is not a trait")
}
