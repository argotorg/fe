use crate::core::hir_def::{ItemKind, scope_graph::ScopeId};
use common::indexmap::IndexMap;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind, resolve_imports,
    },
};

/// Returns all items visible in the given scope for the specified domain(s).
///
/// This includes:
/// - Items from named and glob imports; an import named `_` brings in no
///   name
/// - Direct child items in the scope
/// - Items from parent scopes (recursively)
pub fn items_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    domain: NameDomain,
) -> &'db IndexMap<String, NameRes<'db>> {
    items_in_scope_impl(db, ItemScope::new(db, item_scope(db, scope), domain))
}

/// Returns the innermost module or block scope containing `scope`, which
/// holds the items and imports visible in `scope`.
pub(super) fn item_scope<'db>(db: &'db dyn HirAnalysisDb, mut scope: ScopeId<'db>) -> ScopeId<'db> {
    while !matches!(
        scope,
        ScopeId::Item(ItemKind::TopMod(_) | ItemKind::Mod(_)) | ScopeId::Block(..)
    ) {
        scope = scope.parent(db).unwrap();
    }
    scope
}

#[salsa::tracked]
fn items_in_scope_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    i_scope: ItemScope<'db>,
) -> IndexMap<String, NameRes<'db>> {
    // Use NameResBucket to properly handle precedence:
    // Def > NamedImported > GlobImported > Lex > External > Prim
    let mut buckets: IndexMap<String, NameResBucket<'db>> = IndexMap::default();
    let scope = i_scope.scope(db);
    let domain = i_scope.domain(db);

    let imports = &resolve_imports(db, scope.ingot(db)).1;

    // Collect direct child items first (highest precedence: Def)
    for child in scope.child_items(db) {
        // A named implementation's name is in its own domain.
        let child_domain = if matches!(child, ItemKind::ImplTrait(_)) {
            NameDomain::IMPL
        } else {
            NameDomain::from_scope(db, ScopeId::from_item(child))
        };
        if child_domain & domain != NameDomain::Invalid
            && let Some(name) = child.name(db)
        {
            let name_res = NameRes {
                kind: NameResKind::Scope(ScopeId::from_item(child)),
                domain: child_domain,
                derivation: NameDerivation::Def,
            };
            buckets
                .entry(name.data(db).to_string())
                .or_default()
                .merge(db, &name_res.into());
        }
    }

    // Collect named imports (second highest precedence: NamedImported)
    if let Some(named) = imports.named_resolved.get(&scope) {
        for (name, bucket) in named {
            for name_res in bucket.iter_ok() {
                if name_res.domain & domain != NameDomain::Invalid {
                    buckets
                        .entry(name.data(db).to_string())
                        .or_default()
                        .merge(db, &name_res.clone().into());
                }
            }
        }
    }

    // Collect glob imports (third precedence: GlobImported)
    if let Some(glob) = imports.glob_resolved.get(&scope) {
        for (_, map) in glob.iter() {
            for (name, resolutions) in map {
                for name_res in resolutions {
                    if name_res.domain & domain != NameDomain::Invalid {
                        buckets
                            .entry(name.data(db).to_string())
                            .or_default()
                            .merge(db, &name_res.clone().into());
                    }
                }
            }
        }
    }

    // Recursively collect from parent scope (lower precedence: Lex)
    if let Some(parent) = scope.parent(db) {
        let parent_items = items_in_scope(db, parent, domain);
        for (name, name_res) in parent_items {
            // Parent scope items get Lex derivation which is lower precedence
            let mut lexed_res = name_res.clone();
            lexed_res.derivation = NameDerivation::Lex(Box::new(name_res.derivation.clone()));
            buckets
                .entry(name.clone())
                .or_default()
                .merge(db, &lexed_res.into());
        }
    }

    // Convert buckets to final result, picking the best resolution per domain
    buckets
        .into_iter()
        .filter_map(|(name, bucket)| {
            bucket
                .pick(domain)
                .as_ref()
                .ok()
                .map(|res| (name, res.clone()))
        })
        .collect()
}

#[salsa::interned]
#[derive(Debug)]
pub struct ItemScope<'db> {
    #[returns(copy)]
    scope: ScopeId<'db>,
    #[returns(copy)]
    domain: NameDomain,
}
