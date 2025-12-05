use crate::core::hir_def::{Body, ExprId, ItemKind, Mod, TopLevelMod, scope_graph::ScopeId};
use common::indexmap::IndexMap;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{NameDerivation, NameDomain, NameRes, NameResKind, resolve_imports},
};

/// Returns all items visible in the given scope for the specified domain(s).
///
/// This includes:
/// - Items from imports (named, glob, and unnamed/prelude)
/// - Direct child items in the scope
/// - Items from parent scopes (recursively)
pub fn items_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    domain: NameDomain,
) -> &'db IndexMap<String, NameRes<'db>> {
    let scope_kind = ItemScopeKind::from_scope(db, scope);
    let item_scope = ItemScope::new(db, scope_kind, domain);
    items_in_scope_impl(db, item_scope)
}

#[salsa::tracked(return_ref)]
fn items_in_scope_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    i_scope: ItemScope<'db>,
) -> IndexMap<String, NameRes<'db>> {
    let mut items = IndexMap::default();
    let scope = i_scope.scope_kind(db).to_scope();
    let domain = i_scope.domain(db);

    let imports = &resolve_imports(db, scope.ingot(db)).1;

    // Collect named imports
    if let Some(named) = imports.named_resolved.get(&scope) {
        for (name, bucket) in named {
            for name_res in bucket.iter_ok() {
                if name_res.domain & domain != NameDomain::Invalid {
                    items.insert(name.data(db).to_string(), name_res.clone());
                }
            }
        }
    }

    // Collect glob imports
    if let Some(glob) = imports.glob_resolved.get(&scope) {
        for (_, map) in glob.iter() {
            for (name, resolutions) in map {
                for name_res in resolutions {
                    if name_res.domain & domain != NameDomain::Invalid {
                        items.insert(name.data(db).to_string(), name_res.clone());
                    }
                }
            }
        }
    }

    // Collect unnamed/prelude imports
    if let Some(unnamed) = imports.unnamed_resolved.get(&scope) {
        for bucket in unnamed {
            for name_res in bucket.iter_ok() {
                if name_res.domain & domain != NameDomain::Invalid
                    && let Some(scope) = name_res.scope()
                    && let Some(name) = scope.name(db)
                {
                    items.insert(name.data(db).to_string(), name_res.clone());
                }
            }
        }
    }

    // Collect direct child items
    for child in scope.child_items(db) {
        let child_domain = NameDomain::from_scope(db, ScopeId::from_item(child));
        if child_domain & domain != NameDomain::Invalid
            && let Some(name) = child.name(db)
        {
            // Create a NameRes for this child item
            let name_res = NameRes {
                kind: NameResKind::Scope(ScopeId::from_item(child)),
                domain: child_domain,
                derivation: NameDerivation::Def,
            };
            items.insert(name.data(db).to_string(), name_res);
        }
    }

    // Recursively collect from parent scope
    if let Some(parent) = scope.parent(db) {
        let parent_items = items_in_scope(db, parent, domain);
        for (name, name_res) in parent_items {
            items
                .entry(name.clone())
                .or_insert_with(|| name_res.clone());
        }
    }

    items
}

#[salsa::interned]
#[derive(Debug)]
pub struct ItemScope<'db> {
    scope_kind: ItemScopeKind<'db>,
    domain: NameDomain,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemScopeKind<'db> {
    TopLevelMod(TopLevelMod<'db>),
    Module(Mod<'db>),
    Block(Body<'db>, ExprId),
}

impl<'db> ItemScopeKind<'db> {
    fn from_scope(db: &'db dyn HirAnalysisDb, mut scope: ScopeId<'db>) -> Self {
        loop {
            match scope {
                ScopeId::Item(item) => match item {
                    ItemKind::TopMod(top_level_mod) => {
                        return ItemScopeKind::TopLevelMod(top_level_mod);
                    }
                    ItemKind::Mod(mod_) => {
                        return ItemScopeKind::Module(mod_);
                    }
                    _ => {}
                },
                ScopeId::Block(body, expr_id) => {
                    return ItemScopeKind::Block(body, expr_id);
                }
                _ => {}
            }
            scope = scope.parent(db).unwrap();
        }
    }

    fn to_scope(&self) -> ScopeId<'db> {
        match self {
            ItemScopeKind::TopLevelMod(top_level_mod) => {
                ScopeId::Item(ItemKind::TopMod(*top_level_mod))
            }
            ItemScopeKind::Module(mod_) => ScopeId::Item(ItemKind::Mod(*mod_)),
            ItemScopeKind::Block(body, expr_id) => ScopeId::Block(*body, *expr_id),
        }
    }
}
