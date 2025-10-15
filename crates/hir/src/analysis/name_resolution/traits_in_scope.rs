use crate::hir_def::{Body, ExprId, ItemKind, Mod, TopLevelMod, Trait, scope_graph::ScopeId};
use common::indexmap::IndexSet;

use crate::analysis::{HirAnalysisDb, name_resolution::resolve_imports};

/// Returns the all traits that are available in the given scope.
pub fn available_traits_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> &'db IndexSet<Trait<'db>> {
    let scope_kind = TraitScopeKind::from_scope(db, scope);
    let trait_scope = TraitScope::new(db, scope_kind);
    available_traits_in_scope_impl(db, trait_scope)
}

#[salsa::tracked(return_ref)]
pub(crate) fn available_traits_in_scope_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    t_scope: TraitScope<'db>,
) -> IndexSet<Trait<'db>> {
    let mut traits = IndexSet::default();
    let scope = t_scope.inner(db).to_scope();

    let imports = &resolve_imports(db, scope.ingot(db)).1;
    if let Some(named) = imports.named_resolved.get(&scope) {
        named
            .values()
            .flat_map(|bucket| bucket.iter_ok())
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    if let Some(glob) = imports.glob_resolved.get(&scope) {
        glob.iter()
            .flat_map(|(_, map)| map.values().flat_map(|v| v.iter()))
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    if let Some(unnamed) = imports.unnamed_resolved.get(&scope) {
        unnamed
            .iter()
            .flat_map(|bucket| bucket.iter_ok())
            .for_each(|name_res| {
                if let Some(trait_) = name_res.trait_() {
                    traits.insert(trait_);
                }
            })
    }

    for child in scope.child_items(db) {
        if let ItemKind::Trait(trait_) = child {
            traits.insert(trait_);
        }
    }

    if let Some(parent) = scope.parent(db) {
        let parent_traits = available_traits_in_scope(db, parent);
        traits.extend(parent_traits.iter().copied());
    }

    traits
}

#[salsa::interned]
#[derive(Debug)]
pub struct TraitScope<'db> {
    inner: TraitScopeKind<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitScopeKind<'db> {
    TopLevelMod(TopLevelMod<'db>),
    Module(Mod<'db>),
    Block(Body<'db>, ExprId),
}

impl<'db> TraitScopeKind<'db> {
    fn from_scope(db: &'db dyn HirAnalysisDb, mut scope: ScopeId<'db>) -> Self {
        loop {
            match scope {
                ScopeId::Item(item) => match item {
                    ItemKind::TopMod(top_level_mod) => {
                        return TraitScopeKind::TopLevelMod(top_level_mod);
                    }
                    ItemKind::Mod(mod_) => {
                        return TraitScopeKind::Module(mod_);
                    }
                    _ => {}
                },
                ScopeId::Block(body, expr_id) => {
                    return TraitScopeKind::Block(body, expr_id);
                }
                _ => {}
            }
            scope = scope.parent(db).unwrap();
        }
    }

    fn to_scope(&self) -> ScopeId<'db> {
        match self {
            TraitScopeKind::TopLevelMod(top_level_mod) => {
                ScopeId::Item(ItemKind::TopMod(*top_level_mod))
            }
            TraitScopeKind::Module(mod_) => ScopeId::Item(ItemKind::Mod(*mod_)),
            TraitScopeKind::Block(body, expr_id) => ScopeId::Block(*body, *expr_id),
        }
    }
}
