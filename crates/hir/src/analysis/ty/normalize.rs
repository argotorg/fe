//! Type normalization module
//!
//! This module provides functionality to normalize types by resolving associated types
//! to concrete types when possible. This happens before type unification to ensure
//! that types are in their most resolved form.

use std::collections::hash_map::Entry;

use crate::core::hir_def::{ImplTrait, scope_graph::ScopeId};
use common::indexmap::IndexMap;
use rustc_hash::FxHashMap;

use super::{
    canonical::Canonical,
    fold::{TyFoldable, TyFolder},
    trait_def::impls_for_ty_with_constraints,
    trait_resolution::PredicateListId,
    ty_def::{AssocTy, TyData, TyId, TyParam},
    unify::UnificationTable,
};
use crate::analysis::{HirAnalysisDb, name_resolution::find_associated_type};

/// Normalizes a type by resolving all associated types to concrete types when possible.
///
/// This function takes a type and attempts to resolve any associated types within it
/// using the provided assumptions and scope context. It handles:
/// - Simple associated types (e.g., `T::Output`)
/// - Nested associated types (e.g., `T::Encoder::Output`)
/// - Associated types with generic parameters
pub fn normalize_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    ty.fold_with(db, &mut normalizer)
}

pub struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    // Projection cache: None = in progress (cycle guard), Some(ty) = normalized result
    cache: FxHashMap<AssocTy<'db>, Option<TyId<'db>>>,
}

impl<'db> TypeNormalizer<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self {
            db,
            scope,
            assumptions,
            cache: FxHashMap::default(),
        }
    }
}

impl<'db> TyFolder<'db> for TypeNormalizer<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(self.db) {
            TyData::TyParam(p @ TyParam { owner, .. }) if p.is_trait_self() => {
                if let Some(impl_) = owner.resolve_to::<ImplTrait>(self.db) {
                    // Use the item method to obtain the implementor's self type.
                    let lowered = impl_.ty(self.db);
                    return self.fold_ty(db, lowered);
                }
                ty
            }
            TyData::AssocTy(assoc_ty) => {
                match self.cache.entry(*assoc_ty) {
                    Entry::Occupied(entry) => match entry.get() {
                        Some(cached) => return *cached,
                        None => return ty, // cycle: leave unresolved
                    },
                    Entry::Vacant(entry) => {
                        entry.insert(None);
                    }
                }

                if let Some(replacement) = self.try_resolve_assoc_ty(ty, assoc_ty) {
                    let normalized = self.fold_ty(db, replacement);
                    self.cache.insert(*assoc_ty, Some(normalized));
                    return normalized;
                }

                // Not resolved; still fold internals (e.g., normalize self type)
                let folded = ty.super_fold_with(db, self);
                self.cache.insert(*assoc_ty, Some(folded));
                folded
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

impl<'db> TypeNormalizer<'db> {
    fn try_resolve_assoc_ty(&mut self, ty: TyId<'db>, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        // 1) Check if the trait instance itself carries an explicit binding
        if let Some(&bound_ty) = assoc.trait_.assoc_type_bindings(self.db).get(&assoc.name) {
            return Some(bound_ty);
        }

        // 2) Check assumptions for an equivalent trait instance that carries
        //    an explicit associated type binding (e.g., from where-clauses).
        for &pred in self.assumptions.list(self.db) {
            if pred.def(self.db) != assoc.trait_.def(self.db) {
                continue;
            }

            let mut table = UnificationTable::new(self.db);
            // Normalize self types before attempting unification to avoid
            // requiring a second outer pass for resolution.
            let lhs_self = self.fold_ty(self.db, assoc.trait_.self_ty(self.db));
            let rhs_self = self.fold_ty(self.db, pred.self_ty(self.db));
            if table.unify(lhs_self, rhs_self).is_ok()
                && let Some(&bound) = pred.assoc_type_bindings(self.db).get(&assoc.name)
            {
                return Some(bound.fold_with(self.db, &mut table));
            }
        }

        // 3) Fall back to the general associated type search used by path resolution,
        //    but restrict results to the same trait as `assoc` and deduplicate by
        //    the resulting type. If all viable candidates agree on a single type,
        //    normalize to that type.
        //
        // First attempt an impl-based lookup across relevant ingots (Self's + trait's),
        // mirroring trait-method resolution. This allows normalization to succeed even
        // when the calling scope is in a different ingot (e.g., core code instantiated
        // with std types).
        if let Some(resolved) = self.try_resolve_assoc_ty_from_impls(assoc) {
            return Some(resolved);
        }

        //    Search by the trait's self type: `SelfTy::assoc.name`.
        // Normalize the trait's self type before candidate search.
        let self_ty = self.fold_ty(self.db, assoc.trait_.self_ty(self.db));
        let mut raw_cands = find_associated_type(
            self.db,
            self.scope,
            Canonical::new(self.db, self_ty),
            assoc.name,
            self.assumptions,
        );

        // Keep only candidates from the same trait as `assoc`.
        raw_cands.retain(|(inst, _)| inst.def(self.db) == assoc.trait_.def(self.db));

        // Deduplicate by normalized result type (to handle cases where multiple
        // impls yield the same associated type, e.g., Output = Self for all impls).
        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();
        for (_, t) in raw_cands.into_iter() {
            // Continue folding so nested associated types are also normalized
            let norm_t = self.fold_ty(self.db, t);
            dedup.entry(norm_t).or_insert(());
        }

        match dedup.len() {
            0 => None,
            1 => {
                let (unique, _) = dedup.first().unwrap();
                // Only replace if we're actually making progress
                if *unique != ty { Some(*unique) } else { None }
            }
            _ => None,
        }
    }

    fn try_resolve_assoc_ty_from_impls(&mut self, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        let trait_def = assoc.trait_.def(self.db);
        let trait_ingot = trait_def.ingot(self.db);

        let self_ty = self.fold_ty(self.db, assoc.trait_.self_ty(self.db));
        let self_ingot = self_ty.ingot(self.db);

        let canonical_self_ty = Canonical::new(self.db, self_ty);

        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();

        let mut search_ingots = Vec::with_capacity(2);
        if let Some(ingot) = self_ingot {
            search_ingots.push(ingot);
        }
        if self_ingot != Some(trait_ingot) {
            search_ingots.push(trait_ingot);
        }

        for ingot in search_ingots {
            for implementor in
                impls_for_ty_with_constraints(self.db, ingot, canonical_self_ty, self.assumptions)
            {
                let mut table = UnificationTable::new(self.db);
                let implementor = table.instantiate_with_fresh_vars(implementor);
                if table
                    .unify(implementor.trait_(self.db), assoc.trait_)
                    .is_err()
                {
                    continue;
                }

                let Some(assoc_ty) = implementor.assoc_ty(self.db, assoc.name) else {
                    continue;
                };

                // Apply substitutions and continue folding so nested projections
                // are also normalized before dedup.
                let folded = assoc_ty.fold_with(self.db, &mut table);
                let norm = self.fold_ty(self.db, folded);
                dedup.entry(norm).or_insert(());
            }
        }

        match dedup.len() {
            0 => None,
            1 => Some(*dedup.first().unwrap().0),
            _ => None,
        }
    }
}
