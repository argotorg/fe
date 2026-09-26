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
    binder::Binder,
    canonical::Canonical,
    canonical::Canonicalized,
    fold::{TyFoldable, TyFolder},
    layout_holes::LayoutRootUse,
    trait_def::{
        ImplementorOrigin, TraitInstId, TraitRefId,
        impls_for_trait_and_ty_with_possible_constraints, resolve_trait_impl_instance,
    },
    trait_lower::complete_impl_assoc_ty,
    trait_resolution::{PredicateListId, Selection, TraitSolveCx},
    ty_def::{AssocTy, TyData, TyId, TyParam, collect_variables},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{FindAssociatedTypeError, find_associated_type},
};

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

/// Apply declared associated equalities without implementation selection.
/// Structural slot planning uses declaration coordinates before the slots it
/// is discovering exist, so implementation lookup would create a query cycle.
pub fn normalize_from_assumptions<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    let mut normalizer = TypeNormalizer::new(db, scope, assumptions);
    normalizer.resolve_impls = false;
    value.fold_with(db, &mut normalizer)
}

/// Apply the associated equalities carried by a single trait predicate, as
/// [`normalize_from_assumptions`] does.
pub fn normalize_with_trait_evidence<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    scope: ScopeId<'db>,
    evidence: TraitInstId<'db>,
) -> T
where
    T: TyFoldable<'db>,
{
    normalize_from_assumptions(db, value, scope, PredicateListId::new(db, vec![evidence]))
}

pub(crate) fn normalize_layout_root_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<LayoutRootUse<'db>> {
    fn collect<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        visiting: &mut rustc_hash::FxHashSet<TyId<'db>>,
        uses: &mut Vec<LayoutRootUse<'db>>,
    ) {
        if !visiting.insert(ty) {
            return;
        }
        if let TyData::AssocTy(assoc) = ty.data(db) {
            let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
            if let Selection::Unique(resolved) =
                resolve_trait_impl_instance(db, solve_cx, assoc.trait_.as_predicate(db))
                && let ImplementorOrigin::Hir(impl_trait) = resolved.selected().origin(db)
            {
                for root_use in resolved.assoc_ty_layout_root_uses(db, assoc.name) {
                    let root_use = LayoutRootUse {
                        value: Binder::bind(impl_trait.into(), root_use.value)
                            .instantiate(db, resolved.impl_args(db)),
                        owner: root_use.owner.map(|owner| {
                            normalize_ty(
                                db,
                                Binder::bind(impl_trait.into(), owner)
                                    .instantiate(db, resolved.impl_args(db)),
                                scope,
                                assumptions,
                            )
                        }),
                        selector: root_use.selector,
                        index_dimensions: root_use.index_dimensions,
                    };
                    if !uses.contains(&root_use) {
                        uses.push(root_use);
                    }
                }
                if let Some(instantiated) = resolved.instantiated_assoc_ty(db, assoc.name) {
                    collect(db, instantiated, scope, assumptions, visiting, uses);
                }
            }
        } else {
            let (base, args) = ty.decompose_ty_app(db);
            if base != ty {
                collect(db, base, scope, assumptions, visiting, uses);
            }
            for arg in args {
                collect(db, *arg, scope, assumptions, visiting, uses);
            }
        }
        visiting.remove(&ty);
    }

    let mut uses = Vec::new();
    collect(
        db,
        ty,
        scope,
        assumptions,
        &mut rustc_hash::FxHashSet::default(),
        &mut uses,
    );
    uses
}

pub struct TypeNormalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    resolve_impls: bool,
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
            resolve_impls: true,
            cache: FxHashMap::default(),
        }
    }
}

impl<'db> TyFolder<'db> for TypeNormalizer<'db> {
    fn fold_ty_app(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        abs: TyId<'db>,
        arg: TyId<'db>,
    ) -> TyId<'db> {
        if self.resolve_impls {
            TyId::app(db, abs, arg)
        } else {
            TyId::app_structural(db, abs, arg)
        }
    }

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
        // Equality evidence is separate from the projection's identity. Match
        // its entire trait reference before using an assumption's binding.
        let target = assoc.trait_.fold_with(self.db, self);
        let mut matching_bounds: IndexMap<TyId<'db>, ()> = IndexMap::new();
        for &pred in self.assumptions.list(self.db) {
            let Some(bound) = pred.bound_assoc_ty(self.db, assoc.name) else {
                continue;
            };
            if self.trait_refs_match(target, pred.trait_ref(self.db)) {
                matching_bounds.insert(self.fold_ty(self.db, bound), ());
            }
        }
        if matching_bounds.len() > 1 {
            return None;
        }
        if let Some((&bound, _)) = matching_bounds.first() {
            return (bound != ty).then_some(bound);
        }

        if !self.resolve_impls {
            return None;
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
        let mut raw_cands = match find_associated_type(
            self.db,
            self.scope,
            Canonicalized::new(self.db, self_ty),
            assoc.name,
            self.assumptions,
        ) {
            Ok(raw_cands) => raw_cands,
            Err(FindAssociatedTypeError::InfiniteBoundRecursion) => return None,
        };

        raw_cands.retain(|(inst, _)| self.trait_refs_match(target, inst.trait_ref(self.db)));

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

    /// A pure normalization query may observe established equality, but may
    /// not choose a binding by assigning an unresolved caller inference var.
    fn trait_refs_match(&mut self, target: TraitRefId<'db>, candidate: TraitRefId<'db>) -> bool {
        if target.def(self.db) != candidate.def(self.db) {
            return false;
        }
        let candidate = candidate.fold_with(self.db, self);
        if target == candidate {
            return true;
        }
        if !collect_variables(self.db, &target).is_empty()
            || !collect_variables(self.db, &candidate).is_empty()
        {
            return false;
        }
        UnificationTable::new(self.db)
            .unify::<TraitRefId<'db>>(target, candidate)
            .is_ok()
    }

    fn try_resolve_assoc_ty_from_impls(&mut self, assoc: &AssocTy<'db>) -> Option<TyId<'db>> {
        let trait_inst = assoc.trait_.fold_with(self.db, self).as_predicate(self.db);
        let trait_def = trait_inst.def(self.db);
        let canonical_self_ty = Canonical::new(self.db, trait_inst.self_ty(self.db));

        let mut dedup: IndexMap<TyId<'db>, ()> = IndexMap::new();

        let solve_cx = TraitSolveCx::new(self.db, self.scope).with_assumptions(self.assumptions);
        let (primary, secondary) = solve_cx.search_ingots_for_trait_inst(self.db, trait_inst);
        let search_ingots = [Some(primary), secondary];

        // Canonicalize the target trait instance so we can unify against it in a
        // fresh table without mixing inference keys from other tables.
        let canonical_target = Canonicalized::new(self.db, trait_inst);
        canonical_target.with_materialized(self.db, |cx| {
            let target_inst = cx.query();
            let original_target = cx.try_extract::<TraitInstId<'db>>(target_inst);
            for ingot in search_ingots.into_iter().flatten() {
                for implementor in impls_for_trait_and_ty_with_possible_constraints(
                    self.db,
                    ingot,
                    trait_def,
                    canonical_self_ty,
                    self.assumptions,
                ) {
                    let Some(implementor) =
                        complete_impl_assoc_ty(self.db, implementor, assoc.name)
                    else {
                        continue;
                    };
                    let candidate = cx.with_impl_assoc_ty(
                        implementor,
                        target_inst.self_ty(self.db),
                        assoc.name,
                        |cx, inst, assoc_ty| {
                            cx.unify::<TraitInstId<'db>>(inst, target_inst).ok()?;
                            if cx.try_extract::<TraitInstId<'db>>(target_inst) != original_target {
                                return None;
                            }
                            let assoc_ty = cx.resolve::<TyId<'db>>(assoc_ty);
                            cx.try_extract::<TyId<'db>>(assoc_ty)
                        },
                    );

                    // Extract into the caller's inference environment before
                    // continuing normalization, so scratch-local vars never
                    // leak into the cache.
                    if let Some(Some(folded)) = candidate {
                        let norm = self.fold_ty(self.db, folded);
                        dedup.entry(norm).or_insert(());
                    }
                }
            }
        });

        match dedup.len() {
            0 => None,
            1 => Some(*dedup.first().unwrap().0),
            _ => None,
        }
    }
}
