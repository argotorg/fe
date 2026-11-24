//! The compiler's implementation of `core::marker::Clearable`.
//!
//! A type is clearable when its all-zero representation is a value, so
//! zeroing the slots and lanes of a stored value returns it to its zero
//! value: scalars, raw pointers, and aggregates of clearable types with no
//! `unsafe` fields. An `unsafe` field carries an invariant zero need not
//! satisfy, as a zeroed `FixedMemBuffer` would claim memory at address 0. A
//! pinned type (a collection) is not clearable, since its entries lie outside
//! the slots its own place takes, so a value holding one has no finite set
//! of stores that clears it. No `impl` item implements the trait; the trait
//! solver proves a goal through the components named here.

use common::indexmap::IndexMap;

use super::{
    adt_def::{AdtDef, AdtRef},
    corelib::resolve_core_trait,
    normalize::normalize_ty,
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId},
    trait_resolution::PredicateListId,
    ty_def::{PrimTy, TyBase, TyData, TyId},
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::HirAnalysisDb;

/// The implementation proving `goal` when it is a `Clearable` goal on a
/// type the compiler decomposes, with the goals proving it in turn: one per
/// component type.
pub(crate) fn clearable_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> Option<(ImplementorId<'db>, Vec<TraitInstId<'db>>)> {
    let clearable = resolve_core_trait(db, goal.def(db).scope(), &["marker", "Clearable"])?;
    if goal.def(db) != clearable {
        return None;
    }
    let components = clearable_components(db, goal.self_ty(db))?
        .into_iter()
        .map(|component| TraitInstId::new_simple(db, clearable, vec![component]))
        .collect();
    let implementor = ImplementorId::new(
        db,
        goal,
        Vec::new(),
        IndexMap::new(),
        ImplementorOrigin::Structural,
    );
    Some((implementor, components))
}

/// Whether `goal` is a `Clearable` goal on a type that names a struct or an
/// enum. Impls decide such a goal: one may pin the type, and what its
/// fields' projections resolve to decides whether it is uninhabited, either
/// of which makes it not clearable. A search that doesn't resolve
/// projections through impls can't decide it, and deciding it from what
/// they resolve to would let an impl's where clause refute itself, as `impl
/// Tr for X where S<X>: Clearable { type Out = ! }` does for `struct S<T:
/// Tr> { f: T::Out }`.
pub(crate) fn clearable_needs_impls<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> bool {
    struct NamesAdt<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }
    impl<'db> TyVisitor<'db> for NamesAdt<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_adt(&mut self, _: AdtDef<'db>) {
            self.found = true;
        }
    }

    let mut visitor = NamesAdt { db, found: false };
    if resolve_core_trait(db, goal.def(db).scope(), &["marker", "Clearable"]) == Some(goal.def(db))
    {
        goal.self_ty(db).visit_with(&mut visitor);
    }
    visitor.found
}

/// The types `ty` is clearable through, or `None` when it is not: a pinned
/// type, one with `unsafe` fields, a type the compiler does not decompose,
/// or one still to be inferred.
fn clearable_components<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Vec<TyId<'db>>> {
    if ty.is_uninhabited(db) {
        return None;
    }
    let (base, args) = ty.decompose_ty_app(db);
    match base.data(db) {
        TyData::TyBase(TyBase::Prim(PrimTy::Array)) if ty.array_len(db) == Some(0) => {
            Some(Vec::new())
        }
        TyData::TyBase(TyBase::Prim(PrimTy::Tuple(_) | PrimTy::Array)) => Some(
            args.iter()
                .copied()
                .filter(|arg| !matches!(arg.data(db), TyData::ConstTy(_)))
                .collect(),
        ),
        TyData::TyBase(TyBase::Prim(PrimTy::View | PrimTy::BorrowMut | PrimTy::BorrowRef)) => None,
        TyData::TyBase(TyBase::Prim(_)) => Some(Vec::new()),
        TyData::TyBase(TyBase::Adt(adt)) => {
            if ty.may_be_pinned(db)
                || adt.has_unsafe_field(db)
                || args.len() != adt.params(db).len()
            {
                return None;
            }
            if matches!(adt.adt_ref(db), AdtRef::Enum(_)) && ty.is_enum_variant_uninhabited(db, 0) {
                return None;
            }
            Some(
                adt.fields(db)
                    .iter()
                    .enumerate()
                    .filter(|&(variant_idx, _)| {
                        !matches!(adt.adt_ref(db), AdtRef::Enum(_))
                            || !ty.is_enum_variant_uninhabited(db, variant_idx)
                    })
                    .flat_map(|(_, variant)| variant.iter_types(db))
                    .map(|field| {
                        normalize_ty(
                            db,
                            field.instantiate(db, args),
                            adt.scope(db),
                            PredicateListId::empty_list(db),
                        )
                    })
                    .collect(),
            )
        }
        _ => None,
    }
}
