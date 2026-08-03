use cranelift_entity::SecondaryMap;
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use crate::{
    analysis::{
        HirAnalysisDb,
        place::projectable_place_ty,
        semantic::{
            BorrowActivation, BorrowSlotFamilyId, FieldIndex, LayoutBackingProjection, SBlockId,
            SLocalId, SemOrigin, SemanticInstance, VariantIndex,
        },
        ty::{
            adt_def::{AdtRef, instantiate_adt_field_shape},
            provider::{ProviderAddressSpace, ProviderKind},
            ty_check::LocalBinding,
            ty_def::{BorrowKind, TyId},
            ty_is_noesc,
        },
    },
    projection::{Aliasing, IndexSource, Projection, ProjectionPath},
};

use super::{
    diagnostics::normalized_body_internal_diag,
    ir::{
        BorrowResult, NBorrowRoot, NBorrowRootId, NExpr, NLayoutBackingSource, NSPlace,
        NSPlaceRoot, NSProjectionPath, NSStmt, NSStmtKind, NormalizedBindingLowering,
        NormalizedSemanticBody, SemanticBorrowDiagnostic, layout_path_for_semantic_projection,
        resolved_layout_backing_places, semantic_projection_for_layout_path,
        semantic_projection_ty,
    },
};

pub(super) fn address_space_for_borrow_root<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedSemanticBody<'db>,
    root: &BorrowRoot<'db>,
    origin: SemOrigin<'db>,
) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
    match root {
        BorrowRoot::Param(_) | BorrowRoot::Local(_) => Ok(ProviderAddressSpace::Memory),
        BorrowRoot::Provider(binding) => match binding.semantics.address_space {
            Some(space) => Ok(space),
            None if matches!(binding.semantics.kind, ProviderKind::RootObject) => {
                Ok(ProviderAddressSpace::Memory)
            }
            None => Err(normalized_body_internal_diag(
                db,
                instance,
                body,
                origin,
                format!(
                    "provider `{}` has no address space",
                    binding.provider_ty.pretty_print(db)
                ),
            )),
        },
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct LoanId(pub(super) u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum BorrowRoot<'db> {
    Param(u32),
    Local(SLocalId),
    Provider(crate::semantic::ProviderBinding<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum CanonIndex {
    Local(SLocalId),
    Family(BorrowSlotFamilyId),
    Any,
}

pub(super) type CanonProjectionPath<'db> = ProjectionPath<TyId<'db>, VariantIndex, CanonIndex>;

pub(super) fn canon_projection_from_semantic<'db>(
    path: &NSProjectionPath<'db>,
) -> CanonProjectionPath<'db> {
    let mut out = CanonProjectionPath::new();
    for projection in path.iter() {
        out.push(match projection {
            Projection::Field(field) => Projection::Field(*field),
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => Projection::VariantField {
                variant: *variant,
                enum_ty: *enum_ty,
                field_idx: *field_idx,
            },
            Projection::Index(IndexSource::Constant(index)) => {
                Projection::Index(IndexSource::Constant(*index))
            }
            Projection::Index(IndexSource::Dynamic(index)) => {
                Projection::Index(IndexSource::Dynamic(CanonIndex::Local(*index)))
            }
            Projection::Deref => Projection::Deref,
            Projection::Discriminant => Projection::Discriminant,
        });
    }
    out
}

pub(super) fn semantic_projection_from_canon<'db>(
    path: &CanonProjectionPath<'db>,
) -> Option<NSProjectionPath<'db>> {
    let mut out = NSProjectionPath::new();
    for projection in path.iter() {
        out.push(match projection {
            Projection::Field(field) => Projection::Field(*field),
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => Projection::VariantField {
                variant: *variant,
                enum_ty: *enum_ty,
                field_idx: *field_idx,
            },
            Projection::Index(IndexSource::Constant(index)) => {
                Projection::Index(IndexSource::Constant(*index))
            }
            Projection::Index(IndexSource::Dynamic(CanonIndex::Local(index))) => {
                Projection::Index(IndexSource::Dynamic(*index))
            }
            Projection::Index(IndexSource::Dynamic(CanonIndex::Family(_) | CanonIndex::Any)) => {
                return None;
            }
            Projection::Deref => Projection::Deref,
            Projection::Discriminant => Projection::Discriminant,
        });
    }
    Some(out)
}

pub(super) fn layout_path_for_canon_projection<'db>(
    path: &CanonProjectionPath<'db>,
) -> Option<Vec<LayoutBackingProjection>> {
    let mut out = Vec::new();
    for projection in path.iter() {
        match projection {
            Projection::Field(field) => out.push(LayoutBackingProjection::Field(FieldIndex(
                u16::try_from(*field).ok()?,
            ))),
            Projection::VariantField {
                variant, field_idx, ..
            } => out.push(LayoutBackingProjection::VariantField {
                variant: *variant,
                field: FieldIndex(u16::try_from(*field_idx).ok()?),
            }),
            Projection::Index(IndexSource::Constant(index)) => {
                out.push(LayoutBackingProjection::Index(Some(*index)));
            }
            Projection::Index(IndexSource::Dynamic(CanonIndex::Family(family))) => {
                out.push(LayoutBackingProjection::IndexFamily(*family));
            }
            Projection::Index(IndexSource::Dynamic(CanonIndex::Local(_) | CanonIndex::Any)) => {
                out.push(LayoutBackingProjection::Index(None));
            }
            Projection::Deref => {}
            Projection::Discriminant => return None,
        }
    }
    Some(out)
}

pub(super) fn canon_projection_for_layout_path<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: TyId<'db>,
    path: &[LayoutBackingProjection],
) -> Option<CanonProjectionPath<'db>> {
    let mut out = CanonProjectionPath::new();
    for step in path {
        ty = projectable_place_ty(db, ty);
        match *step {
            LayoutBackingProjection::Field(field) => {
                ty = *ty.field_types(db).get(field.0 as usize)?;
                out.push(Projection::Field(field.0 as usize));
            }
            LayoutBackingProjection::VariantField { variant, field } => {
                let adt = ty.adt_def(db)?;
                if !matches!(adt.adt_ref(db), AdtRef::Enum(_)) {
                    return None;
                }
                let field_ty = instantiate_adt_field_shape(
                    db,
                    adt,
                    variant.0 as usize,
                    field.0 as usize,
                    ty.generic_args(db),
                );
                out.push(Projection::VariantField {
                    variant,
                    enum_ty: ty,
                    field_idx: field.0 as usize,
                });
                ty = field_ty;
            }
            LayoutBackingProjection::Index(index) => {
                if !ty.is_array(db)
                    || index.is_some_and(|index| ty.array_len(db).is_some_and(|len| index >= len))
                {
                    return None;
                }
                ty = *ty.generic_args(db).first()?;
                out.push(Projection::Index(match index {
                    Some(index) => IndexSource::Constant(index),
                    None => IndexSource::Dynamic(CanonIndex::Any),
                }));
            }
            LayoutBackingProjection::IndexFamily(family) => {
                if !ty.is_array(db) {
                    return None;
                }
                ty = *ty.generic_args(db).first()?;
                out.push(Projection::Index(IndexSource::Dynamic(CanonIndex::Family(
                    family,
                ))));
            }
        }
    }
    Some(out)
}

fn resolved_family_binding(
    bindings: &FamilyBindings,
    family: BorrowSlotFamilyId,
) -> Option<LayoutBackingProjection> {
    let mut current = family;
    for _ in 0..=bindings.len() {
        let Some(value) = bindings
            .iter()
            .find_map(|(candidate, value)| (*candidate == current).then_some(*value))
        else {
            return (current != family).then_some(LayoutBackingProjection::IndexFamily(current));
        };
        match value {
            LayoutBackingProjection::IndexFamily(next) if next != current => current = next,
            value => return Some(value),
        }
    }
    Some(LayoutBackingProjection::IndexFamily(current))
}

fn instantiate_canon_projection<'db>(
    path: &CanonProjectionPath<'db>,
    bindings: &FamilyBindings,
) -> CanonProjectionPath<'db> {
    let mut out = CanonProjectionPath::new();
    for projection in path.iter() {
        out.push(match projection {
            Projection::Index(IndexSource::Dynamic(CanonIndex::Family(family))) => {
                match resolved_family_binding(bindings, *family) {
                    Some(LayoutBackingProjection::Index(Some(index))) => {
                        Projection::Index(IndexSource::Constant(index))
                    }
                    Some(LayoutBackingProjection::Index(None)) => {
                        Projection::Index(IndexSource::Dynamic(CanonIndex::Any))
                    }
                    Some(LayoutBackingProjection::IndexFamily(family)) => {
                        Projection::Index(IndexSource::Dynamic(CanonIndex::Family(family)))
                    }
                    Some(
                        LayoutBackingProjection::Field(_)
                        | LayoutBackingProjection::VariantField { .. },
                    )
                    | None => projection.clone(),
                }
            }
            projection => projection.clone(),
        });
    }
    out
}

fn insert_family_binding(
    bindings: &mut FamilyBindings,
    family: BorrowSlotFamilyId,
    value: LayoutBackingProjection,
) -> bool {
    if let Some((_, existing)) = bindings.iter().find(|(candidate, _)| *candidate == family) {
        return *existing == value;
    }
    bindings.push((family, value));
    bindings.sort_unstable_by_key(|(family, _)| *family);
    true
}

fn binding_accepts(
    bindings: &FamilyBindings,
    family: BorrowSlotFamilyId,
    expected: LayoutBackingProjection,
) -> bool {
    match resolved_family_binding(bindings, family) {
        Some(actual @ LayoutBackingProjection::Index(Some(_))) => actual == expected,
        Some(LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_))
        | None => true,
        Some(LayoutBackingProjection::Field(_) | LayoutBackingProjection::VariantField { .. }) => {
            false
        }
    }
}

fn remap_indexed_target<'db>(
    indexed: &IndexedLoanTarget<'db>,
    held_bindings: &FamilyBindings,
    query_bindings: &FamilyBindings,
) -> Option<IndexedLoanTarget<'db>> {
    let mut bindings = query_bindings.clone();
    for &(family, expected) in &indexed.bindings {
        match resolved_family_binding(held_bindings, family) {
            Some(LayoutBackingProjection::IndexFamily(mapped)) => {
                if !insert_family_binding(&mut bindings, mapped, expected) {
                    return None;
                }
            }
            Some(LayoutBackingProjection::Index(Some(actual))) => {
                if expected != LayoutBackingProjection::Index(Some(actual)) {
                    return None;
                }
            }
            Some(LayoutBackingProjection::Index(None)) => {}
            Some(
                LayoutBackingProjection::Field(_) | LayoutBackingProjection::VariantField { .. },
            ) => return None,
            None => {
                if !insert_family_binding(&mut bindings, family, expected) {
                    return None;
                }
            }
        }
    }
    Some(IndexedLoanTarget {
        bindings,
        fallback: indexed.fallback,
        shadows_fallback: indexed.shadows_fallback,
        target: CanonPlace {
            root: indexed.target.root.clone(),
            proj: instantiate_canon_projection(&indexed.target.proj, held_bindings),
        },
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct CanonPlace<'db> {
    pub(super) root: BorrowRoot<'db>,
    pub(super) proj: CanonProjectionPath<'db>,
}

#[derive(Clone, PartialEq, Eq)]
pub(super) struct LoanRegion<'db> {
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) exclusions: FxHashSet<CanonPlace<'db>>,
}

impl<'db> LoanRegion<'db> {
    pub(super) fn active_targets(&self) -> FxHashSet<CanonPlace<'db>> {
        self.targets
            .iter()
            .filter(|target| {
                !self
                    .exclusions
                    .iter()
                    .any(|excluded| place_covers(excluded, target))
            })
            .cloned()
            .collect()
    }

    pub(super) fn covers(&self, target: &CanonPlace<'_>) -> bool {
        self.targets
            .iter()
            .any(|region| place_covers(region, target))
            && !self
                .exclusions
                .iter()
                .any(|excluded| places_overlap(excluded, target))
    }

    pub(super) fn overlaps(&self, target: &CanonPlace<'_>) -> bool {
        self.targets
            .iter()
            .any(|region| places_overlap(region, target))
            && !self
                .exclusions
                .iter()
                .any(|excluded| place_covers(excluded, target))
    }
}

fn place_covers(container: &CanonPlace<'_>, target: &CanonPlace<'_>) -> bool {
    container.root == target.root
        && container.proj.len() <= target.proj.len()
        && container
            .proj
            .iter()
            .zip(target.proj.iter())
            .all(|(container, target)| {
                container == target
                    || matches!(
                        container,
                        Projection::Index(IndexSource::Dynamic(CanonIndex::Any))
                    )
            })
}

#[derive(Clone, Debug)]
pub(super) struct Loan<'db> {
    pub(super) kind: BorrowKind,
    pub(super) activation: BorrowActivation,
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) unconditional_targets: FxHashSet<CanonPlace<'db>>,
    pub(super) indexed_targets: Vec<IndexedLoanTarget<'db>>,
    pub(super) result_exclusions: Vec<FamilyBindings>,
    pub(super) parents: FxHashSet<LoanId>,
    pub(super) origin: SemOrigin<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct IndexedLoanTarget<'db> {
    bindings: FamilyBindings,
    fallback: bool,
    shadows_fallback: bool,
    pub(super) target: CanonPlace<'db>,
}

pub(super) struct CanonicalizedCallInput<'db> {
    pub(super) targets: FxHashSet<CanonPlace<'db>>,
    pub(super) unconditional_targets: FxHashSet<CanonPlace<'db>>,
    pub(super) indexed_targets: Vec<IndexedLoanTarget<'db>>,
}

pub(super) fn indexed_target_is_excluded(
    indexed: &IndexedLoanTarget<'_>,
    exclusions: &[FamilyBindings],
) -> bool {
    exclusions.iter().any(|exclusion| {
        exclusion.iter().all(|(family, expected)| {
            indexed
                .bindings
                .iter()
                .find_map(|(candidate, actual)| (*candidate == *family).then_some(actual))
                == Some(expected)
        })
    })
}

#[derive(Clone, Debug)]
pub(super) struct MoveSite<'db> {
    pub(super) origin: SemOrigin<'db>,
    pub(super) note: String,
}

pub(super) type MovedPlaces<'db> = FxHashMap<CanonPlace<'db>, MoveSite<'db>>;
pub(super) type BlockAdjacency = SmallVec<SBlockId, 2>;
pub(super) type CfgAdjacency = SecondaryMap<SBlockId, BlockAdjacency>;

pub(super) type HeldLoanPath = Vec<LayoutBackingProjection>;
pub(super) type FamilyBindings = Vec<(BorrowSlotFamilyId, LayoutBackingProjection)>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct HeldLoan {
    pub(super) id: LoanId,
    bindings: FamilyBindings,
}

impl HeldLoan {
    pub(super) fn new(id: LoanId) -> Self {
        Self {
            id,
            bindings: Vec::new(),
        }
    }

    fn with_match_bindings(
        &self,
        stored: &[LayoutBackingProjection],
        query: &[LayoutBackingProjection],
    ) -> Option<Self> {
        let mut loan = self.clone();
        for (&stored, &query) in stored.iter().zip(query) {
            let LayoutBackingProjection::IndexFamily(family) = stored else {
                continue;
            };
            if !matches!(
                query,
                LayoutBackingProjection::Index(_) | LayoutBackingProjection::IndexFamily(_)
            ) {
                return None;
            }
            if query == LayoutBackingProjection::IndexFamily(family) {
                continue;
            }
            if !insert_family_binding(&mut loan.bindings, family, query) {
                return None;
            }
        }
        Some(loan)
    }
}

pub(super) type HeldLoans = FxHashMap<HeldLoanPath, FxHashSet<HeldLoan>>;

fn layout_projection_matches(lhs: LayoutBackingProjection, rhs: LayoutBackingProjection) -> bool {
    let is_index = |projection| {
        matches!(
            projection,
            LayoutBackingProjection::Index(_) | LayoutBackingProjection::IndexFamily(_)
        )
    };
    let is_symbolic = |projection| {
        matches!(
            projection,
            LayoutBackingProjection::Index(None) | LayoutBackingProjection::IndexFamily(_)
        )
    };
    lhs == rhs || is_index(lhs) && is_index(rhs) && (is_symbolic(lhs) || is_symbolic(rhs))
}

fn layout_path_is_prefix(
    prefix: &[LayoutBackingProjection],
    path: &[LayoutBackingProjection],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(lhs, rhs)| layout_projection_matches(lhs, rhs))
}

fn prefixed_held_loans(held: HeldLoans, prefix: &[LayoutBackingProjection]) -> HeldLoans {
    held.into_iter()
        .map(|(path, loans)| {
            let mut prefixed = prefix.to_vec();
            prefixed.extend(path);
            (prefixed, loans)
        })
        .collect()
}

fn merge_held_loans(into: &mut HeldLoans, from: HeldLoans) {
    for (path, loans) in from {
        into.entry(path).or_default().extend(loans);
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(super) struct State {
    pub(super) local_loans: FxHashMap<SLocalId, HeldLoans>,
    definite_overrides: FxHashMap<SLocalId, FxHashSet<HeldLoanPath>>,
}

impl State {
    pub(super) fn loans_in(&self, local: SLocalId) -> FxHashSet<LoanId> {
        self.local_loans
            .get(&local)
            .into_iter()
            .flat_map(|held| held.values())
            .flatten()
            .map(|loan| loan.id)
            .collect()
    }

    pub(super) fn held_loans_in(&self, local: SLocalId) -> HeldLoans {
        self.local_loans.get(&local).cloned().unwrap_or_default()
    }

    pub(super) fn materialize_borrow_result(
        &self,
        local: SLocalId,
        template: &BorrowResult,
        loans: &[Loan<'_>],
        layout_backing_sources: &[NLayoutBackingSource<'_>],
        retain_symbolic_fallback: bool,
    ) -> Vec<BorrowResult> {
        if !template
            .projection
            .iter()
            .any(|step| matches!(step, LayoutBackingProjection::IndexFamily(_)))
        {
            return vec![template.clone()];
        }

        let mut out = Vec::new();
        for (path, held_loans) in self.local_loans.get(&local).into_iter().flatten() {
            if path.len() != template.projection.len()
                || !path
                    .iter()
                    .copied()
                    .zip(template.projection.iter().copied())
                    .all(|(stored, result)| layout_projection_matches(stored, result))
            {
                continue;
            }
            let base_projection = template
                .projection
                .iter()
                .copied()
                .zip(path.iter().copied())
                .map(|(result, stored)| match (result, stored) {
                    (
                        LayoutBackingProjection::IndexFamily(_),
                        LayoutBackingProjection::Index(Some(index)),
                    ) => LayoutBackingProjection::Index(Some(index)),
                    (result, _) => result,
                })
                .collect::<Vec<_>>();
            let path_is_symbolic = path
                .iter()
                .any(|step| matches!(step, LayoutBackingProjection::IndexFamily(_)));
            if !path_is_symbolic {
                out.push(BorrowResult {
                    kind: template.kind,
                    projection: base_projection,
                });
                continue;
            }

            let mut has_unconditional = false;
            let mut has_indexed = false;
            for held in held_loans {
                let loan = &loans[held.id.0 as usize];
                has_unconditional |= !loan.unconditional_targets.is_empty();
                for indexed in &loan.indexed_targets {
                    let Some(indexed) =
                        remap_indexed_target(indexed, &held.bindings, &FamilyBindings::new())
                    else {
                        continue;
                    };
                    has_indexed = true;
                    let projection = base_projection
                        .iter()
                        .copied()
                        .map(|step| {
                            let LayoutBackingProjection::IndexFamily(family) = step else {
                                return step;
                            };
                            indexed
                                .bindings
                                .iter()
                                .find_map(|(candidate, value)| {
                                    (*candidate == family).then_some(*value)
                                })
                                .filter(|value| {
                                    matches!(value, LayoutBackingProjection::Index(Some(_)))
                                })
                                .unwrap_or(step)
                        })
                        .collect();
                    out.push(BorrowResult {
                        kind: template.kind,
                        projection,
                    });
                }
            }
            if has_unconditional || !has_indexed {
                out.push(BorrowResult {
                    kind: template.kind,
                    projection: base_projection,
                });
            }
        }
        let layout_results = layout_backing_sources
            .iter()
            .filter(|source| {
                source.target.len() <= template.projection.len()
                    && source
                        .target
                        .iter()
                        .copied()
                        .zip(template.projection.iter().copied())
                        .all(|(source, result)| layout_projection_matches(source, result))
            })
            .map(|source| {
                let mut projection = template.projection.clone();
                for (result, source) in projection.iter_mut().zip(source.target.iter().copied()) {
                    if let (
                        LayoutBackingProjection::IndexFamily(_),
                        LayoutBackingProjection::Index(Some(index)),
                    ) = (*result, source)
                    {
                        *result = LayoutBackingProjection::Index(Some(index));
                    }
                }
                BorrowResult {
                    kind: template.kind,
                    projection,
                }
            })
            .collect::<Vec<_>>();
        if retain_symbolic_fallback {
            out.push(template.clone());
        }
        if !retain_symbolic_fallback
            && layout_results
                .iter()
                .any(|result| result.projection != template.projection)
            && !layout_results
                .iter()
                .any(|result| result.projection == template.projection)
        {
            out.retain(|result| result.projection != template.projection);
        }
        out.extend(layout_results);
        if out.is_empty() {
            out.push(template.clone());
        }
        out.sort_unstable();
        out.dedup();
        out
    }

    pub(super) fn assign_held_loans(&mut self, local: SLocalId, held: HeldLoans) {
        self.definite_overrides.remove(&local);
        if held.is_empty() {
            self.local_loans.remove(&local);
        } else {
            self.local_loans.insert(local, held);
        }
    }

    pub(super) fn projected_held_loans(
        &self,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
    ) -> HeldLoans {
        let mut out = HeldLoans::default();
        for (path, loans) in self.local_loans.get(&local).into_iter().flatten() {
            let (projected, stored, query) = if layout_path_is_prefix(projection, path) {
                (
                    path[projection.len()..].to_vec(),
                    &path[..projection.len()],
                    projection,
                )
            } else if layout_path_is_prefix(path, projection) {
                (
                    HeldLoanPath::new(),
                    path.as_slice(),
                    &projection[..path.len()],
                )
            } else {
                continue;
            };
            out.entry(projected).or_default().extend(
                loans
                    .iter()
                    .filter_map(|loan| loan.with_match_bindings(stored, query)),
            );
        }
        out
    }

    fn path_is_definite_override(&self, local: SLocalId, path: &[LayoutBackingProjection]) -> bool {
        self.definite_overrides
            .get(&local)
            .is_some_and(|overrides| {
                overrides
                    .iter()
                    .any(|override_| layout_path_is_prefix(override_, path))
            })
    }

    fn has_nondefinite_exact_match(
        &self,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
    ) -> bool {
        self.local_loans
            .get(&local)
            .into_iter()
            .flatten()
            .any(|(path, loans)| {
                !loans.is_empty()
                    && path
                        .iter()
                        .any(|step| matches!(step, LayoutBackingProjection::Index(Some(_))))
                    && (layout_path_is_prefix(path, projection)
                        || layout_path_is_prefix(projection, path))
                    && !self.path_is_definite_override(local, path)
            })
    }

    fn deepest_held_loan_matches(
        &self,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
    ) -> Vec<(HeldLoanPath, FxHashSet<HeldLoan>)> {
        let dynamic_query = projection.contains(&LayoutBackingProjection::Index(None));
        let mut best = None;
        let mut matches = Vec::new();
        for (path, loans) in self.local_loans.get(&local).into_iter().flatten() {
            if !layout_path_is_prefix(path, projection) {
                continue;
            }
            let bound = loans
                .iter()
                .filter_map(|loan| loan.with_match_bindings(path, &projection[..path.len()]))
                .collect::<FxHashSet<_>>();
            if bound.is_empty() {
                continue;
            }
            let specificity = if dynamic_query {
                0
            } else {
                path.iter()
                    .copied()
                    .zip(projection.iter().copied())
                    .map(|(stored, query)| match (stored, query) {
                        (stored, query) if stored == query => 3,
                        (
                            LayoutBackingProjection::IndexFamily(_),
                            LayoutBackingProjection::Index(Some(_))
                            | LayoutBackingProjection::IndexFamily(_),
                        ) => 2,
                        (
                            LayoutBackingProjection::Index(None),
                            LayoutBackingProjection::Index(Some(_))
                            | LayoutBackingProjection::IndexFamily(_),
                        ) => 1,
                        _ => 0,
                    })
                    .sum()
            };
            let rank = (path.len(), specificity);
            match best {
                Some(best_rank) if rank < best_rank => continue,
                Some(best_rank) if rank > best_rank => {
                    best = Some(rank);
                    matches.clear();
                }
                None => best = Some(rank),
                Some(_) => {}
            }
            matches.push((path.clone(), bound));
        }
        matches
    }

    pub(super) fn replace_held_loans(
        &mut self,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
        replacement: HeldLoans,
    ) {
        let mut definite_overrides = self
            .definite_overrides
            .get(&local)
            .cloned()
            .unwrap_or_default();
        let mut held = self.held_loans_in(local);
        if !projection.contains(&LayoutBackingProjection::Index(None)) {
            held.retain(|path, _| {
                !layout_path_is_prefix(projection, path)
                    || projection.iter().copied().zip(path.iter().copied()).any(
                        |(written, held)| {
                            matches!(
                                (written, held),
                                (
                                    LayoutBackingProjection::Index(Some(_)),
                                    LayoutBackingProjection::Index(None)
                                        | LayoutBackingProjection::IndexFamily(_)
                                )
                            )
                        },
                    )
            });
        }
        merge_held_loans(&mut held, prefixed_held_loans(replacement, projection));
        self.assign_held_loans(local, held);
        if !projection.contains(&LayoutBackingProjection::Index(None)) {
            definite_overrides.insert(projection.to_vec());
        }
        if !definite_overrides.is_empty() {
            self.definite_overrides.insert(local, definite_overrides);
        }
    }
}

impl JoinSemiLattice for State {
    fn join_into(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (local, held) in &other.local_loans {
            let entry = self.local_loans.entry(*local).or_default();
            for (path, loans) in held {
                let entry = entry.entry(path.clone()).or_default();
                let before = entry.len();
                entry.extend(loans.iter().cloned());
                changed |= before != entry.len();
            }
        }
        self.definite_overrides.retain(|local, overrides| {
            let Some(other_overrides) = other.definite_overrides.get(local) else {
                changed = true;
                return false;
            };
            let before = overrides.len();
            overrides.retain(|path| other_overrides.contains(path));
            changed |= before != overrides.len();
            !overrides.is_empty()
        });
        changed
    }
}

pub(super) struct BorrowCanonCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &'a NormalizedSemanticBody<'db>,
    loans: &'a [Loan<'db>],
    loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
    constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
}

impl<'a, 'db> BorrowCanonCx<'a, 'db> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedSemanticBody<'db>,
        loans: &'a [Loan<'db>],
        loan_for_local: &'a FxHashMap<SLocalId, LoanId>,
        constant_indices: &'a SecondaryMap<SLocalId, Option<usize>>,
    ) -> Self {
        Self {
            db,
            instance,
            body,
            loans,
            loan_for_local,
            constant_indices,
        }
    }

    fn materialize_constant_indices(&self, path: &NSProjectionPath<'db>) -> NSProjectionPath<'db> {
        let mut out = NSProjectionPath::new();
        for projection in path.iter() {
            out.push(match projection {
                Projection::Index(IndexSource::Dynamic(index))
                    if let Some(index) = self.constant_indices[*index] =>
                {
                    Projection::Index(IndexSource::Constant(index))
                }
                projection => projection.clone(),
            });
        }
        out
    }

    fn layout_path(&self, path: &NSProjectionPath<'db>) -> Option<Vec<LayoutBackingProjection>> {
        layout_path_for_semantic_projection(&self.materialize_constant_indices(path))
    }

    pub(super) fn targets_for_held(&self, held: &HeldLoan) -> FxHashSet<CanonPlace<'db>> {
        let loan = &self.loans[held.id.0 as usize];
        let mut matching =
            loan.indexed_targets
                .iter()
                .filter(|indexed| {
                    indexed.bindings.iter().all(|(family, expected)| {
                        binding_accepts(&held.bindings, *family, *expected)
                    })
                })
                .collect::<Vec<_>>();
        let has_exact_override = matching.iter().any(|indexed| {
            !indexed.fallback
                && indexed.shadows_fallback
                && !indexed.bindings.is_empty()
                && indexed.bindings.iter().all(|(family, expected)| {
                    resolved_family_binding(&held.bindings, *family) == Some(*expected)
                })
        });
        let mut out = loan
            .unconditional_targets
            .iter()
            .map(|target| CanonPlace {
                root: target.root.clone(),
                proj: instantiate_canon_projection(&target.proj, &held.bindings),
            })
            .collect::<FxHashSet<_>>();
        out.extend(
            matching
                .drain(..)
                .filter(|indexed| !indexed.fallback || !has_exact_override)
                .map(|indexed| CanonPlace {
                    root: indexed.target.root.clone(),
                    proj: instantiate_canon_projection(&indexed.target.proj, &held.bindings),
                }),
        );
        out
    }

    pub(super) fn active_targets_for_held(
        &self,
        state: &State,
        local: SLocalId,
        path: &[LayoutBackingProjection],
        held: &HeldLoan,
    ) -> LoanRegion<'db> {
        let targets = self.targets_for_held(held);
        let exclusions = state
            .definite_overrides
            .get(&local)
            .into_iter()
            .flatten()
            .filter(|override_| {
                layout_path_is_prefix(override_, path)
                    && override_.iter().copied().zip(path.iter().copied()).any(
                        |(override_, stored)| {
                            matches!(
                                (override_, stored),
                                (
                                    LayoutBackingProjection::Index(Some(_)),
                                    LayoutBackingProjection::IndexFamily(_)
                                )
                            )
                        },
                    )
            })
            .filter_map(|override_| held.with_match_bindings(path, override_))
            .flat_map(|held| self.targets_for_held(&held))
            .collect();
        LoanRegion {
            targets,
            exclusions,
        }
    }

    fn deepest_held_projection_targets(
        &self,
        state: &State,
        local: SLocalId,
        path: &NSProjectionPath<'db>,
    ) -> Option<FxHashSet<CanonPlace<'db>>> {
        let path = self.materialize_constant_indices(path);
        let projection = self.layout_path(&path)?;
        let matches = state.deepest_held_loan_matches(local, &projection);
        let depth = matches.first()?.0.len();
        let mut consumed = 0;
        let mut suffix = NSProjectionPath::default();
        for projection in path.iter() {
            if consumed < depth {
                if !matches!(projection, Projection::Deref) {
                    consumed += 1;
                }
                continue;
            }
            if suffix.is_empty() && matches!(projection, Projection::Deref) {
                continue;
            }
            suffix.push(projection.clone());
        }
        let suffix = canon_projection_from_semantic(&suffix);
        let mut targets = FxHashSet::default();
        for (path, loans) in matches {
            for held in loans {
                targets.extend(
                    self.active_targets_for_held(state, local, &path, &held)
                        .active_targets()
                        .into_iter()
                        .map(|target| CanonPlace {
                            root: target.root,
                            proj: target.proj.concat(&suffix),
                        }),
                );
            }
        }
        Some(targets)
    }

    pub(super) fn apply_stmt_state_with_call_loans(
        &self,
        state: &mut State,
        stmt: &NSStmt<'db>,
        call_result_loans: Option<&[(BorrowResult, LoanId)]>,
    ) {
        match &stmt.kind {
            NSStmtKind::Assign { dst, expr } => {
                let propagated_definite_overrides = match expr {
                    NExpr::Use(src) => state
                        .definite_overrides
                        .get(&src.local)
                        .cloned()
                        .unwrap_or_default(),
                    _ => FxHashSet::default(),
                };
                let held = match expr {
                    NExpr::Use(src) => {
                        let own = self.own_held_loan_for_local(*dst);
                        if own.is_empty() {
                            self.propagated_held_loans(*dst, state.held_loans_in(src.local))
                        } else {
                            own
                        }
                    }
                    NExpr::Borrow { .. } => self.own_held_loan_for_local(*dst),
                    NExpr::Call { args, .. } => {
                        let own = self.own_held_loan_for_local(*dst);
                        if !own.is_empty() {
                            own
                        } else if let Some(call_result_loans) = call_result_loans {
                            let mut held = HeldLoans::default();
                            for (result, loan) in call_result_loans {
                                held.entry(result.projection.clone())
                                    .or_default()
                                    .insert(HeldLoan::new(*loan));
                            }
                            held
                        } else {
                            let mut held = HeldLoans::default();
                            for arg in args {
                                merge_held_loans(&mut held, state.held_loans_in(arg.local));
                            }
                            self.propagated_held_loans(*dst, held)
                        }
                    }
                    NExpr::AggregateMake { ty, fields } => {
                        let mut held = HeldLoans::default();
                        for (idx, field) in fields.iter().enumerate() {
                            let projection = if ty.is_array(self.db) {
                                LayoutBackingProjection::Index(Some(idx))
                            } else {
                                let Ok(idx) = u16::try_from(idx) else {
                                    continue;
                                };
                                LayoutBackingProjection::Field(FieldIndex(idx))
                            };
                            merge_held_loans(
                                &mut held,
                                prefixed_held_loans(
                                    state.held_loans_in(field.local),
                                    &[projection],
                                ),
                            );
                        }
                        held
                    }
                    NExpr::EnumMake {
                        variant, fields, ..
                    } => {
                        let mut held = HeldLoans::default();
                        for (idx, field) in fields.iter().enumerate() {
                            let Ok(field_idx) = u16::try_from(idx) else {
                                continue;
                            };
                            merge_held_loans(
                                &mut held,
                                prefixed_held_loans(
                                    state.held_loans_in(field.local),
                                    &[LayoutBackingProjection::VariantField {
                                        variant: *variant,
                                        field: FieldIndex(field_idx),
                                    }],
                                ),
                            );
                        }
                        held
                    }
                    NExpr::ArrayRepeat { value, .. } => prefixed_held_loans(
                        state.held_loans_in(value.local),
                        &[LayoutBackingProjection::Index(None)],
                    ),
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => {
                        let held = state.projected_held_loans(
                            value.local,
                            &[LayoutBackingProjection::VariantField {
                                variant: *variant,
                                field: *field,
                            }],
                        );
                        self.propagated_held_loans(*dst, held)
                    }
                    NExpr::ReadPlace { place, .. } => {
                        let own = self.own_held_loan_for_local(*dst);
                        if own.is_empty() {
                            self.place_base_local(place)
                                .map(|base| {
                                    let projection =
                                        self.layout_path(&place.path).unwrap_or_default();
                                    self.propagated_held_loans(
                                        *dst,
                                        state.projected_held_loans(base, &projection),
                                    )
                                })
                                .unwrap_or_default()
                        } else {
                            own
                        }
                    }
                    _ => HeldLoans::default(),
                };
                state.assign_held_loans(*dst, held);
                if !propagated_definite_overrides.is_empty() {
                    state
                        .definite_overrides
                        .insert(*dst, propagated_definite_overrides);
                }
            }
            NSStmtKind::Store { dst, src } => {
                if let NSPlaceRoot::Root(root) = dst.root
                    && let Some(base) = self.root_base_local(root)
                {
                    let path = self.materialize_constant_indices(&dst.path);
                    if self
                        .body
                        .place_root_ty(&dst.root)
                        .and_then(|ty| semantic_projection_ty(self.db, ty, &path))
                        .is_some_and(|(_, traverses_capability)| traverses_capability)
                    {
                        return;
                    }
                    let projection = layout_path_for_semantic_projection(&path).unwrap_or_default();
                    state.replace_held_loans(base, &projection, state.held_loans_in(src.local));
                }
            }
        }
    }

    fn propagated_held_loans(&self, dst: SLocalId, held: HeldLoans) -> HeldLoans {
        if held.is_empty() {
            return held;
        }
        let Some(dst_local) = self.body.local(dst) else {
            return HeldLoans::default();
        };
        if dst_local.ty.as_capability(self.db).is_some() || ty_is_noesc(self.db, dst_local.ty) {
            held
        } else {
            HeldLoans::default()
        }
    }

    fn own_held_loan_for_local(&self, local: SLocalId) -> HeldLoans {
        self.loan_for_local
            .get(&local)
            .copied()
            .map(|loan| {
                FxHashMap::from_iter([(
                    HeldLoanPath::new(),
                    FxHashSet::from_iter([HeldLoan::new(loan)]),
                )])
            })
            .unwrap_or_default()
    }

    fn root_base_local(&self, root: NBorrowRootId) -> Option<SLocalId> {
        match self.body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => Some(*local),
            NBorrowRoot::Provider { .. } => None,
        }
    }

    fn place_base_local(&self, place: &NSPlace<'db>) -> Option<SLocalId> {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
            NSPlaceRoot::Root(root) => self.root_base_local(root),
        }
    }

    pub(super) fn canonicalize_value_base(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        if self
            .body
            .local(local)
            .is_some_and(|local| local.ty.as_borrow(self.db).is_some())
        {
            return self.borrow_local_targets(state, local);
        }

        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        let root = match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root))),
            NormalizedBindingLowering::Erased => None,
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => unreachable!(),
        };
        root.into_iter()
            .map(|root| CanonPlace {
                root,
                proj: CanonProjectionPath::default(),
            })
            .collect()
    }

    pub(super) fn canonicalize_value_projection(
        &self,
        state: &State,
        local: SLocalId,
        projection: &NSProjectionPath<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        let projection = self.materialize_constant_indices(projection);
        let traverses_capability = semantic_projection_ty(self.db, local_data.ty, &projection)
            .is_none_or(|(_, traverses_capability)| traverses_capability);
        if traverses_capability
            && let Some(mut targets) =
                self.deepest_held_projection_targets(state, local, &projection)
        {
            if let Some(layout_projection) = self.layout_path(&projection)
                && state.has_nondefinite_exact_match(local, &layout_projection)
            {
                targets.extend(self.canonicalize_value_symbolic_fallback_projection(
                    state,
                    local,
                    local_data,
                    &layout_projection,
                ));
            }
            return targets;
        }
        if local_data.ty.as_borrow(self.db).is_none() {
            let resolved =
                resolved_layout_backing_places(local_data.layout_backing_sources(), &projection);
            if !resolved.is_empty() {
                return resolved
                    .iter()
                    .flat_map(|place| self.canonicalize_place_targets(state, place))
                    .collect();
            }
        }
        if local_data.ty.as_borrow(self.db).is_none()
            && ty_is_noesc(self.db, local_data.ty)
            && !matches!(
                local_data.source,
                Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
            )
            && traverses_capability
        {
            return FxHashSet::default();
        }

        self.canonicalize_value_base(state, local)
            .into_iter()
            .map(|base| CanonPlace {
                root: base.root,
                proj: base
                    .proj
                    .concat(&canon_projection_from_semantic(&projection)),
            })
            .collect()
    }

    pub(super) fn canonicalize_value_layout_projection(
        &self,
        state: &State,
        local: SLocalId,
        projection: &[LayoutBackingProjection],
    ) -> FxHashSet<CanonPlace<'db>> {
        let Some(local_ty) = self.body.local(local).map(|local| local.ty) else {
            return FxHashSet::default();
        };
        if let Some(projection) = semantic_projection_for_layout_path(self.db, local_ty, projection)
        {
            return self.canonicalize_value_projection(state, local, &projection);
        }
        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        let mut held_targets = FxHashSet::default();
        for (path, loans) in state.deepest_held_loan_matches(local, projection) {
            for loan in loans {
                held_targets.extend(
                    self.active_targets_for_held(state, local, &path, &loan)
                        .active_targets(),
                );
            }
        }
        let layout_backing_targets = if state.path_is_definite_override(local, projection) {
            FxHashSet::default()
        } else {
            self.canonicalize_value_symbolic_fallback_projection(
                state, local, local_data, projection,
            )
        };
        if !held_targets.is_empty() || !layout_backing_targets.is_empty() {
            let mut targets = held_targets;
            targets.extend(layout_backing_targets);
            return targets;
        }
        let Some(suffix) = canon_projection_for_layout_path(self.db, local_ty, projection) else {
            return self.borrow_local_targets(state, local);
        };
        if local_data.ty.as_borrow(self.db).is_none()
            && ty_is_noesc(self.db, local_data.ty)
            && !matches!(
                local_data.source,
                Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
            )
        {
            return FxHashSet::default();
        }
        self.canonicalize_value_base(state, local)
            .into_iter()
            .map(|base| CanonPlace {
                root: base.root,
                proj: base.proj.concat(&suffix),
            })
            .collect()
    }

    fn canonicalize_value_layout_backing_projections(
        &self,
        state: &State,
        local_data: &super::ir::NSLocal<'db>,
        projection: &[LayoutBackingProjection],
    ) -> Vec<(FamilyBindings, CanonPlace<'db>)> {
        let mut targets = local_data
            .layout_backing_sources()
            .iter()
            .filter_map(|source| {
                let mut bindings = FamilyBindings::new();
                if source.target.len() > projection.len()
                    || !source
                        .target
                        .iter()
                        .copied()
                        .zip(projection.iter().copied())
                        .all(|(source, result)| {
                            if !layout_projection_matches(source, result) {
                                return false;
                            }
                            match (source, result) {
                                (
                                    exact @ LayoutBackingProjection::Index(Some(_)),
                                    LayoutBackingProjection::IndexFamily(family),
                                ) => insert_family_binding(&mut bindings, family, exact),
                                (
                                    LayoutBackingProjection::IndexFamily(source),
                                    LayoutBackingProjection::IndexFamily(result),
                                ) if source != result => insert_family_binding(
                                    &mut bindings,
                                    result,
                                    LayoutBackingProjection::IndexFamily(source),
                                ),
                                _ => true,
                            }
                        })
                {
                    return None;
                }
                self.body
                    .place_ty(self.db, &source.source)
                    .map(|source_ty| (source, source_ty, bindings))
            })
            .flat_map(|(source, source_ty, bindings)| {
                self.canonicalize_layout_backing_source_projection(
                    state,
                    &source.source,
                    source_ty,
                    &projection[source.target.len()..],
                )
                .into_iter()
                .map(move |target| (bindings.clone(), target))
            })
            .collect::<Vec<_>>();
        let mut seen = FxHashSet::default();
        targets.retain(|target| seen.insert(target.clone()));
        targets
    }

    fn canonicalize_value_symbolic_fallback_projections(
        &self,
        state: &State,
        local: SLocalId,
        local_data: &super::ir::NSLocal<'db>,
        projection: &[LayoutBackingProjection],
    ) -> Vec<(FamilyBindings, CanonPlace<'db>)> {
        let mut targets =
            self.canonicalize_value_layout_backing_projections(state, local_data, projection);
        if matches!(
            local_data.source,
            Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
        ) && local_data.ty.as_borrow(self.db).is_none()
            && let Some(suffix) =
                canon_projection_for_layout_path(self.db, local_data.ty, projection)
        {
            targets.extend(
                self.canonicalize_value_base(state, local)
                    .into_iter()
                    .map(|base| {
                        (
                            FamilyBindings::new(),
                            CanonPlace {
                                root: base.root,
                                proj: base.proj.concat(&suffix),
                            },
                        )
                    }),
            );
        }
        let mut seen = FxHashSet::default();
        targets.retain(|target| seen.insert(target.clone()));
        targets
    }

    fn canonicalize_value_symbolic_fallback_projection(
        &self,
        state: &State,
        local: SLocalId,
        local_data: &super::ir::NSLocal<'db>,
        projection: &[LayoutBackingProjection],
    ) -> FxHashSet<CanonPlace<'db>> {
        self.canonicalize_value_symbolic_fallback_projections(state, local, local_data, projection)
            .into_iter()
            .map(|(_, target)| target)
            .collect()
    }

    fn canonicalize_layout_backing_source_projection(
        &self,
        state: &State,
        source: &NSPlace<'db>,
        source_ty: TyId<'db>,
        projection: &[LayoutBackingProjection],
    ) -> FxHashSet<CanonPlace<'db>> {
        let Some(suffix) = canon_projection_for_layout_path(self.db, source_ty, projection) else {
            return self.canonicalize_place_targets(state, source);
        };
        self.canonicalize_place_targets(state, source)
            .into_iter()
            .map(|base| CanonPlace {
                root: base.root,
                proj: base.proj.concat(&suffix),
            })
            .collect()
    }

    pub(super) fn canonicalize_call_input_with_families(
        &self,
        state: &State,
        arg: SLocalId,
        input: &super::ir::BorrowInput,
    ) -> CanonicalizedCallInput<'db> {
        let mut targets = match input {
            super::ir::BorrowInput::Place { projection, .. } => {
                self.canonicalize_value_layout_projection(state, arg, projection)
            }
            super::ir::BorrowInput::AnyInParam(_) => self.all_value_targets(state, arg),
        };
        let mut unconditional_targets = targets.clone();
        let mut indexed_targets = Vec::new();

        if let super::ir::BorrowInput::Place { projection, .. } = input
            && projection
                .iter()
                .any(|step| matches!(step, LayoutBackingProjection::IndexFamily(_)))
        {
            let matches = state.deepest_held_loan_matches(arg, projection);
            if !matches.is_empty() {
                let layout_backing_targets = self
                    .body
                    .local(arg)
                    .map(|local| {
                        self.canonicalize_value_symbolic_fallback_projections(
                            state, arg, local, projection,
                        )
                    })
                    .unwrap_or_default();
                unconditional_targets.clear();
                for (stored, held_loans) in matches {
                    let mut query_bindings = FamilyBindings::new();
                    for (&query, &stored) in projection.iter().zip(&stored) {
                        if let (
                            LayoutBackingProjection::IndexFamily(family),
                            LayoutBackingProjection::Index(Some(index)),
                        ) = (query, stored)
                        {
                            insert_family_binding(
                                &mut query_bindings,
                                family,
                                LayoutBackingProjection::Index(Some(index)),
                            );
                        }
                    }
                    for held in held_loans {
                        let loan = &self.loans[held.id.0 as usize];
                        for target in &loan.unconditional_targets {
                            let target = CanonPlace {
                                root: target.root.clone(),
                                proj: instantiate_canon_projection(&target.proj, &held.bindings),
                            };
                            if query_bindings.is_empty() {
                                unconditional_targets.insert(target);
                            } else {
                                indexed_targets.push(IndexedLoanTarget {
                                    bindings: query_bindings.clone(),
                                    fallback: false,
                                    shadows_fallback: state.path_is_definite_override(arg, &stored),
                                    target,
                                });
                            }
                        }
                        for indexed in &loan.indexed_targets {
                            let Some(mut indexed) =
                                remap_indexed_target(indexed, &held.bindings, &query_bindings)
                            else {
                                continue;
                            };
                            if !query_bindings.is_empty()
                                && !state.path_is_definite_override(arg, &stored)
                            {
                                indexed.shadows_fallback = false;
                            }
                            if indexed.bindings.is_empty() && !indexed.fallback {
                                unconditional_targets.insert(indexed.target);
                            } else {
                                indexed_targets.push(indexed);
                            }
                        }
                    }
                }
                indexed_targets.extend(layout_backing_targets.into_iter().map(
                    |(bindings, target)| IndexedLoanTarget {
                        bindings,
                        fallback: true,
                        shadows_fallback: false,
                        target,
                    },
                ));
                let mut seen = FxHashSet::default();
                indexed_targets.retain(|indexed| seen.insert(indexed.clone()));
                targets = unconditional_targets.clone();
                targets.extend(indexed_targets.iter().map(|indexed| indexed.target.clone()));
            }
        }
        CanonicalizedCallInput {
            targets,
            unconditional_targets,
            indexed_targets,
        }
    }

    pub(super) fn canonicalize_place_layout_projection(
        &self,
        state: &State,
        place: &NSPlace<'db>,
        target_ty: TyId<'db>,
        projection: &[LayoutBackingProjection],
    ) -> FxHashSet<CanonPlace<'db>> {
        if let Some(suffix) = semantic_projection_for_layout_path(self.db, target_ty, projection) {
            if let Some(local) = self.place_base_local(place) {
                let path = self
                    .materialize_constant_indices(&place.path)
                    .concat(&suffix);
                return self.canonicalize_value_projection(state, local, &path);
            }
            let mut projected = place.clone();
            projected.path = projected.path.concat(&suffix);
            return self.canonicalize_place_targets(state, &projected);
        }

        if let Some(local) = self.place_base_local(place) {
            let mut path = self
                .layout_path(&self.materialize_constant_indices(&place.path))
                .unwrap_or_default();
            path.extend_from_slice(projection);
            let targets = state
                .projected_held_loans(local, &path)
                .into_values()
                .flatten()
                .flat_map(|loan| self.targets_for_held(&loan))
                .collect::<FxHashSet<_>>();
            if !targets.is_empty() {
                return targets;
            }
            if self.body.local(local).is_some_and(|local| {
                ty_is_noesc(self.db, local.ty)
                    && !matches!(
                        local.source,
                        Some(LocalBinding::Param { .. } | LocalBinding::EffectParam { .. })
                    )
            }) {
                return FxHashSet::default();
            }
        }

        self.canonicalize_place_targets(state, place)
    }

    pub(super) fn borrow_local_targets(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        let Some(local_data) = self.body.local(local) else {
            return FxHashSet::default();
        };
        let held_loans = if local_data.ty.as_borrow(self.db).is_some() {
            state
                .deepest_held_loan_matches(local, &[])
                .into_iter()
                .flat_map(|(_, loans)| loans)
                .collect::<Vec<_>>()
        } else {
            state
                .local_loans
                .get(&local)
                .into_iter()
                .flat_map(|held| held.values())
                .flatten()
                .cloned()
                .collect()
        };
        let has_tracked_loan = !held_loans.is_empty();
        let mut out = FxHashSet::default();
        for held in held_loans {
            out.extend(self.targets_for_held(&held));
        }
        if !out.is_empty() || has_tracked_loan {
            return out;
        }

        if local_data.ty.as_borrow(self.db).is_none() && ty_is_noesc(self.db, local_data.ty) {
            let targets = local_data
                .layout_backing_sources()
                .iter()
                .flat_map(|source| self.canonicalize_place_targets(state, &source.source))
                .collect::<FxHashSet<_>>();
            if !targets.is_empty() {
                return targets;
            }
        }

        if let Some(place) = local_data.lowering.place() {
            return self.canonicalize_place_targets(state, place);
        }
        match &local_data.lowering {
            NormalizedBindingLowering::CarrierLocal { root, provider, .. } => provider
                .clone()
                .map(BorrowRoot::Provider)
                .or_else(|| root.and_then(|root| self.root_to_borrow_root(root)))
                .into_iter()
                .map(|root| CanonPlace {
                    root,
                    proj: CanonProjectionPath::default(),
                })
                .collect(),
            NormalizedBindingLowering::Erased => FxHashSet::default(),
            NormalizedBindingLowering::ValueLocal { .. }
            | NormalizedBindingLowering::PlaceBoundValue { .. } => FxHashSet::default(),
        }
    }

    pub(super) fn all_value_targets(
        &self,
        state: &State,
        local: SLocalId,
    ) -> FxHashSet<CanonPlace<'db>> {
        let mut out =
            self.canonicalize_value_projection(state, local, &NSProjectionPath::default());
        out.extend(self.borrow_local_targets(state, local));
        out
    }

    pub(super) fn canonicalize_place(
        &self,
        state: &State,
        place: &NSPlace<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashSet<CanonPlace<'db>>, SemanticBorrowDiagnostic<'db>> {
        let out = self.canonicalize_place_targets(state, place);
        if out.is_empty() {
            return Err(self.internal_diag(
                origin,
                "cannot canonicalize carrier-rooted place".to_string(),
            ));
        }
        Ok(out)
    }

    fn canonicalize_place_targets(
        &self,
        state: &State,
        place: &NSPlace<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        let path = canon_projection_from_semantic(&self.materialize_constant_indices(&place.path));
        match place.root {
            NSPlaceRoot::Root(root) => {
                let borrow_root = self
                    .root_to_borrow_root(root)
                    .expect("normalized borrow root");
                FxHashSet::from_iter([CanonPlace {
                    root: borrow_root,
                    proj: path,
                }])
            }
            NSPlaceRoot::CarrierDerefLocal(local) => {
                let suffix = path;
                let mut out = FxHashSet::default();
                let mut resolved = false;
                for loan in state.loans_in(local) {
                    resolved = true;
                    for target in &self.loans[loan.0 as usize].targets {
                        out.insert(CanonPlace {
                            root: target.root.clone(),
                            proj: target.proj.concat(&suffix),
                        });
                    }
                }
                if !resolved
                    && let Some(NormalizedBindingLowering::CarrierLocal { root, provider, .. }) =
                        self.body.local(local).map(|local| &local.lowering)
                {
                    if let Some(provider) = provider {
                        out.insert(CanonPlace {
                            root: BorrowRoot::Provider(provider.clone()),
                            proj: suffix.clone(),
                        });
                    } else if let Some(root) = root.and_then(|root| self.root_to_borrow_root(root))
                    {
                        out.insert(CanonPlace { root, proj: suffix });
                    }
                }
                out
            }
        }
    }

    pub(super) fn root_to_borrow_root(&self, root: NBorrowRootId) -> Option<BorrowRoot<'db>> {
        match self.body.root(root)? {
            NBorrowRoot::Param { param_idx, .. } => Some(BorrowRoot::Param(*param_idx)),
            NBorrowRoot::LocalSlot { local } => Some(BorrowRoot::Local(*local)),
            NBorrowRoot::Provider { binding, .. } => Some(BorrowRoot::Provider(binding.clone())),
        }
    }

    pub(super) fn mut_loans_for_place(
        &self,
        state: &State,
        place: &NSPlace<'db>,
    ) -> FxHashSet<LoanId> {
        self.loans_for_place(state, place)
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn mut_loans_for_place_targets(
        &self,
        state: &State,
        place: &NSPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> FxHashSet<LoanId> {
        self.loans_for_place_targets(state, place, targets)
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn loans_for_place_targets(
        &self,
        state: &State,
        place: &NSPlace<'db>,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> FxHashSet<LoanId> {
        self.place_base_local(place).map_or_else(
            || self.loans_for_place(state, place),
            |local| self.loans_for_value_targets(state, local, targets),
        )
    }

    pub(super) fn loans_for_place(&self, state: &State, place: &NSPlace<'db>) -> FxHashSet<LoanId> {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => state.loans_in(local),
            NSPlaceRoot::Root(_) => FxHashSet::default(),
        }
    }

    pub(super) fn mut_loans_for_value(&self, state: &State, local: SLocalId) -> FxHashSet<LoanId> {
        state
            .loans_in(local)
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn mut_loans_for_value_targets(
        &self,
        state: &State,
        local: SLocalId,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> FxHashSet<LoanId> {
        self.loans_for_value_targets(state, local, targets)
            .into_iter()
            .filter(|loan| self.loans[loan.0 as usize].kind == BorrowKind::Mut)
            .collect()
    }

    pub(super) fn loans_for_value_targets(
        &self,
        state: &State,
        local: SLocalId,
        targets: &FxHashSet<CanonPlace<'db>>,
    ) -> FxHashSet<LoanId> {
        state
            .loans_in(local)
            .into_iter()
            .filter(|loan| {
                let loan = &self.loans[loan.0 as usize];
                place_set_overlaps(&loan.targets, targets)
            })
            .collect()
    }

    fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        normalized_body_internal_diag(self.db, self.instance, self.body, origin, message)
    }
}

pub(super) fn place_set_overlaps<'db>(
    lhs: &FxHashSet<CanonPlace<'db>>,
    rhs: &FxHashSet<CanonPlace<'db>>,
) -> bool {
    lhs.iter()
        .any(|lhs| rhs.iter().any(|rhs| places_overlap(lhs, rhs)))
}

pub(super) fn places_overlap<'db>(lhs: &CanonPlace<'db>, rhs: &CanonPlace<'db>) -> bool {
    lhs.root == rhs.root && !matches!(lhs.proj.may_alias(&rhs.proj), Aliasing::No)
}
