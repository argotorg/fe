//! Physical access ranges, separate from address identity and typed coverage.
use common::layout::enum_tag_bits;

use super::{
    external::{ExternalOrigin, MemoryOffset},
    guard::Guard,
    handle::HandleAddressSpace,
    index::{IndexExpr, IndexSubst},
    path::Projection,
    region::{
        OverlapResult, RegionRoot, RegionSet, SymbolicPlace, open_clause_pair, path_alias_guard,
    },
    value::Guarded,
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::runtime_size_bytes,
    ty::{ProviderAddressSpace, ty_def::TyId},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessExtent<'db> {
    /// The selected semantic type's representation, not its capability shape.
    Typed,
    /// Linear-memory bytes. Unknown scalar values never denote an empty range.
    Bytes(IndexExpr<'db>),
    /// No bound is known within the compatible address space.
    Unknown,
}

impl<'db> AccessExtent<'db> {
    pub fn indices(self) -> impl Iterator<Item = IndexExpr<'db>> {
        match self {
            Self::Bytes(index) => Some(index),
            Self::Typed | Self::Unknown => None,
        }
        .into_iter()
    }

    pub fn substitute(self, subst: &IndexSubst<'db>) -> Self {
        match self {
            Self::Bytes(index) => Self::Bytes(subst.apply(index)),
            Self::Typed | Self::Unknown => self,
        }
    }
}

#[derive(Clone, Copy)]
pub struct AccessFootprint<'a, 'db> {
    pub region: &'a RegionSet<'db>,
    pub extent: AccessExtent<'db>,
}

impl<'a, 'db> AccessFootprint<'a, 'db> {
    pub fn typed(region: &'a RegionSet<'db>) -> Self {
        Self {
            region,
            extent: AccessExtent::Typed,
        }
    }

    pub fn overlap(self, db: &'db dyn HirAnalysisDb, other: Self) -> OverlapResult<'db> {
        let mut clauses = Vec::new();
        for (common, uncertain) in self.intersections(db, other) {
            // One uncertain pair determines this result. Constructing the union
            // of every remaining pair cannot provide any additional precision.
            if uncertain {
                return OverlapResult::Unknown;
            }
            clauses.extend(common.clauses().iter().cloned());
        }
        let region = RegionSet::new(self.region.scope(), clauses);
        if region.is_empty() {
            OverlapResult::Disjoint
        } else {
            OverlapResult::Overlap(region)
        }
    }

    pub fn intersect(self, db: &'db dyn HirAnalysisDb, other: Self) -> (RegionSet<'db>, bool) {
        let mut clauses = Vec::new();
        let mut uncertain = false;
        for (common, unknown) in self.intersections(db, other) {
            clauses.extend(common.clauses().iter().cloned());
            uncertain |= unknown;
        }
        (RegionSet::new(self.region.scope(), clauses), uncertain)
    }

    fn intersections(
        self,
        db: &'db dyn HirAnalysisDb,
        other: Self,
    ) -> impl Iterator<Item = (RegionSet<'db>, bool)> {
        let scope = self.region.scope();
        assert_eq!(scope, other.region.scope(), "footprint scopes must match");
        self.region.clauses().iter().flat_map(move |left| {
            other.region.clauses().iter().filter_map(move |right| {
                let (left, right, left_subst, right_subst) = open_clause_pair(left, right, scope);
                let guard =
                    if self.extent == AccessExtent::Typed && other.extent == AccessExtent::Typed {
                        left.payload
                            .root
                            .alias_guard(&right.payload.root, left.guard.clone(), true)?
                            .and(&right.guard)?
                    } else {
                        left.guard.and(&right.guard)?
                    };
                let left_address = LinearAddress::new(db, &left.payload);
                let right_address = LinearAddress::new(db, &right.payload);
                let left_len = extent_length(
                    db,
                    self.extent.substitute(&left_subst),
                    &left.payload,
                    &left_address,
                    &guard,
                );
                let right_len = extent_length(
                    db,
                    other.extent.substitute(&right_subst),
                    &right.payload,
                    &right_address,
                    &guard,
                );
                if left_len == Some(0)
                    || right_len == Some(0)
                    || disjoint_ranges(&left_address, &right_address, left_len, right_len)
                {
                    return None;
                }
                if self.extent == AccessExtent::Typed && other.extent == AccessExtent::Typed {
                    // Structural separation is valid for typed subobjects. Raw
                    // byte spans may cross a field boundary and cannot use it.
                    Some(RegionSet::new(scope, [left]).intersect(&RegionSet::new(scope, [right])))
                } else {
                    let left_object = left_address
                        .as_ref()
                        .map_or(&left.payload.root, |address| &address.object);
                    let right_object = right_address
                        .as_ref()
                        .map_or(&right.payload.root, |address| &address.object);
                    let guard = left_object.byte_alias_guard(right_object, guard)?;
                    Some((
                        RegionSet::new(
                            scope,
                            [
                                Guarded {
                                    guard: guard.clone(),
                                    payload: left.payload,
                                },
                                Guarded {
                                    guard,
                                    payload: right.payload,
                                },
                            ],
                        ),
                        true,
                    ))
                }
            })
        })
    }
}

/// One access clause and one borrowed clause, opened in one scope.
pub struct FootprintPair<'db> {
    pub access: SymbolicPlace<'db>,
    pub borrowed: SymbolicPlace<'db>,
    /// The access extent in the pair's scope.
    pub extent: AccessExtent<'db>,
    /// Where the two may share a byte, using only physical separation.
    pub possible: Guard<'db>,
    /// Where they certainly share a byte, in the same scope.
    pub definite: Option<Guard<'db>>,
}

impl<'a, 'db> AccessFootprint<'a, 'db> {
    /// Classify every pair of this access with a typed borrowed region without
    /// the entry assumption that distinct certain sources are separate. Pairs
    /// separated physically, including by empty or disjoint constant ranges,
    /// are omitted.
    pub fn physical_pairs(
        self,
        db: &'db dyn HirAnalysisDb,
        borrowed: &RegionSet<'db>,
    ) -> Vec<FootprintPair<'db>> {
        let scope = self.region.scope();
        assert_eq!(scope, borrowed.scope(), "footprint scopes must match");
        let mut pairs = Vec::new();
        for left in self.region.clauses() {
            for right in borrowed.clauses() {
                let (left, right, left_subst, _) = open_clause_pair(left, right, scope);
                let Some(guard) = left.guard.and(&right.guard) else {
                    continue;
                };
                let extent = self.extent.substitute(&left_subst);
                let left_address = LinearAddress::new(db, &left.payload);
                let right_address = LinearAddress::new(db, &right.payload);
                let left_len = extent_length(db, extent, &left.payload, &left_address, &guard);
                let right_len = extent_length(
                    db,
                    AccessExtent::Typed,
                    &right.payload,
                    &right_address,
                    &guard,
                );
                if left_len == Some(0)
                    || right_len == Some(0)
                    || disjoint_ranges(&left_address, &right_address, left_len, right_len)
                {
                    continue;
                }
                // Paths separate only places of one typed object, in one
                // interpretation; objects that may overlap do so anywhere.
                let (left_place, right_place) = (&left.payload, &right.payload);
                let exact = left_place
                    .root
                    .alias_guard(&right_place.root, guard.clone(), false)
                    .filter(|_| left_place.views == right_place.views);
                let possible = if extent == AccessExtent::Typed {
                    let same = exact.clone().and_then(|exact| {
                        path_alias_guard(
                            left_place.path.as_slice(),
                            right_place.path.as_slice(),
                            exact,
                            true,
                        )
                    });
                    let other = left_place
                        .root
                        .physical_alias_guard(&right_place.root, guard, true)
                        .and_then(|any| match &exact {
                            Some(exact) => any.difference(exact),
                            None => Some(any),
                        });
                    match (same, other) {
                        (Some(same), Some(other)) => Some(same.or(&other)),
                        (same, other) => same.or(other),
                    }
                } else {
                    let object =
                        |place: &'_ SymbolicPlace<'db>, address: &Option<LinearAddress<'db>>| {
                            address
                                .as_ref()
                                .map_or(&place.root, |address| &address.object)
                                .clone()
                        };
                    object(left_place, &left_address).physical_alias_guard(
                        &object(right_place, &right_address),
                        guard,
                        false,
                    )
                };
                let Some(possible) = possible else {
                    continue;
                };
                // A typed access shares its exact intersection with the typed
                // borrow. A byte span certainly shares a byte only when it is
                // nonempty and starts at the borrowed place.
                let certain = match extent {
                    AccessExtent::Typed => true,
                    AccessExtent::Bytes(_) => {
                        left_len.is_some_and(|len| len > 0)
                            && left_place.path.as_slice().len() == right_place.path.as_slice().len()
                    }
                    AccessExtent::Unknown => false,
                };
                let definite = exact
                    .filter(|_| certain)
                    .and_then(|exact| {
                        path_alias_guard(
                            left_place.path.as_slice(),
                            right_place.path.as_slice(),
                            exact,
                            false,
                        )
                    })
                    .and_then(|definite| definite.and(&possible));
                pairs.push(FootprintPair {
                    access: left.payload,
                    borrowed: right.payload,
                    extent,
                    possible,
                    definite,
                });
            }
        }
        pairs
    }
}

/// The byte length of one side of a comparison, when known.
fn extent_length<'db>(
    db: &'db dyn HirAnalysisDb,
    extent: AccessExtent<'db>,
    place: &SymbolicPlace<'db>,
    address: &Option<LinearAddress<'db>>,
    guard: &Guard<'db>,
) -> Option<u64> {
    match extent {
        AccessExtent::Typed
            if place
                .root
                .contract()
                .is_some_and(|contract| !contract.addressable) =>
        {
            Some(0)
        }
        AccessExtent::Typed => address
            .as_ref()
            .and_then(|address| semantic_size(db, address.ty)),
        AccessExtent::Bytes(IndexExpr::Const(len)) => u64::try_from(len).ok(),
        AccessExtent::Bytes(index) if guard.proves_equal(index, IndexExpr::Const(0)) => Some(0),
        AccessExtent::Bytes(_) | AccessExtent::Unknown => None,
    }
}

/// Constant, nonwrapping byte ranges of one linear object that do not meet.
fn disjoint_ranges<'db>(
    left: &Option<LinearAddress<'db>>,
    right: &Option<LinearAddress<'db>>,
    left_len: Option<u64>,
    right_len: Option<u64>,
) -> bool {
    if let (Some(left), Some(right), Some(left_len), Some(right_len)) =
        (left, right, left_len, right_len)
        && left.object == right.object
        && let (Some(left_start), Some(right_start)) = (left.offset, right.offset)
        && let (Some(left_end), Some(right_end)) = (
            left_start.checked_add(left_len),
            right_start.checked_add(right_len),
        )
    {
        left_end <= right_start || right_end <= left_start
    } else {
        false
    }
}

/// The source-language size_of contract supplies the linear-memory layout.
/// Unknown types, overflow and unsupported projections retain uncertainty.
///
/// Overlap checks ask for the sizes of the same types for every pair of
/// accessed places. An array length can evaluate `size_of` through CTFE, so a
/// size may depend on itself; it is then unknown until the cycle converges.
#[salsa::tracked(cycle_fn=semantic_size_cycle_recover, cycle_initial=semantic_size_cycle_initial)]
fn semantic_size<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<u64> {
    if ty.has_param(db) {
        return None;
    }
    runtime_size_bytes(db, ty).ok().flatten()
}

fn semantic_size_cycle_initial<'db>(_db: &'db dyn HirAnalysisDb, _ty: TyId<'db>) -> Option<u64> {
    None
}

fn semantic_size_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Option<u64>,
    _count: u32,
    _ty: TyId<'db>,
) -> salsa::CycleRecoveryAction<Option<u64>> {
    salsa::CycleRecoveryAction::Iterate
}

struct LinearAddress<'db> {
    object: RegionRoot<'db>,
    offset: Option<u64>,
    ty: TyId<'db>,
}

impl<'db> LinearAddress<'db> {
    fn new(db: &'db dyn HirAnalysisDb, place: &SymbolicPlace<'db>) -> Option<Self> {
        let contract = place.root.contract()?;
        if contract.address_space != HandleAddressSpace::Known(ProviderAddressSpace::Memory)
            || matches!(&place.root, RegionRoot::External(source) if matches!(source.origin, ExternalOrigin::OpaqueMemory))
            || place.views.iter().next().is_some()
        {
            return None;
        }
        let mut address = if let RegionRoot::External(source) = &place.root
            && source.dereferences().is_empty()
            && !source.is_reachable()
            && let ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } = &source.origin
        {
            let mut address = Self::new(
                db,
                &SymbolicPlace {
                    root: RegionRoot::External(base.source.clone()),
                    path: base.path.clone(),
                    views: base.views.clone(),
                },
            )?;
            if *offset == MemoryOffset::Unknown {
                address.offset = None;
            } else if let MemoryOffset::Element(stride, index) = offset {
                address.offset = address.offset.and_then(|offset| {
                    let IndexExpr::Const(index) = index else {
                        return None;
                    };
                    offset.checked_add(
                        semantic_size(db, *stride)?.checked_mul(u64::try_from(*index).ok()?)?,
                    )
                });
            }
            address.ty = *target_ty;
            address
        } else {
            Self {
                object: place.root.clone(),
                offset: Some(0),
                ty: contract.ty,
            }
        };
        for projection in place.path.as_slice() {
            let (ty, offset) = match projection {
                Projection::Index(index) if address.ty.is_array(db) => {
                    let ty = *address.ty.generic_args(db).first()?;
                    let offset = if let IndexExpr::Const(index) = index {
                        semantic_size(db, ty)
                            .and_then(|size| size.checked_mul(u64::try_from(*index).ok()?))
                    } else {
                        None
                    };
                    (ty, offset)
                }
                Projection::Field(field) => {
                    let fields = address.ty.field_types(db);
                    let field = usize::from(field.0);
                    (
                        *fields.get(field)?,
                        fields[..field].iter().try_fold(0_u64, |offset, ty| {
                            offset.checked_add(semantic_size(db, *ty)?)
                        }),
                    )
                }
                Projection::VariantField { variant, field } => {
                    let enum_ = address.ty.as_enum(db)?;
                    let fields = enum_
                        .variants(db)
                        .nth(usize::from(variant.0))?
                        .field_tys(db)
                        .into_iter()
                        .map(|ty| ty.instantiate(db, address.ty.generic_args(db)))
                        .collect::<Vec<_>>();
                    let field = usize::from(field.0);
                    let tag = u64::from(enum_tag_bits(enum_.len_variants(db)).div_ceil(8));
                    (
                        *fields.get(field)?,
                        fields[..field].iter().try_fold(tag, |offset, ty| {
                            offset.checked_add(semantic_size(db, *ty)?)
                        }),
                    )
                }
                Projection::Index(_) => return None,
            };
            address.offset = address
                .offset
                .zip(offset)
                .and_then(|(base, offset)| base.checked_add(offset));
            address.ty = ty;
        }
        Some(address)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::semantic::{
            FieldIndex, VariantIndex,
            capability::{
                external::{ExternalSource, ReferentContract},
                guard::{ChoiceKey, Guard, ValueOccurrence},
                index::{BinderScope, IndexNamespace},
                path::{RegionPath, StructuralPath},
                source::{InputSource, SourceExpr},
            },
        },
        test_db::HirAnalysisTestDb,
    };

    fn shifted<'db>(
        db: &'db HirAnalysisTestDb,
        offset: IndexExpr<'db>,
        ty: TyId<'db>,
    ) -> RegionSet<'db> {
        let source = ExternalSource::input(
            InputSource::slot(0, StructuralPath::default()),
            ReferentContract::memory(db, TyId::u8(db)),
            false,
        );
        RegionSet::singleton(
            &BinderScope::default(),
            RegionRoot::External(ExternalSource::memory(
                db,
                SourceExpr {
                    source,
                    path: RegionPath::default(),
                    views: Default::default(),
                    invalidated: false,
                },
                ty,
                MemoryOffset::Element(TyId::u8(db), offset),
            )),
            RegionPath::default(),
        )
    }

    #[test]
    fn typed_cells_separate_only_at_the_accessed_location() {
        let db = HirAnalysisTestDb::default();
        let ty = TyId::u256(&db);
        for space in [ProviderAddressSpace::Memory, ProviderAddressSpace::Storage] {
            let base = ExternalSource::input(
                InputSource::slot(0, StructuralPath::default()),
                ReferentContract::new(&db, ty, HandleAddressSpace::Known(space)),
                false,
            );
            let cell = |base, index| {
                let base = SourceExpr::whole(base);
                ExternalSource::memory(
                    &db,
                    base,
                    ty,
                    MemoryOffset::Element(ty, IndexExpr::Const(index)),
                )
            };
            let region = |source| {
                RegionSet::singleton(
                    &BinderScope::default(),
                    RegionRoot::External(source),
                    RegionPath::default(),
                )
            };
            let (first, second) = (cell(base.clone(), 1), cell(base, 2));
            let (left, right) = (
                region(cell(first.clone(), 2)),
                region(cell(first.clone(), 3)),
            );
            assert_eq!(left.overlap(&db, &right), OverlapResult::Disjoint);
            // A raw byte span may cross from one cell into the next.
            let bytes = AccessFootprint {
                region: &left,
                extent: AccessExtent::Bytes(IndexExpr::Const(64)),
            };
            assert_ne!(
                bytes.overlap(&db, AccessFootprint::typed(&right)),
                OverlapResult::Disjoint
            );
            // Offsets compose: distinct intermediate cells reach one address.
            let (left, right) = (region(cell(first, 2)), region(cell(second, 1)));
            assert_ne!(left.overlap(&db, &right), OverlapResult::Disjoint);
        }
    }

    #[test]
    fn physical_footprints_cover_all_bounded_concrete_interval_overlaps() {
        let db = HirAnalysisTestDb::default();
        let cases: Vec<_> = [0, 1, 2, 31, 32, 33, 64]
            .into_iter()
            .flat_map(|start| {
                [0, 1, 2, 8, 32, 64].map(|len| {
                    (
                        start,
                        len,
                        shifted(&db, IndexExpr::Const(start), TyId::u8(&db)),
                    )
                })
            })
            .collect();
        for (left_start, left_len, left_region) in &cases {
            for (right_start, right_len, right_region) in &cases {
                let left = AccessFootprint {
                    region: left_region,
                    extent: AccessExtent::Bytes(IndexExpr::Const(*left_len)),
                };
                let right = AccessFootprint {
                    region: right_region,
                    extent: AccessExtent::Bytes(IndexExpr::Const(*right_len)),
                };
                let concrete_overlap = *left_len != 0
                    && *right_len != 0
                    && left_start < &(right_start + right_len)
                    && right_start < &(left_start + left_len);
                let abstract_disjoint = left.overlap(&db, right) == OverlapResult::Disjoint;
                assert!(
                    !abstract_disjoint || !concrete_overlap,
                    "lost overlap: ({left_start}, {left_len}) vs ({right_start}, {right_len})"
                );
                // These constant, nonwrapping intervals have a full proof.
                assert_eq!(abstract_disjoint, !concrete_overlap);
            }
        }
        for (ty, width) in [(TyId::u8(&db), 1), (TyId::u256(&db), 32)] {
            for (start, len, bytes) in &cases {
                let typed = shifted(&db, IndexExpr::Const(2), ty);
                let disjoint = AccessFootprint::typed(&typed).overlap(
                    &db,
                    AccessFootprint {
                        region: bytes,
                        extent: AccessExtent::Bytes(IndexExpr::Const(*len)),
                    },
                ) == OverlapResult::Disjoint;
                let concrete_overlap = *len != 0 && *start < 2 + width && 2 < start + len;
                assert_eq!(
                    disjoint, !concrete_overlap,
                    "typed width {width} vs ({start}, {len})"
                );
            }
        }
    }

    #[test]
    fn overlap_classification_matches_complete_guarded_intersections() {
        let db = HirAnalysisTestDb::default();
        let scope = BinderScope::default();
        let roots: Vec<_> = (0..2)
            .map(|param| {
                RegionSet::singleton(
                    &scope,
                    RegionRoot::External(ExternalSource::input(
                        InputSource::slot(param, StructuralPath::default()),
                        ReferentContract::new(
                            &db,
                            TyId::u256(&db),
                            HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                        ),
                        false,
                    )),
                    RegionPath::default(),
                )
            })
            .collect();
        let selected = |variant| {
            Guard::always(&scope)
                .with_variant(
                    ChoiceKey::new(ValueOccurrence::Argument(0), StructuralPath::default()),
                    VariantIndex(variant),
                )
                .unwrap()
        };
        let regions = [
            RegionSet::empty(&scope),
            roots[0].clone(),
            roots[1].clone(),
            roots[0].union(&roots[1]),
            roots[0].with_guard(&selected(0)),
            roots[1].with_guard(&selected(1)),
            roots[0]
                .with_guard(&selected(0))
                .union(&roots[1].with_guard(&selected(1))),
        ];
        let footprints: Vec<_> = regions
            .iter()
            .flat_map(|region| {
                [
                    AccessExtent::Typed,
                    AccessExtent::Unknown,
                    AccessExtent::Bytes(0.into()),
                    AccessExtent::Bytes(1.into()),
                    AccessExtent::Bytes(IndexExpr::FormalValue(0)),
                ]
                .map(|extent| AccessFootprint { region, extent })
            })
            .collect();
        for &left in &footprints {
            for &right in &footprints {
                let (common, uncertain) = left.intersect(&db, right);
                let expected = if uncertain {
                    OverlapResult::Unknown
                } else if common.is_empty() {
                    OverlapResult::Disjoint
                } else {
                    OverlapResult::Overlap(common)
                };
                assert_eq!(left.overlap(&db, right), expected);
            }
        }
    }

    #[test]
    fn physical_footprints_keep_unknown_and_overflowing_ranges_conservative() {
        let db = HirAnalysisTestDb::default();
        let fixed = shifted(&db, IndexExpr::Const(2), TyId::u256(&db));
        for offset in [
            IndexExpr::Const(1),
            IndexExpr::FormalValue(0),
            IndexExpr::Const(usize::MAX),
        ] {
            let unknown = shifted(&db, offset, TyId::u8(&db));
            for extent in [
                AccessExtent::Unknown,
                AccessExtent::Bytes(IndexExpr::FormalValue(1)),
                AccessExtent::Bytes(IndexExpr::Const(usize::MAX)),
            ] {
                assert_ne!(
                    AccessFootprint {
                        region: &unknown,
                        extent
                    }
                    .overlap(&db, AccessFootprint::typed(&fixed)),
                    OverlapResult::Disjoint
                );
            }
            assert_eq!(
                AccessFootprint {
                    region: &unknown,
                    extent: AccessExtent::Bytes(IndexExpr::Const(0))
                }
                .overlap(&db, AccessFootprint::typed(&fixed)),
                OverlapResult::Disjoint
            );
        }
    }

    fn input_root<'db>(
        db: &'db HirAnalysisTestDb,
        param: u32,
        dereferences: Vec<RegionPath<IndexExpr<'db>>>,
    ) -> RegionRoot<'db> {
        let contract = ReferentContract::memory(db, TyId::u256(db));
        let source = dereferences.into_iter().fold(
            ExternalSource::input(InputSource::place(param), contract, false),
            |source, path| source.follow(path, contract, false),
        );
        RegionRoot::External(source)
    }

    fn pairs<'db>(
        db: &'db HirAnalysisTestDb,
        access: (
            &BinderScope,
            RegionRoot<'db>,
            Vec<Projection<IndexExpr<'db>>>,
        ),
        borrowed: (RegionRoot<'db>, Vec<Projection<IndexExpr<'db>>>),
    ) -> Vec<FootprintPair<'db>> {
        let (scope, root, path) = access;
        let access = RegionSet::singleton(scope, root, RegionPath::new(path));
        let borrowed = RegionSet::singleton(scope, borrowed.0, RegionPath::new(borrowed.1));
        AccessFootprint::typed(&access).physical_pairs(db, &borrowed)
    }

    #[test]
    fn physical_pairs_separate_paths_only_within_one_proved_object() {
        let db = HirAnalysisTestDb::default();
        let scope = BinderScope::default();
        let field = |index| Projection::Field(FieldIndex(index));
        let variant = |variant| Projection::VariantField {
            variant: VariantIndex(variant),
            field: FieldIndex(0),
        };
        // Distinct certain inputs may overlap anywhere; fields do not separate them.
        let possible = pairs(
            &db,
            (&scope, input_root(&db, 0, vec![]), vec![field(0)]),
            (input_root(&db, 1, vec![]), vec![field(1)]),
        );
        assert!(matches!(possible.as_slice(), [pair] if pair.definite.is_none()));
        // Within one proved object, different fields are separate.
        assert!(
            pairs(
                &db,
                (&scope, input_root(&db, 0, vec![]), vec![field(0)]),
                (input_root(&db, 0, vec![]), vec![field(1)]),
            )
            .is_empty()
        );
        // Enum variants overlay the same storage.
        assert_eq!(
            pairs(
                &db,
                (&scope, input_root(&db, 0, vec![]), vec![variant(0)]),
                (input_root(&db, 0, vec![]), vec![variant(1)]),
            )
            .len(),
            1
        );
        // Unequal selectors of stored pointers can still hold equal addresses.
        let (scope, first) = scope.bind(IndexNamespace::Value);
        let (scope, second) = scope.bind(IndexNamespace::Value);
        let loaded = |index| input_root(&db, 0, vec![RegionPath::new([Projection::Index(index)])]);
        assert_eq!(
            pairs(
                &db,
                (&scope, loaded(first), vec![field(0)]),
                (loaded(second), vec![field(1)]),
            )
            .len(),
            1
        );
    }

    #[test]
    fn physical_pairs_call_only_certain_intersections_definite() {
        let db = HirAnalysisTestDb::default();
        let (scope, index) = BinderScope::default().bind(IndexNamespace::Value);
        let root = RegionRoot::External(ExternalSource::input(
            InputSource::place(0),
            ReferentContract::memory(&db, TyId::u256(&db)),
            false,
        ));
        let whole = RegionSet::singleton(&scope, root.clone(), RegionPath::default());
        let member =
            RegionSet::singleton(&scope, root, RegionPath::new([Projection::Index(index)]));
        // A typed access contains every typed member it prefixes.
        let [typed] = AccessFootprint::typed(&whole)
            .physical_pairs(&db, &member)
            .try_into()
            .ok()
            .unwrap();
        assert!(typed.definite.is_some());
        // One byte at the start need not reach the member a caller selects.
        let [byte] = AccessFootprint {
            region: &whole,
            extent: AccessExtent::Bytes(IndexExpr::Const(1)),
        }
        .physical_pairs(&db, &member)
        .try_into()
        .ok()
        .unwrap();
        assert!(byte.definite.is_none());
        // It certainly shares the first byte of a place it starts at.
        let [start] = AccessFootprint {
            region: &member,
            extent: AccessExtent::Bytes(IndexExpr::Const(1)),
        }
        .physical_pairs(&db, &member)
        .try_into()
        .ok()
        .unwrap();
        assert!(start.definite.is_some());
    }
}
