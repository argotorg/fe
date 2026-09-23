//! Physical access ranges, separate from address identity and typed coverage.
use common::layout::enum_tag_bits;

use super::{
    external::ExternalOrigin,
    handle::HandleAddressSpace,
    index::{IndexExpr, IndexSubst},
    path::Projection,
    region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace, open_clause_pair},
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
                let guard = left.guard.and(&right.guard)?;
                let left_address = LinearAddress::new(db, &left.payload);
                let right_address = LinearAddress::new(db, &right.payload);
                let length =
                    |extent, place: &SymbolicPlace<'db>, address: &Option<LinearAddress<'db>>| {
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
                            AccessExtent::Bytes(index)
                                if guard.proves_equal(index, IndexExpr::Const(0)) =>
                            {
                                Some(0)
                            }
                            AccessExtent::Bytes(_) | AccessExtent::Unknown => None,
                        }
                    };
                let left_len = length(
                    self.extent.substitute(&left_subst),
                    &left.payload,
                    &left_address,
                );
                let right_len = length(
                    other.extent.substitute(&right_subst),
                    &right.payload,
                    &right_address,
                );
                if left_len == Some(0) || right_len == Some(0) {
                    return None;
                }
                if let (Some(left), Some(right), Some(left_len), Some(right_len)) =
                    (&left_address, &right_address, left_len, right_len)
                    && left.object == right.object
                    && let (Some(left_start), Some(right_start)) = (left.offset, right.offset)
                    && let (Some(left_end), Some(right_end)) = (
                        left_start.checked_add(left_len),
                        right_start.checked_add(right_len),
                    )
                    && (left_end <= right_start || right_end <= left_start)
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
                    let guard = left_object.alias_guard(right_object, guard, true)?;
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

/// The source-language size_of contract supplies the linear-memory layout.
/// Unknown types, overflow and unsupported projections retain uncertainty.
fn semantic_size(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<u64> {
    if ty.has_param(db) {
        return None;
    }
    runtime_size_bytes(db, ty).ok().flatten()
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
            || place.views.iter().next().is_some()
        {
            return None;
        }
        let mut address = if let RegionRoot::External(source) = &place.root
            && source.dereferences().is_empty()
            && !source.is_reachable()
            && let ExternalOrigin::Memory {
                base,
                element,
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
            if let Some((stride, index)) = element {
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
            VariantIndex,
            capability::{
                external::{ExternalSource, ReferentContract},
                guard::{ChoiceKey, Guard, ValueOccurrence},
                index::BinderScope,
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
            ReferentContract::new(
                db,
                TyId::u8(db),
                HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            ),
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
                Some((TyId::u8(db), offset)),
            )),
            RegionPath::default(),
        )
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
}
