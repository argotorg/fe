//! Arbitrary replacement contents after a possible byte-level overwrite.
//! Entry reads have a different denotation and must never be used as havoc.
use super::{
    external::{AddressProvenance, ClobberCondition, ExternalSource, ReferentContract},
    footprint::AccessFootprint,
    guard::Guard,
    handle::{
        AddressOccurrence, OpaqueContentsId, OpaqueHandleContract, OpaqueHandleRef, OpaqueWriteSite,
    },
    index::{BinderScope, IndexNamespace},
    loan::CapabilityRef,
    path::RegionPath,
    region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace, substitute_clause},
    semantics::{CapabilityClass, UnresolvedCapability},
    shape::ShapeId,
    source::SourceExpr,
    state::{CapabilityValue, CapabilityValues},
    value::{Guarded, IndexPayload},
};
use crate::{
    analysis::ty::{trait_resolution::PredicateListId, ty_def::TyId},
    hir_def::scope_graph::ScopeId,
};

#[derive(Clone, Copy)]
pub struct OpaqueWrite<'db> {
    pub site: OpaqueWriteSite<'db>,
    pub scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
}

impl<'db> OpaqueWrite<'db> {
    pub fn contents(
        self,
        values: &mut CapabilityValues<'db>,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        target: Option<&RegionRoot<'db>>,
        written: Option<AccessFootprint<'_, 'db>>,
    ) -> Result<CapabilityValue<'db>, UnresolvedCapability<'db>> {
        let db = values.db;
        // Keep conditional replacements for identifiable cells. Seeds and
        // replacements inside already arbitrary memory share one closed heap
        // family; retaining their cell parameters would grow at every load.
        let saturated = matches!(self.site, OpaqueWriteSite::Seed)
            || target.is_some_and(
                |root| matches!(root, RegionRoot::External(source) if source.is_arbitrary()),
            );
        // A raw pointer stored in raw memory and overwritten through an
        // arbitrary address joins the same family. Its condition would repeat
        // an earlier replacement's once per write site, multiplying the cell's
        // alternatives across repeated writes through replaced pointers.
        let raw_memory_cell = target.is_some_and(
            |root| matches!(root, RegionRoot::External(source) if source.in_raw_memory()),
        );
        let mut failure = None;
        let value = values.from_shape(shape, scope, |semantics, path, scope| {
            let native = matches!(
                semantics.class,
                CapabilityClass::Borrow(_) | CapabilityClass::View
            );
            // Native bytes establish no valid target or authority. This address
            // is only a stable summary identity; typed use of the marker rejects.
            let ty = if native {
                TyId::ptr_to(db, semantics.target_ty)
            } else {
                semantics.representation_ty
            };
            let contract = if let Ok(Some(contract)) =
                OpaqueHandleContract::for_ty(db, self.scope, self.assumptions, ty)
            {
                contract
            } else {
                failure = Some(UnresolvedCapability(ty));
                return Vec::new();
            };
            let mut family = ExternalSource::opaque_memory(ReferentContract::new(
                db,
                contract.target_ty,
                contract.address_space,
            ));
            // Contents reached through a conditional cell exist only under
            // that cell's prerequisite. Reuse it without introducing another
            // overwrite identity or nesting a new condition at each load.
            if let Some(RegionRoot::External(source)) = target {
                family.clobber = source.clobber_dependency().cloned().map(Box::new);
            }
            let (witness_scope, source) = if saturated {
                (scope.clone(), family.clone())
            } else {
                // Independent cells may receive different addresses even at the
                // same write site. Lexical witnesses keep replay deterministic.
                let (witness_scope, witness) = scope.bind(IndexNamespace::Existential);
                (
                    witness_scope,
                    ExternalSource::opaque(
                        db,
                        OpaqueHandleRef {
                            contract,
                            occurrence: AddressOccurrence::Overwrite(OpaqueContentsId::new(
                                db,
                                self.site,
                                semantics.representation_ty,
                                path.clone(),
                            )),
                            arguments: scope.variables().chain([witness]).collect(),
                            provenance: AddressProvenance::Raw,
                        },
                    ),
                )
            };
            let alternatives = if !saturated
                && let (Some(target), Some(written)) = (target, written)
            {
                let extent = written.extent;
                let target = SymbolicPlace {
                    root: target.clone(),
                    path: RegionPath::new(path.as_slice()),
                    views: Default::default(),
                };
                let target_region =
                    RegionSet::singleton(&witness_scope, target.root.clone(), target.path.clone());
                written
                    .region
                    .clauses()
                    .iter()
                    .filter_map(|clause| {
                        let fresh = clause.guard.scope().freshening(&witness_scope);
                        let guard = clause.guard.substitute(&fresh)?;
                        let written_region =
                            RegionSet::new(&witness_scope, vec![substitute_clause(clause, &fresh)]);
                        if matches!(
                            AccessFootprint::typed(&target_region).overlap(
                                db,
                                AccessFootprint {
                                    region: &written_region,
                                    extent: extent.substitute(&fresh),
                                }
                            ),
                            OverlapResult::Disjoint
                        ) {
                            return None;
                        }
                        // A write through a conditional replacement keeps that
                        // replacement's condition as its prerequisite, which
                        // offsets and summaries restate.
                        if !native
                            && raw_memory_cell
                            && matches!(&clause.payload.root,
                                RegionRoot::External(written)
                                    if written.is_arbitrary() && !written.has_clobber_dependency())
                        {
                            return Some((guard, family.clone()));
                        }
                        // Only the condition lets a caller refute a replaced
                        // address. Overwrite occurrences may alias each other, so
                        // a per-site identity separates nothing, but it multiplies
                        // a cell's alternatives by every write site and summary
                        // choice. Native capabilities keep their identity.
                        let mut alternative = if native {
                            source.clone()
                        } else {
                            family.clone()
                        };
                        alternative.clobber = SourceExpr::from_place(&target)
                            .zip(SourceExpr::from_place(&clause.payload))
                            .map(|(target, written)| {
                                Box::new(ClobberCondition::new(
                                    target,
                                    written.substitute(db, &fresh),
                                    extent.substitute(&fresh),
                                ))
                            });
                        Some((guard, alternative))
                    })
                    .collect::<Vec<_>>()
            } else {
                vec![(Guard::always(&witness_scope), source)]
            };
            alternatives
                .into_iter()
                .map(|(guard, source)| {
                    let region = RegionSet::singleton(
                        guard.scope(),
                        RegionRoot::External(source),
                        RegionPath::default(),
                    );
                    let payload = if native {
                        CapabilityRef::Invalidated {
                            class: semantics.class,
                            region,
                        }
                    } else {
                        CapabilityRef::Address(region)
                    };
                    Guarded { guard, payload }
                })
                .collect()
        });
        failure.map_or(Ok(value), Err)
    }
}
