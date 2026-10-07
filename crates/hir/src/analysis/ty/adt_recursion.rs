//! Representation containment and the finite family of reachable referents.
//!
//! Generic arguments describe an application, not its stored children. Both
//! walks instantiate declaration fields; only the referent walk follows raw
//! pointers and effect-handle targets. ABI schemas also expand dynamic arrays.

use rustc_hash::FxHashSet;

use super::{
    abi_ty::core_dyn_array_elem_ty,
    adt_def::{AdtCycleMember, AdtDef, instantiate_adt_field_shape},
    layout_holes::{LayoutViewRecurrence, classify_layout_view_recurrence},
    provider::{EffectHandleResolution, resolve_effect_handle},
    trait_resolution::{PredicateListId, constraint::collect_constraints},
    ty_def::{TyBase, TyData, TyId},
};
use crate::analysis::HirAnalysisDb;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum RecursionKind {
    Representation,
    Referents,
    Abi,
}

#[salsa::tracked]
pub(crate) fn recursive_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    root: AdtDef<'db>,
    kind: RecursionKind,
) -> Option<Vec<AdtCycleMember<'db>>> {
    let mut ty = TyId::new(db, TyData::TyBase(TyBase::Adt(root)));
    for &param in root.params(db) {
        ty = TyId::app(db, ty, param);
    }
    Walk {
        db,
        root,
        kind,
        assumptions: collect_constraints(db, root.as_generic_param_owner(db))
            .instantiate_identity(),
        ancestors: Vec::new(),
        growth_start: 0,
        provider_depth: 0,
        chain: Vec::new(),
        finished: FxHashSet::default(),
    }
    .walk(ty)
}

struct Walk<'db> {
    db: &'db dyn HirAnalysisDb,
    root: AdtDef<'db>,
    kind: RecursionKind,
    assumptions: PredicateListId<'db>,
    ancestors: Vec<(TyId<'db>, AdtDef<'db>)>,
    growth_start: usize,
    /// Constructor growth in ordinary referents is checked by the ingot-wide
    /// flow analysis. This walk additionally checks paths through provider
    /// targets, which are outside that analysis's field graph.
    provider_depth: usize,
    chain: Vec<AdtCycleMember<'db>>,
    finished: FxHashSet<TyId<'db>>,
}

impl<'db> Walk<'db> {
    fn walk(&mut self, ty: TyId<'db>) -> Option<Vec<AdtCycleMember<'db>>> {
        if self.finished.contains(&ty) {
            return None;
        }
        if let Some(inner) = ty.as_ptr(self.db) {
            return (self.kind == RecursionKind::Referents)
                .then(|| self.walk(inner))
                .flatten();
        }
        if let Some((_, inner)) = ty.as_borrow(self.db) {
            return (self.kind != RecursionKind::Representation)
                .then(|| self.walk(inner))
                .flatten();
        }
        if let Some(inner) = ty.as_view(self.db) {
            return self.walk(inner);
        }
        // Dynamic arrays have finite carriers, but Solidity ABI schemas
        // expand their element types and cannot contain recursive tuples.
        if self.kind == RecursionKind::Abi
            && let Some(element) = core_dyn_array_elem_ty(self.db, ty)
        {
            return self.walk(element);
        }
        if ty.is_tuple(self.db) {
            for &element in ty.generic_args(self.db) {
                if let Some(cycle) = self.walk(element) {
                    return Some(cycle);
                }
            }
        } else if ty.is_array(self.db) {
            // Keep the existing representation rule for inline recursion,
            // including zero-length arrays. Referents are exposed only by a
            // known positive length; symbolic guards are checked by the
            // borrow checker's inventory after instantiation.
            if !(self.kind == RecursionKind::Referents
                && !matches!(ty.array_len(self.db), Some(1..)))
                && let Some(&element) = ty.generic_args(self.db).first()
            {
                return self.walk(element);
            }
        } else if let Some(adt) = ty.adt_def(self.db) {
            match classify_layout_view_recurrence(
                self.db,
                ty,
                adt,
                self.growth_start,
                self.ancestors
                    .iter()
                    .enumerate()
                    .rev()
                    .map(|(idx, &(earlier, family))| (idx, earlier, family)),
            ) {
                LayoutViewRecurrence::BackEdge { ancestor } => {
                    if self.kind != RecursionKind::Referents {
                        return self.cycle_containing_root(ancestor);
                    }
                    return None;
                }
                LayoutViewRecurrence::NonRegular { ancestor } => {
                    if self.kind == RecursionKind::Referents && self.provider_depth == 0 {
                        return None;
                    }
                    return self.cycle_containing_root(ancestor);
                }
                LayoutViewRecurrence::Expand => {}
            }
            self.ancestors.push((ty, adt));
            for (field_idx, field) in adt.fields(self.db).iter().enumerate() {
                for ty_idx in 0..field.num_types() {
                    let field_ty = instantiate_adt_field_shape(
                        self.db,
                        adt,
                        field_idx,
                        ty_idx,
                        ty.generic_args(self.db),
                    );
                    self.chain.push(AdtCycleMember {
                        adt,
                        field_idx,
                        ty_idx,
                    });
                    let saved_growth = self.growth_start;
                    if !field
                        .ty(self.db, ty_idx)
                        .instantiate_identity()
                        .has_param(self.db)
                    {
                        self.growth_start = self.ancestors.len();
                    }
                    let cycle = self.walk(field_ty);
                    self.growth_start = saved_growth;
                    if let Some(cycle) = cycle {
                        return Some(cycle);
                    }
                    self.chain.pop();
                }
            }
            if self.kind == RecursionKind::Referents
                && !self.chain.is_empty()
                && let EffectHandleResolution::Resolved {
                    target_ty,
                    target_template,
                    ..
                } =
                    resolve_effect_handle(self.db, self.root.scope(self.db), self.assumptions, ty)
            {
                let saved_growth = self.growth_start;
                if !target_template.has_param(self.db) {
                    self.growth_start = self.ancestors.len();
                }
                self.provider_depth += 1;
                let cycle = self.walk(target_ty);
                self.provider_depth -= 1;
                self.growth_start = saved_growth;
                if let Some(cycle) = cycle {
                    return Some(cycle);
                }
            }
            self.ancestors.pop();
        }
        self.finished.insert(ty);
        None
    }

    fn cycle_containing_root(&self, ancestor: usize) -> Option<Vec<AdtCycleMember<'db>>> {
        // Types containing an invalid definition are diagnosed at that
        // definition, rather than reported as participants in its cycle.
        if !self.ancestors[ancestor..]
            .iter()
            .any(|(_, adt)| *adt == self.root)
        {
            return None;
        }
        let mut seen = FxHashSet::default();
        Some(
            self.chain
                .iter()
                .copied()
                .filter(|member| seen.insert(*member))
                .collect(),
        )
    }
}
