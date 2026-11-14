//! Associated effect rows.
//!
//! A trait declares a row, `uses E`, and each implementation gives its
//! effects, `uses E = (storage: mut RawStorage)`, or none. A `uses E` entry of
//! a trait method, or `uses C::E` for a type `C` bounded by the trait, names
//! the row of a trait instance. Where the instance's implementation is known
//! the row expands to its components: effects, and rows that expand in turn.
//! A row that does not expand is matched by identity: a call forwards the
//! caller's own. In an implementation's own methods, `uses E` is spliced
//! into the method's effects (`Func::effects`).
use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            binder::Binder,
            closure::{CallableTrait, CallableValue},
            effects::resolve_effect_key,
            fold::{TyFoldable, TyFolder},
            trait_def::{ImplementorOrigin, TraitInstId, resolve_trait_impl_instance},
            trait_resolution::{PredicateListId, Selection, TraitSolveCx},
            visitor::{TyVisitable, TyVisitor},
        },
    },
    core::semantic::{EffectRequirement, EffectRequirementKey, constraints_for},
    hir_def::{IdentId, ImplTrait, TypeId as HirTypeId, scope_graph::ScopeId},
};

/// Row `row` of a trait instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub struct RowKey<'db> {
    pub inst: TraitInstId<'db>,
    pub row: u16,
}

impl<'db> RowKey<'db> {
    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.inst.def(db).rows(db)[self.row as usize].name.to_opt()
    }
}

impl<'db> TyVisitable<'db> for RowKey<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.inst.visit_with(visitor)
    }
}

impl<'db> TyFoldable<'db> for RowKey<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        RowKey {
            inst: self.inst.fold_with(db, folder),
            row: self.row,
        }
    }
}

/// A component of a row: an ordinary effect requirement, or another row.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct RowComponent<'db> {
    pub name: IdentId<'db>,
    pub key: EffectRequirementKey<'db>,
    pub is_mut: bool,
    pub key_syntax: HirTypeId<'db>,
}

impl<'db> TyVisitable<'db> for RowComponent<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        self.key.visit_with(visitor)
    }
}

impl<'db> TyFoldable<'db> for RowComponent<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        RowComponent {
            key: self.key.fold_with(db, folder),
            ..self
        }
    }
}

/// Where a row component sits: the requirement entry whose row it descends
/// from, and its index in each row on the way down. A path names the same
/// component in every view of a requirement, however far each view expands
/// it.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct RowPath {
    pub entry: u32,
    pub steps: Vec<u32>,
}

impl RowPath {
    pub fn entry(entry: u32) -> Self {
        Self {
            entry,
            steps: Vec::new(),
        }
    }

    fn child(&self, index: usize) -> Self {
        let mut steps = self.steps.clone();
        steps.push(index as u32);
        Self {
            entry: self.entry,
            steps,
        }
    }

    /// `self` with its prefix `from` replaced by `to`, if it has that prefix.
    pub fn rebase(&self, from: &RowPath, to: &RowPath) -> Option<RowPath> {
        (self.entry == from.entry && self.steps.starts_with(&from.steps)).then(|| RowPath {
            entry: to.entry,
            steps: to
                .steps
                .iter()
                .chain(&self.steps[from.steps.len()..])
                .copied()
                .collect(),
        })
    }
}

/// A component of the rows a function's requirements name, as one view
/// expands them: an effect, or a row the view cannot expand.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
pub struct RowComponentRequirement<'db> {
    pub path: RowPath,
    pub requirement: EffectRequirement<'db>,
}

/// The rows among a function's effect requirements, as one view expands
/// them: each as far as the view selects the implementations involved.
#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue, Default)]
pub struct RowExpansion<'db> {
    /// The components, depth first in entry order, as effect requirements
    /// numbered after the function's own. Numbers are particular to the
    /// view; paths are not.
    pub components: Vec<RowComponentRequirement<'db>>,
    /// The entries whose rows the view cannot expand at all.
    pub abstract_rows: Vec<(u32, RowKey<'db>)>,
}

impl<'db> RowExpansion<'db> {
    /// Whether the row entry `entry` names stays abstract.
    pub fn is_abstract(&self, entry: u32) -> bool {
        self.abstract_rows
            .iter()
            .any(|(abstract_entry, _)| *abstract_entry == entry)
    }

    pub fn component(&self, path: &RowPath) -> Option<&RowComponentRequirement<'db>> {
        self.components
            .iter()
            .find(|component| component.path == *path)
    }
}

pub fn expand_rows<'db>(
    db: &'db dyn HirAnalysisDb,
    requirements: &[EffectRequirement<'db>],
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> RowExpansion<'db> {
    let mut next = requirements
        .iter()
        .map(|requirement| requirement.binding_idx + 1)
        .max()
        .unwrap_or(0);
    let mut out = RowExpansion::default();
    for requirement in requirements {
        let Some(row) = requirement.key.key_row() else {
            continue;
        };
        let Some(components) = direct_row_components(db, row, scope, assumptions) else {
            out.abstract_rows.push((requirement.binding_idx, row));
            continue;
        };
        let entry = RowPath::entry(requirement.binding_idx);
        // Each component with the rows expanded above it: a row that names
        // itself is reported at its implementation and stays abstract here.
        let mut pending: Vec<_> = components
            .into_iter()
            .enumerate()
            .rev()
            .map(|(index, component)| (entry.child(index), component, vec![row]))
            .collect();
        while let Some((path, component, above)) = pending.pop() {
            if let Some(inner) = component.key.key_row() {
                if above.contains(&inner) {
                    out.abstract_rows.push((requirement.binding_idx, inner));
                    continue;
                }
                if let Some(components) = direct_row_components(db, inner, scope, assumptions) {
                    let mut above = above;
                    above.push(inner);
                    pending.extend(
                        components
                            .into_iter()
                            .enumerate()
                            .rev()
                            .map(|(index, component)| {
                                (path.child(index), component, above.clone())
                            }),
                    );
                    continue;
                }
            }
            out.components.push(RowComponentRequirement {
                path,
                requirement: EffectRequirement {
                    binding_name: component.name,
                    key: component.key,
                    is_mut: component.is_mut,
                    binding_site: requirement.binding_site,
                    binding_idx: next,
                    binding_ty: component.key_syntax,
                },
            });
            next += 1;
        }
    }
    out
}

/// Whether `key`'s row is known to have no effects. A row that names itself,
/// which its implementation reports, is not.
pub(crate) fn row_is_empty<'db>(
    db: &'db dyn HirAnalysisDb,
    key: RowKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    fn empty<'db>(
        db: &'db dyn HirAnalysisDb,
        key: RowKey<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        above: &mut Vec<RowKey<'db>>,
    ) -> bool {
        if above.contains(&key) {
            return false;
        }
        above.push(key);
        let empty = direct_row_components(db, key, scope, assumptions).is_some_and(|components| {
            components.iter().all(|component| {
                component
                    .key
                    .key_row()
                    .is_some_and(|inner| empty(db, inner, scope, assumptions, above))
            })
        });
        above.pop();
        empty
    }
    empty(db, key, scope, assumptions, &mut Vec::new())
}

/// Whether `key`'s row, expanded through the rows its components name, comes
/// back to a row being expanded, as `uses E = Self::E` does.
pub(crate) fn row_key_cycles<'db>(
    db: &'db dyn HirAnalysisDb,
    key: RowKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> bool {
    fn cycles<'db>(
        db: &'db dyn HirAnalysisDb,
        key: RowKey<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        above: &mut Vec<RowKey<'db>>,
    ) -> bool {
        if above.contains(&key) {
            return true;
        }
        above.push(key);
        let cycles = direct_row_components(db, key, scope, assumptions).is_some_and(|components| {
            components.iter().any(|component| {
                component
                    .key
                    .key_row()
                    .is_some_and(|inner| cycles(db, inner, scope, assumptions, above))
            })
        });
        above.pop();
        cycles
    }
    cycles(db, key, scope, assumptions, &mut Vec::new())
}

/// The components `key`'s implementation gives its row, if the scope
/// selects the implementation.
fn direct_row_components<'db>(
    db: &'db dyn HirAnalysisDb,
    key: RowKey<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<Vec<RowComponent<'db>>> {
    // A callable value's row is the effects a call of it runs.
    if CallableTrait::of(db, key.inst.def(db)).is_some()
        && let Some(value) = CallableValue::of(db, key.inst.self_ty(db))
    {
        return Some(value.effects(db));
    }
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let name = key.name(db)?;
    let row_effects = |impl_trait: ImplTrait<'db>| {
        impl_trait
            .rows(db)
            .iter()
            .find(|row| row.name.to_opt() == Some(name))
            .and_then(|row| row.effects)
            .filter(|effects| !effects.data(db).is_empty())
    };
    // An instance still being inferred selects its implementation, but only
    // an empty row expands before its arguments are known.
    if key.inst.args(db).iter().any(|ty| ty.has_var(db)) {
        let Selection::Unique(implementor) = solve_cx.select_impl(db, key.inst) else {
            return None;
        };
        let ImplementorOrigin::Hir(impl_trait) = implementor.origin(db) else {
            return None;
        };
        return row_effects(impl_trait).is_none().then(Vec::new);
    }
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, key.inst) else {
        return None;
    };
    implementation_row_components(db, resolved, name)
}

/// The row supplied by this selected implementation at its actual arguments.
pub(crate) fn implementation_row_components<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: crate::analysis::ty::trait_def::ResolvedImplInstance<'db>,
    name: IdentId<'db>,
) -> Option<Vec<RowComponent<'db>>> {
    let row_effects = |impl_trait: ImplTrait<'db>| {
        impl_trait
            .rows(db)
            .iter()
            .find(|row| row.name.to_opt() == Some(name))
            .and_then(|row| row.effects)
            .filter(|effects| !effects.data(db).is_empty())
    };
    let ImplementorOrigin::Hir(impl_trait) = resolved.selected().origin(db) else {
        return None;
    };
    let Some(effects) = row_effects(impl_trait) else {
        return Some(Vec::new());
    };
    let impl_scope = impl_trait.scope();
    let impl_assumptions = constraints_for(db, impl_trait.into());
    let args = resolved.impl_args(db);
    effects
        .data(db)
        .iter()
        .map(|effect| {
            let key_syntax = effect.key_ty.to_opt()?;
            let key = resolve_effect_key(db, key_syntax, impl_scope, impl_assumptions)
                .into_requirement_key(db);
            let key = Binder::bind(impl_trait.into(), key).instantiate(db, args);
            Some(RowComponent {
                name: effect
                    .name
                    .or_else(|| key_syntax.as_path(db)?.ident(db).to_opt())?,
                key,
                is_mut: effect.is_mut(),
                key_syntax,
            })
        })
        .collect()
}
