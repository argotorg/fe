use hir::analysis::{HirAnalysisDb, ty::ty_def::TyId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{MirProjection, MirProjectionPath, ir::AddressSpaceKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CapabilitySpaceConflict<'db> {
    pub path: crate::MirProjectionPath<'db>,
    pub existing: AddressSpaceKind,
    pub incoming: AddressSpaceKind,
}

pub(crate) fn normalize_capability_space_entries<'db>(
    entries: impl IntoIterator<Item = (crate::MirProjectionPath<'db>, AddressSpaceKind)>,
) -> Result<Vec<(crate::MirProjectionPath<'db>, AddressSpaceKind)>, CapabilitySpaceConflict<'db>> {
    let mut merged: FxHashMap<crate::MirProjectionPath<'db>, AddressSpaceKind> =
        FxHashMap::default();
    for (path, space) in entries {
        let Some(existing) = merged.get(&path).copied() else {
            merged.insert(path, space);
            continue;
        };
        if existing == space || matches!(space, AddressSpaceKind::Memory) {
            continue;
        }
        if matches!(existing, AddressSpaceKind::Memory) {
            merged.insert(path, space);
            continue;
        }
        return Err(CapabilitySpaceConflict {
            path,
            existing,
            incoming: space,
        });
    }
    let mut out: Vec<_> = merged.into_iter().collect();
    out.sort_by_cached_key(|(path, _)| format!("{path:?}"));
    Ok(out)
}

fn collect_capability_leaf_paths_inner<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    prefix: &MirProjectionPath<'db>,
    out: &mut Vec<MirProjectionPath<'db>>,
    active: &mut FxHashSet<TyId<'db>>,
) {
    // Track only the active recursion chain so sibling branches that reuse the same field type
    // are still traversed.
    if !active.insert(ty) {
        return;
    }

    if let Some((_, inner)) = ty.as_capability(db) {
        let before = out.len();
        collect_capability_leaf_paths_inner(db, inner, prefix, out, active);
        if out.len() == before {
            out.push(prefix.clone());
        }
    } else if let Some(inner) = crate::repr::transparent_newtype_field_ty(db, ty) {
        collect_capability_leaf_paths_inner(db, inner, prefix, out, active);
    } else {
        for (idx, field_ty) in ty.field_types(db).iter().copied().enumerate() {
            let mut field_prefix = prefix.clone();
            field_prefix.push(MirProjection::Field(idx));
            collect_capability_leaf_paths_inner(db, field_ty, &field_prefix, out, active);
        }
    }

    active.remove(&ty);
}

pub(crate) fn capability_leaf_paths_for_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<MirProjectionPath<'db>> {
    let mut out = Vec::new();
    let mut active = FxHashSet::default();
    collect_capability_leaf_paths_inner(db, ty, &MirProjectionPath::new(), &mut out, &mut active);
    out
}

pub(crate) fn capability_spaces_for_ty_with_default<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
) -> Vec<(MirProjectionPath<'db>, AddressSpaceKind)> {
    capability_leaf_paths_for_ty(db, ty)
        .into_iter()
        .map(|path| (path, default_space))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn memory_then_non_memory_promotes_to_non_memory() {
        let root = crate::MirProjectionPath::new();
        let entries = vec![
            (root.clone(), AddressSpaceKind::Memory),
            (root.clone(), AddressSpaceKind::Storage),
        ];
        let normalized = normalize_capability_space_entries(entries).expect("no conflict");
        assert_eq!(normalized, vec![(root, AddressSpaceKind::Storage)]);
    }

    #[test]
    fn non_memory_then_memory_keeps_non_memory() {
        let root = crate::MirProjectionPath::new();
        let entries = vec![
            (root.clone(), AddressSpaceKind::Calldata),
            (root.clone(), AddressSpaceKind::Memory),
        ];
        let normalized = normalize_capability_space_entries(entries).expect("no conflict");
        assert_eq!(normalized, vec![(root, AddressSpaceKind::Calldata)]);
    }

    #[test]
    fn conflicting_non_memory_spaces_error() {
        let root = crate::MirProjectionPath::new();
        let entries = vec![
            (root.clone(), AddressSpaceKind::Storage),
            (root.clone(), AddressSpaceKind::Calldata),
        ];
        let conflict = normalize_capability_space_entries(entries).expect_err("must conflict");
        assert_eq!(conflict.path, root);
        assert_eq!(conflict.existing, AddressSpaceKind::Storage);
        assert_eq!(conflict.incoming, AddressSpaceKind::Calldata);
    }
}
