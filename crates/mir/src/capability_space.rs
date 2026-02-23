use rustc_hash::FxHashMap;

use crate::ir::AddressSpaceKind;

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
