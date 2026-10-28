//! Names of runtime objects.
//!
//! An object is named after the contract it is built from. Source names are
//! only unique within one module, so two contracts of an ingot (or of a
//! dependency) may share a name. Following the rule used for function symbols,
//! the bare name is kept while it is unique, and on a clash the module path,
//! then the ingot name, then a short fingerprint are added.
//!
//! The names of the contracts declared in the ingot being built are chosen over
//! all of its contracts, whether or not a given package contains them. Every
//! package of one ingot therefore agrees on every contract's name. A contract
//! from another ingot that is pulled into a package (`create2` of a dependency's
//! contract) fits around those names: the local contract keeps a name the two
//! share.

use std::collections::BTreeMap;

use common::{indexmap::IndexSet, ingot::Ingot};
use hir::hir_def::{Contract, HirIngot, ItemKind, ManualContractRootAttr, TopLevelMod};
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    runtime::stable_key::{
        ingot_component_for_scope, item_identity, module_path_components_for_scope,
        stable_identity_fingerprint,
    },
};

const FINGERPRINT_LEN: usize = 4;
const SEGMENT_SEPARATOR: &str = "__";

/// What a runtime object is built from. The object's name is derived from this
/// once every object of a package is known, never from a bare string.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum ObjectId<'db> {
    /// An object whose name is fixed by the construct (`main`, a test).
    Fixed(String),
    /// A high-level `contract`.
    Contract(Contract<'db>),
    /// The `#[contract_init(Name)]` and `#[contract_runtime(Name)]` roots of one
    /// module. `name` is the sanitized source name.
    ManualContract {
        top_mod: TopLevelMod<'db>,
        name: String,
    },
}

impl<'db> ObjectId<'db> {
    pub(crate) fn manual(top_mod: TopLevelMod<'db>, name: &str) -> Self {
        Self::ManualContract {
            top_mod,
            name: sanitize_object_name(name),
        }
    }

    /// The source name, sanitized: the name used while it is unique.
    pub(crate) fn base_name(&self, db: &'db dyn MirDb) -> String {
        match self {
            Self::Fixed(name) | Self::ManualContract { name, .. } => name.clone(),
            Self::Contract(contract) => contract_object_base_name(db, *contract),
        }
    }

    /// Orders objects independently of their final names.
    pub(crate) fn sort_key(&self, db: &'db dyn MirDb) -> (String, String) {
        (self.base_name(db), self.identity(db))
    }

    fn identity(&self, db: &'db dyn MirDb) -> String {
        match self {
            Self::Fixed(name) => name.clone(),
            Self::Contract(contract) => item_identity(db, (*contract).into()),
            Self::ManualContract { top_mod, name } => {
                format!("{}${name}", item_identity(db, ItemKind::TopMod(*top_mod)))
            }
        }
    }

    fn ingot(&self, db: &'db dyn MirDb) -> Option<Ingot<'db>> {
        match self {
            Self::Fixed(_) => None,
            Self::Contract(contract) => Some(contract.scope().top_mod(db).ingot(db)),
            Self::ManualContract { top_mod, .. } => Some(top_mod.ingot(db)),
        }
    }

    /// Names to try, from the most to the least preferred. The last carries a
    /// fingerprint of the item, so distinct items do not run out of names; the
    /// rare clash that survives is numbered by `uniquify`.
    fn candidates(&self, db: &'db dyn MirDb) -> Vec<String> {
        let base = self.base_name(db);
        let (mut module_parts, ingot_name) = match self {
            Self::Fixed(_) => return vec![base],
            Self::Contract(contract) => (
                module_path_components_for_scope(db, contract.scope()),
                ingot_component_for_scope(db, contract.scope()),
            ),
            Self::ManualContract { top_mod, .. } => (
                module_path_of(db, *top_mod),
                ingot_component_for_scope(db, top_mod.scope()),
            ),
        };
        module_parts.push(base.clone());
        let mut ingot_parts = vec![ingot_name];
        ingot_parts.extend(module_parts.iter().cloned());
        let by_module = join_segments(&module_parts);
        let by_ingot = join_segments(&ingot_parts);
        let fingerprint = stable_identity_fingerprint(&self.identity(db));
        let mut candidates = vec![base];
        for candidate in [
            by_module,
            by_ingot.clone(),
            format!("{by_ingot}_{}", &fingerprint[..FINGERPRINT_LEN]),
        ] {
            if !candidates.contains(&candidate) {
                candidates.push(candidate);
            }
        }
        candidates
    }
}

/// A contract of an ingot, as it is named in the build artifacts.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IngotContract<'db> {
    /// The object and artifact name, unique in the ingot.
    pub name: String,
    /// The name written in the source, which another contract may share.
    pub source_name: String,
    /// The module that declares the contract.
    pub top_mod: TopLevelMod<'db>,
    /// The `contract` item, or `None` for a contract made of manual roots.
    pub contract: Option<Contract<'db>>,
}

/// The contracts declared in `ingot`, with the names their objects and
/// artifacts carry. Sorted by name.
pub fn ingot_contracts<'db>(db: &'db dyn MirDb, ingot: Ingot<'db>) -> Vec<IngotContract<'db>> {
    let ids = declared_object_ids(db, ingot);
    let names = assign_object_names(db, ingot, ids);
    let mut contracts = ids
        .iter()
        .cloned()
        .zip(names)
        .map(|(id, name)| {
            let source_name = id.base_name(db);
            match id {
                ObjectId::Contract(contract) => IngotContract {
                    name,
                    source_name,
                    top_mod: contract.scope().top_mod(db),
                    contract: Some(contract),
                },
                ObjectId::ManualContract { top_mod, .. } => IngotContract {
                    name,
                    source_name,
                    top_mod,
                    contract: None,
                },
                ObjectId::Fixed(_) => unreachable!("declared objects are contracts"),
            }
        })
        .collect::<Vec<_>>();
    contracts.sort_by(|lhs, rhs| lhs.name.cmp(&rhs.name));
    contracts
}

/// The path of the module `top_mod`, the way a contract declared in it has it:
/// its ancestors, then its own name.
fn module_path_of<'db>(db: &'db dyn MirDb, top_mod: TopLevelMod<'db>) -> Vec<String> {
    let mut path = module_path_components_for_scope(db, top_mod.scope());
    // For the root module the helper has no ancestor to report and names the
    // module itself.
    if top_mod != top_mod.ingot(db).root_mod(db) {
        path.push(top_mod.name(db).data(db).to_string());
    }
    path
}

/// Every contract `ingot` declares, in a deterministic order.
#[salsa::tracked(returns(ref))]
fn declared_object_ids<'db>(db: &'db dyn MirDb, ingot: Ingot<'db>) -> Vec<ObjectId<'db>> {
    let mut ids = Vec::new();
    for &top_mod in ingot.all_modules(db) {
        ids.extend(
            top_mod
                .all_contracts(db)
                .iter()
                .copied()
                .map(ObjectId::Contract),
        );
        let mut manual = IndexSet::new();
        for &func in top_mod.all_funcs(db) {
            if func.top_mod(db) != top_mod {
                continue;
            }
            if let Some(
                ManualContractRootAttr::Init { contract_name }
                | ManualContractRootAttr::Runtime { contract_name },
            ) = func.manual_contract_root_attr(db)
            {
                manual.insert(ObjectId::manual(top_mod, contract_name.data(db)));
            }
        }
        ids.extend(manual);
    }
    ids
}

/// Names the objects `ids` of a package built from `ingot`. The names are
/// pairwise distinct, and each contract of `ingot` gets the name it has in
/// every other package of the ingot.
pub(crate) fn assign_object_names<'db>(
    db: &'db dyn MirDb,
    ingot: Ingot<'db>,
    ids: &[ObjectId<'db>],
) -> Vec<String> {
    let mut domain = ids.iter().cloned().collect::<IndexSet<_>>();
    domain.extend(declared_object_ids(db, ingot).iter().cloned());
    let slots = domain
        .iter()
        .map(|id| Slot {
            candidates: id.candidates(db),
            movable: !matches!(id, ObjectId::Fixed(_)),
            local: id.ingot(db).is_none_or(|owner| owner == ingot),
        })
        .collect::<Vec<_>>();
    let names = select_names(&slots);
    ids.iter()
        .map(|id| {
            let index = domain.get_index_of(id).expect("domain contains the ids");
            names[index].clone()
        })
        .collect()
}

#[derive(Debug)]
struct Slot {
    candidates: Vec<String>,
    /// `false` for a name fixed by the construct (`main`, a test).
    movable: bool,
    /// Declared in the ingot being built.
    local: bool,
}

/// Picks one candidate per slot so that the picks are pairwise distinct. Slots
/// that share a pick move to their next candidate, except that a local slot
/// keeps a pick it shares with a slot of another ingot.
fn select_names(slots: &[Slot]) -> Vec<String> {
    let mut chosen = vec![0usize; slots.len()];
    loop {
        let mut groups = BTreeMap::<&str, Vec<usize>>::new();
        for (index, slot) in slots.iter().enumerate() {
            groups
                .entry(slot.candidates[chosen[index]].as_str())
                .or_default()
                .push(index);
        }
        let mut moved = false;
        for group in groups.values().filter(|group| group.len() > 1) {
            let foreign_present = group.iter().any(|&index| !slots[index].local);
            for &index in group {
                let slot = &slots[index];
                if !slot.movable || (slot.local && foreign_present) {
                    continue;
                }
                if chosen[index] + 1 < slot.candidates.len() {
                    chosen[index] += 1;
                    moved = true;
                }
            }
        }
        if !moved {
            break;
        }
    }
    uniquify(slots, &chosen)
}

/// Last resort when the candidates of items with the same identity run out:
/// number the later ones.
fn uniquify(slots: &[Slot], chosen: &[usize]) -> Vec<String> {
    let mut names = slots
        .iter()
        .zip(chosen)
        .map(|(slot, &chosen)| slot.candidates[chosen].clone())
        .collect::<Vec<_>>();
    let mut used = slots
        .iter()
        .zip(&names)
        .filter(|(slot, _)| !slot.movable)
        .map(|(_, name)| name.clone())
        .collect::<std::collections::HashSet<_>>();
    for (index, slot) in slots.iter().enumerate() {
        if !slot.movable {
            continue;
        }
        let base = names[index].clone();
        let mut candidate = base.clone();
        let mut suffix = 0usize;
        while !used.insert(candidate.clone()) {
            suffix += 1;
            candidate = format!("{base}_{suffix}");
        }
        names[index] = candidate;
    }
    names
}

fn contract_object_base_name<'db>(db: &'db dyn MirDb, contract: Contract<'db>) -> String {
    sanitize_object_name(
        &contract
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "contract".to_string()),
    )
}

fn join_segments(parts: &[String]) -> String {
    parts
        .iter()
        .map(|part| sanitize_object_name(part))
        .collect::<Vec<_>>()
        .join(SEGMENT_SEPARATOR)
}

pub(crate) fn sanitize_object_name(value: &str) -> String {
    let sanitized = value
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>();
    if sanitized.is_empty() {
        "object".to_string()
    } else {
        sanitized
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slot(candidates: &[&str], local: bool) -> Slot {
        Slot {
            candidates: candidates.iter().map(|c| c.to_string()).collect(),
            movable: true,
            local,
        }
    }

    fn fixed(name: &str) -> Slot {
        Slot {
            candidates: vec![name.to_string()],
            movable: false,
            local: true,
        }
    }

    #[test]
    fn unique_names_stay_bare() {
        let slots = [
            slot(&["A", "m__A"], true),
            slot(&["B", "m__B"], true),
            fixed("main"),
        ];
        assert_eq!(select_names(&slots), ["A", "B", "main"]);
    }

    #[test]
    fn a_clash_qualifies_every_contract_involved() {
        let slots = [
            slot(&["C", "a__C"], true),
            slot(&["C", "b__C"], true),
            slot(&["D", "a__D"], true),
        ];
        assert_eq!(select_names(&slots), ["a__C", "b__C", "D"]);
    }

    #[test]
    fn three_way_clash_is_resolved_in_one_step() {
        let slots = [
            slot(&["C", "a__C"], true),
            slot(&["C", "b__C"], true),
            slot(&["C", "c__C"], true),
        ];
        assert_eq!(select_names(&slots), ["a__C", "b__C", "c__C"]);
    }

    #[test]
    fn qualified_names_that_clash_move_on_to_the_ingot_name() {
        let slots = [
            slot(&["C", "a__C", "x__a__C"], true),
            slot(&["C", "a__C", "y__a__C"], false),
        ];
        // The local contract keeps `C` while the foreign one moves.
        assert_eq!(select_names(&slots), ["C", "a__C"]);
        let slots = [
            slot(&["C", "a__C", "x__a__C"], false),
            slot(&["C", "a__C", "y__a__C"], false),
        ];
        assert_eq!(select_names(&slots), ["x__a__C", "y__a__C"]);
    }

    #[test]
    fn a_local_contract_keeps_a_name_shared_with_a_foreign_one() {
        let slots = [
            slot(&["C", "app__C", "p__app__C"], true),
            slot(&["C", "lib__C", "p__lib__C"], false),
        ];
        assert_eq!(select_names(&slots), ["C", "lib__C"]);
    }

    #[test]
    fn a_name_fixed_by_the_construct_is_never_renamed() {
        let slots = [fixed("main"), slot(&["main", "m__main"], true)];
        assert_eq!(select_names(&slots), ["main", "m__main"]);
    }

    #[test]
    fn identical_candidates_are_numbered() {
        let slots = [slot(&["C"], true), slot(&["C"], true)];
        assert_eq!(select_names(&slots), ["C", "C_1"]);
    }
}
