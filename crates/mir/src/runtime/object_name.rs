//! Names of runtime objects.
//!
//! An object is named after what it is built from. Source names are only
//! unique within one module, so two contracts of an ingot (or of a dependency)
//! may share a name. Following the rule used for function symbols, the bare
//! name is kept while it is unique, and on a clash the module path, then the
//! ingot name, then a short fingerprint are added.
//!
//! The contracts an ingot declares are named once, over all of them, by
//! [`ingot_contracts`]. A package keeps those names and fits its other objects
//! (`main`, tests, and contracts of other ingots reached by `create2`) around
//! them. A contract therefore has the same name in every package of its ingot,
//! and that name is its artifact name.

use std::collections::BTreeMap;

use common::{indexmap::IndexSet, ingot::Ingot};
use hir::hir_def::{Contract, Func, HirIngot, ItemKind, ManualContractRootAttr, TopLevelMod};
use rustc_hash::FxHashSet;
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    runtime::{
        package::{contract_name, sanitize_symbol},
        stable_key::{
            ingot_component_for_scope, item_identity, module_path_components_for_scope,
            stable_identity_fingerprint,
        },
    },
};

const FINGERPRINT_LEN: usize = 4;
const SEGMENT_SEPARATOR: &str = "__";
/// The name of the executable object. No contract takes it.
const MAIN_OBJECT_NAME: &str = "main";

/// What a runtime object is built from. The object's name is derived from this
/// once every object of a package is known, never from a bare string.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub(crate) enum ObjectId<'db> {
    /// The `main` executable.
    Main,
    /// A high-level `contract`.
    Contract(Contract<'db>),
    /// The `#[contract_init(Name)]` and `#[contract_runtime(Name)]` roots of one
    /// module. `name` is the sanitized source name.
    ManualContract {
        top_mod: TopLevelMod<'db>,
        name: String,
    },
    /// A `#[test]` function.
    Test(Func<'db>),
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
            Self::Main => MAIN_OBJECT_NAME.to_string(),
            Self::Contract(contract) => contract_name(db, *contract),
            Self::ManualContract { name, .. } => name.clone(),
            Self::Test(func) => sanitize_object_name(
                &func
                    .name(db)
                    .to_opt()
                    .map_or_else(|| "test".to_string(), |name| name.data(db).to_string()),
            ),
        }
    }

    /// Orders objects independently of their final names.
    pub(crate) fn sort_key(&self, db: &'db dyn MirDb) -> (String, String) {
        (self.base_name(db), self.identity(db))
    }

    fn identity(&self, db: &'db dyn MirDb) -> String {
        match self {
            Self::Main => MAIN_OBJECT_NAME.to_string(),
            Self::Contract(contract) => item_identity(db, (*contract).into()),
            Self::ManualContract { top_mod, name } => {
                format!("{}${name}", item_identity(db, ItemKind::TopMod(*top_mod)))
            }
            Self::Test(func) => item_identity(db, (*func).into()),
        }
    }

    /// Names to try, from the most to the least preferred. The last carries a
    /// fingerprint of the item, so distinct items do not run out of names; the
    /// rare clash that survives is numbered by `select_names`.
    fn candidates(&self, db: &'db dyn MirDb) -> Vec<String> {
        let base = self.base_name(db);
        let (scope, mut module_parts) = match self {
            Self::Main => return vec![base],
            Self::Contract(contract) => (
                contract.scope(),
                module_path_components_for_scope(db, contract.scope()),
            ),
            Self::Test(func) => (
                func.scope(),
                module_path_components_for_scope(db, func.scope()),
            ),
            Self::ManualContract { top_mod, .. } => {
                // The path of an item declared in `top_mod`: the modules above
                // `top_mod`, then `top_mod` itself.
                let scope = top_mod.scope();
                let mut path = if scope.parent_module(db).is_some() {
                    module_path_components_for_scope(db, scope)
                } else {
                    Vec::new()
                };
                path.push(top_mod.name(db).data(db).to_string());
                (scope, path)
            }
        };
        module_parts.push(base.clone());
        let mut ingot_parts = vec![ingot_component_for_scope(db, scope)];
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
#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
pub struct IngotContract<'db> {
    /// The object and artifact name, unique in the ingot.
    pub name: String,
    /// The name written in the source, which another contract may share.
    pub source_name: String,
    /// The module that declares the contract.
    pub top_mod: TopLevelMod<'db>,
    pub(crate) id: ObjectId<'db>,
}

impl<'db> IngotContract<'db> {
    /// The `contract` item, or `None` for a contract made of manual roots.
    pub fn contract(&self) -> Option<Contract<'db>> {
        match self.id {
            ObjectId::Contract(contract) => Some(contract),
            _ => None,
        }
    }
}

/// The contracts declared in `ingot`, with the names their objects and
/// artifacts carry in every package of the ingot. Sorted by name.
#[salsa::tracked(returns(ref))]
pub fn ingot_contracts<'db>(db: &'db dyn MirDb, ingot: Ingot<'db>) -> Vec<IngotContract<'db>> {
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
            if func.top_mod(db) == top_mod
                && let Some(
                    ManualContractRootAttr::Init { contract_name }
                    | ManualContractRootAttr::Runtime { contract_name },
                ) = func.manual_contract_root_attr(db)
            {
                manual.insert(ObjectId::manual(top_mod, contract_name.data(db)));
            }
        }
        ids.extend(manual);
    }
    // In identity order, the numbering of a clash that survives every
    // candidate does not depend on the order of the modules.
    ids.sort_by_cached_key(|id| id.sort_key(db));
    let mut slots = vec![Slot {
        candidates: vec![MAIN_OBJECT_NAME.to_string()],
        movable: false,
    }];
    slots.extend(ids.iter().map(|id| Slot {
        candidates: id.candidates(db),
        movable: true,
    }));
    let mut contracts = Vec::with_capacity(ids.len());
    for (id, name) in ids
        .into_iter()
        .zip(select_names(&slots).into_iter().skip(1))
    {
        let top_mod = match &id {
            ObjectId::Contract(contract) => contract.scope().top_mod(db),
            ObjectId::ManualContract { top_mod, .. } => *top_mod,
            ObjectId::Main | ObjectId::Test(_) => unreachable!("declared objects are contracts"),
        };
        contracts.push(IngotContract {
            name,
            source_name: id.base_name(db),
            top_mod,
            id,
        });
    }
    contracts.sort_by(|lhs, rhs| lhs.name.cmp(&rhs.name));
    contracts
}

/// Names the objects `ids` of a package built from `ingot`. The contracts of
/// `ingot` keep the names [`ingot_contracts`] gives them, and the other objects
/// take names around those. The names are pairwise distinct.
pub(crate) fn assign_object_names<'db>(
    db: &'db dyn MirDb,
    ingot: Ingot<'db>,
    ids: &[ObjectId<'db>],
) -> Vec<String> {
    let mut domain = IndexSet::new();
    let mut slots = Vec::new();
    for contract in ingot_contracts(db, ingot) {
        domain.insert(contract.id.clone());
        slots.push(Slot {
            candidates: vec![contract.name.clone()],
            movable: false,
        });
    }
    for id in ids {
        if domain.insert(id.clone()) {
            slots.push(Slot {
                candidates: id.candidates(db),
                movable: *id != ObjectId::Main,
            });
        }
    }
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
    /// `false` for a settled name: `main`, or a contract's name in its ingot.
    movable: bool,
}

/// Picks one candidate per slot so that the picks are pairwise distinct. Slots
/// that share a pick move on to their next candidate, and a settled slot keeps
/// its pick. Movable slots whose candidates run out (items with the same
/// identity) are numbered in slot order.
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
        for &index in groups.values().filter(|group| group.len() > 1).flatten() {
            if slots[index].movable && chosen[index] + 1 < slots[index].candidates.len() {
                chosen[index] += 1;
                moved = true;
            }
        }
        if !moved {
            break;
        }
    }
    let mut used = slots
        .iter()
        .zip(&chosen)
        .filter(|(slot, _)| !slot.movable)
        .map(|(slot, &chosen)| slot.candidates[chosen].clone())
        .collect::<FxHashSet<_>>();
    slots
        .iter()
        .zip(chosen)
        .map(|(slot, chosen)| {
            let base = &slot.candidates[chosen];
            if !slot.movable {
                return base.clone();
            }
            let mut name = base.clone();
            let mut suffix = 0usize;
            while !used.insert(name.clone()) {
                suffix += 1;
                name = format!("{base}_{suffix}");
            }
            name
        })
        .collect()
}

fn join_segments(parts: &[String]) -> String {
    parts
        .iter()
        .map(|part| sanitize_object_name(part))
        .collect::<Vec<_>>()
        .join(SEGMENT_SEPARATOR)
}

pub(crate) fn sanitize_object_name(value: &str) -> String {
    let sanitized = sanitize_symbol(value);
    if sanitized.is_empty() {
        "object".to_string()
    } else {
        sanitized
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn slot(candidates: &[&str]) -> Slot {
        Slot {
            candidates: candidates.iter().map(|c| c.to_string()).collect(),
            movable: true,
        }
    }

    fn settled(name: &str) -> Slot {
        Slot {
            candidates: vec![name.to_string()],
            movable: false,
        }
    }

    #[test]
    fn unique_names_stay_bare() {
        let slots = [slot(&["A", "m__A"]), slot(&["B", "m__B"]), settled("main")];
        assert_eq!(select_names(&slots), ["A", "B", "main"]);
    }

    #[test]
    fn a_clash_qualifies_every_contract_involved() {
        let slots = [
            slot(&["C", "a__C"]),
            slot(&["C", "b__C"]),
            slot(&["D", "a__D"]),
        ];
        assert_eq!(select_names(&slots), ["a__C", "b__C", "D"]);
    }

    #[test]
    fn three_way_clash_is_resolved_in_one_step() {
        let slots = [
            slot(&["C", "a__C"]),
            slot(&["C", "b__C"]),
            slot(&["C", "c__C"]),
        ];
        assert_eq!(select_names(&slots), ["a__C", "b__C", "c__C"]);
    }

    #[test]
    fn qualified_names_that_clash_move_on_to_the_ingot_name() {
        let slots = [
            slot(&["C", "a__C", "x__a__C"]),
            slot(&["C", "a__C", "y__a__C"]),
        ];
        assert_eq!(select_names(&slots), ["x__a__C", "y__a__C"]);
    }

    #[test]
    fn objects_fit_around_settled_names() {
        // A contract of the ingot keeps `C`; a dependency's `C` moves.
        let slots = [settled("C"), slot(&["C", "lib__C", "p__lib__C"])];
        assert_eq!(select_names(&slots), ["C", "lib__C"]);
        // `main` is never renamed.
        let slots = [settled("main"), slot(&["main", "m__main"])];
        assert_eq!(select_names(&slots), ["main", "m__main"]);
        // Moving past a settled name does not disturb it either.
        let slots = [
            settled("C"),
            settled("lib__C"),
            slot(&["C", "lib__C", "p__lib__C"]),
        ];
        assert_eq!(select_names(&slots), ["C", "lib__C", "p__lib__C"]);
    }

    #[test]
    fn identical_candidates_are_numbered() {
        let slots = [slot(&["C"]), slot(&["C"])];
        assert_eq!(select_names(&slots), ["C", "C_1"]);
        let slots = [slot(&["C"]), settled("C")];
        assert_eq!(select_names(&slots), ["C_1", "C"]);
    }
}
