//! The manual contract roots that a build makes objects of are the functions
//! that the HIR marks as manual roots, which is also where the drop check
//! starts its walk. A function with a malformed or generic root attribute is in
//! neither list.
use common::InputDb;
use driver::DriverDataBase;
use fe_mir::ingot_contracts;
use std::collections::BTreeSet;
use url::Url;

#[test]
fn ingot_contracts_and_runtime_roots_list_the_same_manual_roots() {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///manual_root_lists.fe").unwrap(),
        Some(
            r#"
use std::evm::Evm

#[contract_init(Good)]
fn good_init() uses (evm: mut Evm) {}

#[contract_runtime(Good)]
fn good_runtime() uses (evm: mut Evm) {}

#[contract_runtime(Generic)]
fn generic_root<T>() uses (evm: mut Evm) {}

#[contract_init(Twice)]
#[contract_init(Twice)]
fn twice() uses (evm: mut Evm) {}

#[contract_runtime(Other, Extra)]
fn malformed() uses (evm: mut Evm) {}

fn ordinary() {}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);

    let built: BTreeSet<String> = ingot_contracts(&db, top_mod.ingot(&db))
        .iter()
        .filter(|contract| contract.contract().is_none())
        .map(|contract| contract.source_name.clone())
        .collect();
    let marked: BTreeSet<String> = top_mod
        .all_funcs(&db)
        .iter()
        .filter(|func| func.is_runtime_root(&db) && func.is_manual_contract_root(&db))
        .filter_map(|func| func.manual_contract_root_name(&db))
        .map(|name| name.data(&db).to_string())
        .collect();

    assert_eq!(built, marked, "the two lists of manual roots differ");
    assert_eq!(
        marked,
        BTreeSet::from(["Good".to_string()]),
        "only the well-formed, non-generic roots are manual roots"
    );
}
