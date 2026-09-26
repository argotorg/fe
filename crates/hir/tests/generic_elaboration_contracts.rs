use camino::Utf8PathBuf;
use common::indexmap::IndexMap;
use fe_hir::{
    analysis::{
        semantic::{
            SemanticInstanceKey, check_semantic_borrows, check_semantic_boundaries,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            root_semantic_instance_key,
        },
        ty::{
            binder::Binder,
            normalize::normalize_ty,
            trait_def::TraitInstId,
            trait_resolution::PredicateListId,
            ty_check::BodyOwner,
            ty_def::{Kind, TyData, TyId, TyVarSort},
            ty_lower::{
                CompleteSubst, LoweredSlot, ParamBasis, ParamDomainId, ParamKey, SourceParamIndex,
                param_schema,
            },
            unify::{InferenceKey, UnificationTableBase},
        },
    },
    hir_def::{CallableDef, IdentId, ItemKind},
    test_db::{HirAnalysisTestDb, find_func},
};

/// The selected callee of the first call site in `key`'s instance.
fn first_callee_key<'db>(
    db: &'db HirAnalysisTestDb,
    key: SemanticInstanceKey<'db>,
) -> SemanticInstanceKey<'db> {
    get_or_build_semantic_instance(db, key)
        .call_sites(db)
        .iter()
        .flatten()
        .find_map(|site| site.callee)
        .expect("missing semantic callee")
        .key
}

#[test]
fn concrete_callees_do_not_inherit_unrelated_caller_bounds() {
    for source in [
        r#"
trait A {}
trait B {}
trait Out { fn f() {} }
impl Out for bool {}
fn first<T: A>() { <bool as Out>::f() }
fn second<U: B>() { <bool as Out>::f() }
"#,
        r#"
trait A {}
trait B {}
trait Need {}
impl Need for bool {}
fn target<T: Need>(_ x: T) {}
fn first<X: A>() { target(true) }
fn second<Y: B>() { target(true) }
"#,
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("concrete_callee_evidence.fe"), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let [first, second] = ["first", "second"].map(|name| {
            let func = find_func(&db, module, name);
            first_callee_key(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func)),
            )
        });
        assert_eq!(first, second, "{source}");
    }
}

#[test]
fn source_and_full_method_schemas_keep_inherited_keys_in_both_query_orders() {
    for source_first in [true, false] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("method_schema_basis.fe"),
            r#"
struct Slot<const ROOT: u256 = _> {}
trait T<X> { fn f<Y>(_ value: Slot, _ extra: Y) {} }
fn foreign<Z>() {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let func = module
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "f")
            })
            .expect("missing trait method");
        let (source, full) = if source_first {
            (
                param_schema(&db, func.into(), ParamBasis::Source),
                param_schema(&db, func.into(), ParamBasis::Full),
            )
        } else {
            let full = param_schema(&db, func.into(), ParamBasis::Full);
            let source = param_schema(&db, func.into(), ParamBasis::Source);
            (source, full)
        };
        assert_eq!(source.basis(&db), ParamBasis::Source);
        assert_eq!(full.basis(&db), ParamBasis::Full);
        assert!(
            full.keys(&db)
                .iter()
                .any(|key| matches!(key, ParamKey::CallableLayout { .. })),
            "method signature did not reserve a hidden layout slot"
        );
        let own_key = source
            .source_key(&db, SourceParamIndex(0))
            .expect("missing method source parameter");
        assert_eq!(full.source_key(&db, SourceParamIndex(0)), Some(own_key));
        assert!(
            full.slot_for(&db, own_key).expect("missing full slot").0
                > source
                    .slot_for(&db, own_key)
                    .expect("missing source slot")
                    .0
        );
        assert_eq!(&source.keys(&db)[..2], &full.keys(&db)[..2]);
        let inherited = source.key_at(&db, LoweredSlot(0)).unwrap();
        let inherited_formal = source.formal_at(&db, LoweredSlot(0)).unwrap();
        assert_eq!(source.original_key(&db, inherited_formal), Some(inherited));
        assert_eq!(full.original_key(&db, inherited_formal), Some(inherited));
        let ParamKey::TraitSelf(trait_) = inherited else {
            panic!("first inherited key should be trait Self");
        };
        let parent = param_schema(&db, trait_.into(), ParamBasis::Full);
        let parent_formal = parent.formal_at(&db, LoweredSlot(0)).unwrap();
        assert_ne!(parent_formal, inherited_formal);
        assert_eq!(full.original_key(&db, parent_formal), Some(inherited));
        let TyData::TyParam(param) = inherited_formal.data(&db) else {
            panic!("trait Self should be a parameter");
        };
        assert_eq!(param.owner, func.scope());

        let source_own = source
            .formal_at(&db, source.slot_for(&db, own_key).unwrap())
            .unwrap();
        let full_own = full
            .formal_at(&db, full.slot_for(&db, own_key).unwrap())
            .unwrap();
        assert_eq!(source.original_key(&db, source_own), Some(own_key));
        assert_eq!(full.original_key(&db, full_own), Some(own_key));
        assert_eq!(full.original_key(&db, source_own), None);
        let hidden_key = full.key_at(&db, LoweredSlot(2)).unwrap();
        assert!(matches!(hidden_key, ParamKey::CallableLayout { .. }));
        let hidden = full.formal_at(&db, LoweredSlot(2)).unwrap();
        assert_eq!(source.original_key(&db, hidden), None);

        let foreign = module
            .all_funcs(&db)
            .iter()
            .copied()
            .find(|candidate| {
                candidate
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "foreign")
            })
            .expect("missing foreign function");
        let foreign_param = CallableDef::Func(foreign).params(&db)[0];
        assert_eq!(source.original_key(&db, foreign_param), None);
        assert_eq!(full.original_key(&db, foreign_param), None);

        let domain = ParamDomainId::full(&db, full);
        let values = domain
            .slots(&db)
            .map(|slot| full.formal_at(&db, slot).unwrap())
            .collect();
        let subst = CompleteSubst::new(domain, &db, values).unwrap();
        assert_eq!(subst.get(&db, own_key), Some(full_own));
        let prefix = source
            .allowed_default_dependencies(&db, SourceParamIndex(0))
            .unwrap();
        assert!(prefix.position_for(&db, inherited).is_some());
        assert_eq!(prefix.position_for(&db, own_key), None);
    }
}

#[test]
fn sealed_effect_identity_retains_bodyless_call_after_semantic_checks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("sealed_effect_trait_selection.fe"),
        r#"
use std::evm::RawStorage
fn access() -> u256 uses (raw: mut RawStorage) { raw.sload(0) }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let func = find_func(&db, module, "access");
    let identity = identity_semantic_instance_key(&db, BodyOwner::Func(func));
    let instance = get_or_build_semantic_instance(&db, identity);
    check_semantic_borrows(&db, instance).expect("sealed effect call must pass borrow validation");
    check_semantic_boundaries(&db, instance)
        .expect("sealed effect call must pass boundary validation");
    let BodyOwner::Func(selected) = first_callee_key(&db, identity).owner(&db) else {
        panic!("sealed effect callee is not a function");
    };
    assert!(
        selected.body(&db).is_none(),
        "sealed effect call unexpectedly selected its runtime body in HIR"
    );

    let root_key = root_semantic_instance_key(&db, BodyOwner::Func(func))
        .expect("standalone root must synthesize the sealed effect provider");
    let BodyOwner::Func(selected) = first_callee_key(&db, root_key).owner(&db) else {
        panic!("root sealed effect callee is not a function");
    };
    assert!(
        selected.body(&db).is_some(),
        "standalone root did not select a concrete effect method body"
    );
}

#[test]
fn substituting_projection_and_evidence_preserves_both_arguments_and_bindings() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("projection_binding_substitution.fe"),
        "trait Out { type Item }\nfn f<T>() {}\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let (trait_, func) =
        module
            .children_non_nested(&db)
            .fold((None, None), |acc, item| match item {
                ItemKind::Trait(trait_) => (Some(trait_), acc.1),
                ItemKind::Func(func) => (acc.0, Some(func)),
                _ => acc,
            });
    let trait_ = trait_.expect("missing Out");
    let func = func.expect("missing f");
    let param = CallableDef::Func(func).params(&db)[0];
    let item = IdentId::new(&db, "Item");
    let inst = TraitInstId::new(
        &db,
        trait_,
        vec![param],
        IndexMap::from_iter([(item, param)]),
    );
    let projection = TyId::assoc_ty(&db, inst.trait_ref(&db), item);
    let result = Binder::bind(func.into(), projection).instantiate(&db, &[TyId::bool(&db)]);
    let evidence = Binder::bind(func.into(), inst).instantiate(&db, &[TyId::bool(&db)]);
    let TyData::AssocTy(result) = result.data(&db) else {
        panic!("expected projection after substitution");
    };
    assert_eq!(result.trait_.args(&db), &[TyId::bool(&db)]);
    assert_eq!(
        evidence.assoc_type_bindings(&db).get(&item),
        Some(&TyId::bool(&db)),
        "associated binding retained a stale declaration parameter"
    );
}

#[test]
fn projection_normalization_does_not_commit_speculative_trait_arguments() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("speculative_trait_reference_matching.fe"),
        "trait Out<A> { type Item }\nimpl Out<u8> for bool { type Item = u256 }\nfn f<T>() {}\n",
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let trait_ = module
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Trait(trait_) => Some(trait_),
            _ => None,
        })
        .expect("missing Out");
    let item = IdentId::new(&db, "Item");
    let mut table = UnificationTableBase::<ena::unify::InPlace<InferenceKey<'_>>>::new(&db);
    let unknown = table.new_var(TyVarSort::General, &Kind::Star);
    let projection_ref = TraitInstId::new_simple(&db, trait_, vec![TyId::bool(&db), unknown]);
    let projection = TyId::assoc_ty(&db, projection_ref.trait_ref(&db), item);
    let assumption = TraitInstId::new(
        &db,
        trait_,
        vec![TyId::bool(&db), TyId::u8(&db)],
        IndexMap::from_iter([(item, TyId::u256(&db))]),
    );
    let assumptions = PredicateListId::new(&db, vec![assumption]);
    assert_eq!(
        normalize_ty(&db, projection, module.scope(), assumptions),
        projection,
        "possible equality or impl selection cannot assign a caller inference variable"
    );
}
