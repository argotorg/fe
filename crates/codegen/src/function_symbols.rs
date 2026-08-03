use driver::DriverDataBase;
use hir::analysis::{
    semantic::SemanticInstance,
    ty::{
        ty_check::BodyOwner,
        ty_def::{TyBase, TyData, TyId},
    },
};
use mir::{
    RuntimeFunctionOwner,
    runtime::stable_key::{
        closure_symbol_base, generic_args_identity, ingot_component_for_scope,
        module_path_components_for_scope, semantic_owner_context_identity, stable_identity_hash,
    },
};
use rustc_hash::FxHashSet;
use std::collections::BTreeMap;

const GENERIC_SUFFIX_HASH_LEN: usize = 4;
const OWNER_CONTEXT_HASH_LEN: usize = 4;
const CONFLICT_SUFFIX_HASH_LEN: usize = 4;

const SEGMENT_SEPARATOR: &str = "__";
const VARIANT_SEPARATOR: &str = "_";
const FALLBACK_SEPARATOR: &str = "_";

struct OwnerContextCandidates {
    primary: Option<String>,
    fallback: Option<String>,
}

pub(crate) struct FunctionSymbolInput<'db> {
    pub owner: RuntimeFunctionOwner<'db>,
    pub fallback_symbol: String,
    pub variant_suffix: String,
    pub disambiguator: String,
}

pub(crate) fn assign_function_symbols<'db>(
    db: &'db DriverDataBase,
    inputs: &[FunctionSymbolInput<'db>],
) -> Vec<String> {
    let candidates = inputs
        .iter()
        .map(|input| function_symbol_candidates(db, input))
        .collect::<Vec<_>>();
    let mut selected = vec![0usize; candidates.len()];

    loop {
        let conflicts = symbol_conflicts(&candidates, &selected);
        if conflicts.values().all(|group| group.len() == 1) {
            break;
        }

        let mut changed = false;
        for group in conflicts.values().filter(|group| group.len() > 1) {
            for &idx in group {
                if selected[idx] + 1 < candidates[idx].len() {
                    selected[idx] += 1;
                    changed = true;
                }
            }
        }

        if !changed {
            break;
        }
    }

    let mut symbols = candidates
        .iter()
        .zip(selected)
        .map(|(candidates, selected)| candidates[selected].clone())
        .collect::<Vec<_>>();
    uniquify_function_symbols(&mut symbols, &candidates, inputs);
    symbols
}

fn function_symbol_candidates<'db>(
    db: &'db DriverDataBase,
    input: &FunctionSymbolInput<'db>,
) -> Vec<String> {
    match &input.owner {
        RuntimeFunctionOwner::Semantic(semantic) => {
            semantic_function_symbol_candidates(db, *semantic, &input.variant_suffix)
        }
        RuntimeFunctionOwner::Synthetic(_) => {
            vec![render_raw_symbol(
                &input.fallback_symbol,
                &input.variant_suffix,
            )]
        }
    }
}

fn semantic_function_symbol_candidates<'db>(
    db: &'db DriverDataBase,
    semantic: SemanticInstance<'db>,
    variant_suffix: &str,
) -> Vec<String> {
    let owner = semantic.key(db).owner(db);
    let leaf = semantic_leaf_component(db, owner);
    let generic = generic_component(db, semantic.key(db).subst(db).generic_args(db));
    let owner_context = semantic_owner_context_candidates(db, owner);
    let module_path = module_path_components_for_scope(db, owner.scope());
    let ingot = ingot_component_for_scope(db, owner.scope());

    let mut candidates = Vec::new();
    push_symbol_candidate(&mut candidates, vec![leaf.clone()], variant_suffix);
    if let Some(generic) = &generic {
        push_symbol_candidate(
            &mut candidates,
            vec![leaf.clone(), generic.clone()],
            variant_suffix,
        );
    }

    let mut scoped_leaf = vec![leaf];
    if let Some(generic) = generic {
        scoped_leaf.push(generic);
    }

    if let Some(owner_context) = &owner_context.primary {
        let mut parts = vec![owner_context.clone()];
        parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, parts, variant_suffix);
    }

    let mut module_parts = module_path.clone();
    if let Some(owner_context) = &owner_context.primary {
        module_parts.push(owner_context.clone());
    }
    module_parts.extend(scoped_leaf.clone());
    push_symbol_candidate(&mut candidates, module_parts.clone(), variant_suffix);

    let mut ingot_parts = vec![ingot];
    ingot_parts.extend(module_parts.clone());
    push_symbol_candidate(&mut candidates, ingot_parts, variant_suffix);

    if let Some(fallback_context) = &owner_context.fallback {
        let mut parts = vec![fallback_context.clone()];
        parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, parts, variant_suffix);

        let mut module_parts = module_path;
        module_parts.push(fallback_context.clone());
        module_parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, module_parts.clone(), variant_suffix);

        let mut ingot_parts = vec![ingot_component_for_scope(db, owner.scope())];
        ingot_parts.extend(module_parts);
        push_symbol_candidate(&mut candidates, ingot_parts, variant_suffix);
    }
    candidates
}

fn push_symbol_candidate(candidates: &mut Vec<String>, parts: Vec<String>, variant_suffix: &str) {
    let symbol = render_symbol(parts, variant_suffix);
    if candidates.iter().all(|candidate| *candidate != symbol) {
        candidates.push(symbol);
    }
}

fn render_symbol(parts: Vec<String>, variant_suffix: &str) -> String {
    let mut symbol = parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .map(|part| sanitize_ident_segment(&part))
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(SEGMENT_SEPARATOR);
    if symbol.is_empty() {
        symbol = sanitize_ident_segment("symbol");
    }
    if !variant_suffix.is_empty() {
        symbol.push_str(VARIANT_SEPARATOR);
        symbol.push_str(&sanitize_ident_segment(variant_suffix));
    }
    symbol
}

fn render_raw_symbol(symbol: &str, variant_suffix: &str) -> String {
    render_symbol(vec![symbol.to_string()], variant_suffix)
}

fn sanitize_ident_segment(value: &str) -> String {
    let sanitized = value
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();
    if sanitized
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
    {
        sanitized
    } else {
        format!("_{sanitized}")
    }
}

fn symbol_conflicts(
    candidates: &[Vec<String>],
    selected: &[usize],
) -> BTreeMap<String, Vec<usize>> {
    selected
        .iter()
        .enumerate()
        .fold(BTreeMap::new(), |mut conflicts, (idx, selected)| {
            conflicts
                .entry(candidates[idx][*selected].clone())
                .or_insert_with(Vec::new)
                .push(idx);
            conflicts
        })
}

fn uniquify_function_symbols(
    symbols: &mut [String],
    candidates: &[Vec<String>],
    inputs: &[FunctionSymbolInput<'_>],
) {
    let conflicts = symbols.iter().enumerate().fold(
        BTreeMap::<String, Vec<usize>>::new(),
        |mut conflicts, (idx, symbol)| {
            conflicts.entry(symbol.clone()).or_default().push(idx);
            conflicts
        },
    );
    let mut used = conflicts
        .iter()
        .filter(|(_, group)| group.len() == 1)
        .map(|(symbol, _)| symbol.clone())
        .collect::<FxHashSet<_>>();
    for group in conflicts.values().filter(|group| group.len() > 1) {
        let mut group = group.clone();
        group.sort_by(|lhs, rhs| {
            candidates[*lhs]
                .cmp(&candidates[*rhs])
                .then_with(|| inputs[*lhs].disambiguator.cmp(&inputs[*rhs].disambiguator))
                .then_with(|| lhs.cmp(rhs))
        });
        for idx in group {
            let base = symbols[idx].clone();
            let hash = stable_identity_hash(&inputs[idx].disambiguator);
            let candidate = format!(
                "{base}{FALLBACK_SEPARATOR}{}",
                &hash[..CONFLICT_SUFFIX_HASH_LEN]
            );
            if used.insert(candidate.clone()) {
                symbols[idx] = candidate;
                continue;
            }
            for suffix in 0.. {
                let candidate = format!(
                    "{base}{FALLBACK_SEPARATOR}{}{FALLBACK_SEPARATOR}{suffix}",
                    &hash[..CONFLICT_SUFFIX_HASH_LEN]
                );
                if used.insert(candidate.clone()) {
                    symbols[idx] = candidate;
                    break;
                }
            }
        }
    }
}

fn semantic_leaf_component<'db>(db: &'db DriverDataBase, owner: BodyOwner<'db>) -> String {
    match owner {
        BodyOwner::Func(func) => func
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "__anon".to_string()),
        BodyOwner::Const(const_) => const_
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "__const".to_string()),
        BodyOwner::AnonConstBody { .. } => "__anon_const".to_string(),
        BodyOwner::ContractInit { contract } => format!(
            "__{}_init",
            contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "contract".to_string())
        ),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => format!(
            "__{}_recv_{}_{}",
            contract
                .name(db)
                .to_opt()
                .map(|name| name.data(db).to_string())
                .unwrap_or_else(|| "contract".to_string()),
            recv_idx,
            arm_idx
        ),
        BodyOwner::Closure {
            ty, receiver_mode, ..
        } => closure_symbol_base(db, ty, receiver_mode),
    }
}

fn semantic_owner_context_candidates<'db>(
    db: &'db DriverDataBase,
    owner: BodyOwner<'db>,
) -> OwnerContextCandidates {
    let stable = semantic_owner_context_identity(db, owner);
    let readable = readable_owner_context(db, owner);
    let primary = readable
        .clone()
        .or_else(|| stable.clone().map(shorten_owner_context_hash));
    let fallback = readable
        .zip(stable.as_deref().and_then(short_owner_context_hash))
        .map(|(readable, hash)| format!("{readable}${hash}"))
        .filter(|fallback| Some(fallback) != primary.as_ref());

    OwnerContextCandidates { primary, fallback }
}

fn readable_owner_context<'db>(db: &'db DriverDataBase, owner: BodyOwner<'db>) -> Option<String> {
    let BodyOwner::Func(func) = owner else {
        return None;
    };

    if let Some(impl_) = func.containing_impl(db) {
        return readable_type_component(db, impl_.ty(db)).map(|ty| format!("impl${ty}"));
    }

    if let Some(impl_trait) = func.containing_impl_trait(db) {
        return readable_type_component(db, impl_trait.ty(db)).map(|ty| format!("impl_trait${ty}"));
    }

    None
}

fn readable_type_component<'db>(db: &'db DriverDataBase, ty: TyId<'db>) -> Option<String> {
    let base = ty.base_ty(db);
    match base.data(db) {
        TyData::TyBase(TyBase::Adt(adt)) => adt
            .adt_ref(db)
            .name(db)
            .map(|name| name.data(db).to_string()),
        TyData::TyBase(TyBase::Contract(contract)) => contract
            .name(db)
            .to_opt()
            .map(|name| name.data(db).to_string()),
        TyData::TyBase(TyBase::Prim(_) | TyBase::Func(_) | TyBase::Closure(_))
        | TyData::TyParam(_)
        | TyData::QualifiedTy(_) => {
            let component = base.pretty_print(db).to_string();
            (!component.is_empty()).then_some(component)
        }
        TyData::AssocTy(_)
        | TyData::ConstTy(_)
        | TyData::Never
        | TyData::TyVar(_)
        | TyData::Invalid(_)
        | TyData::TyApp(..) => None,
    }
}

fn shorten_owner_context_hash(context: String) -> String {
    if let Some(hash) = short_owner_context_hash(&context)
        && let Some((kind, _)) = context.split_once('$')
    {
        return format!("{kind}${hash}");
    }
    context
}

fn short_owner_context_hash(context: &str) -> Option<&str> {
    if let Some((kind, hash)) = context.split_once('$')
        && matches!(kind, "impl" | "impl_trait")
        && hash.len() > OWNER_CONTEXT_HASH_LEN
    {
        return Some(&hash[..OWNER_CONTEXT_HASH_LEN]);
    }
    None
}

fn generic_component<'db>(db: &'db DriverDataBase, args: &[TyId<'db>]) -> Option<String> {
    (!args.is_empty()).then(|| {
        let hash = stable_identity_hash(&generic_args_identity(db, args));
        format!("g{}", &hash[..GENERIC_SUFFIX_HASH_LEN])
    })
}

#[cfg(test)]
mod tests {
    use common::InputDb as _;
    use hir::analysis::ty::ty_check::ClosureReceiverMode;
    use url::Url;

    use super::*;

    #[test]
    fn reusable_closure_call_abis_use_mode_aware_symbols() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///reusable_closure_call_abis_use_mode_aware_symbols.fe").unwrap(),
            Some(
                r#"
use core::functional::Fn
use core::functional::FnOnce

struct Boxed {
    value: u256,
}

#[test]
fn reusable_closure_call_abis_use_mode_aware_symbols() {
    let boxed = Boxed { value: 21 }
    let read = |_ unit: own ()| -> u256 { boxed.value }
    let first = read.call(())
    let second = read.call_once(())
    assert(first + second == 42)
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package = mir::build_test_runtime_package(
            &db,
            top_mod,
            Some("reusable_closure_call_abis_use_mode_aware_symbols"),
        )
        .expect("test package should build");
        let functions = package
            .functions(&db)
            .iter()
            .copied()
            .filter(|function| {
                let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                    return false;
                };
                matches!(semantic.key(&db).owner(&db), BodyOwner::Closure { .. })
            })
            .collect::<Vec<_>>();
        assert_eq!(functions.len(), 2, "expected Fn and FnOnce closure bodies");

        let inputs = functions
            .iter()
            .map(|function| FunctionSymbolInput {
                owner: function.owner(&db).clone(),
                fallback_symbol: function.symbol(&db).clone(),
                variant_suffix: String::new(),
                disambiguator: mir::runtime_instance_symbol_key(&db, function.instance(&db)),
            })
            .collect::<Vec<_>>();
        let codegen_symbols = assign_function_symbols(&db, &inputs);

        let mut saw_view = false;
        let mut saw_own = false;
        for (function, codegen_symbol) in functions.iter().zip(codegen_symbols) {
            let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                unreachable!("closure function is semantic");
            };
            let BodyOwner::Closure {
                ty, receiver_mode, ..
            } = semantic.key(&db).owner(&db)
            else {
                unreachable!("filtered to closure functions");
            };
            let expected = closure_symbol_base(&db, ty, receiver_mode);
            assert_eq!(function.symbol(&db), expected.clone());
            assert_eq!(codegen_symbol, expected);
            match receiver_mode {
                ClosureReceiverMode::View => saw_view = true,
                ClosureReceiverMode::Own => saw_own = true,
            }
        }
        assert!(saw_view, "missing reusable Fn body");
        assert!(saw_own, "missing reusable FnOnce body");
    }

    #[test]
    fn closure_symbols_include_their_distinct_body_owners() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///closure_symbols_include_their_distinct_body_owners.fe").unwrap(),
            Some(
                r#"
use core::functional::Fn

#[test]
fn first() {
    let answer = || 1 as u256
    assert(answer.call() == 1)
}

#[test]
fn second() {
    let answer = || 2 as u256
    assert(answer.call() == 2)
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package =
            mir::build_test_runtime_package(&db, top_mod, None).expect("test package should build");
        let closures = package
            .functions(&db)
            .iter()
            .filter_map(|function| {
                let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                    return None;
                };
                let BodyOwner::Closure {
                    ty,
                    def,
                    receiver_mode,
                } = semantic.key(&db).owner(&db)
                else {
                    return None;
                };
                let defining_owner = BodyOwner::from_body(&db, def.body)
                    .expect("runtime closure must have a defining body owner");
                Some((
                    def,
                    defining_owner,
                    closure_symbol_base(&db, ty, receiver_mode),
                ))
            })
            .collect::<Vec<_>>();
        let closure_symbols = closures
            .iter()
            .map(|(_, _, symbol)| symbol)
            .collect::<Vec<_>>();

        assert_eq!(closures.len(), 2, "{closure_symbols:#?}");
        assert_eq!(
            closures[0].0.expr, closures[1].0.expr,
            "the regression requires the closures to share a body-local expression index",
        );
        assert!(
            closures
                .iter()
                .all(|(_, owner, _)| matches!(owner, BodyOwner::Func(_))),
            "function closures must resolve to function body owners: {closures:#?}",
        );
        assert_ne!(
            closures[0].1, closures[1].1,
            "the closures must resolve to distinct function owners",
        );
        assert_eq!(
            closure_symbols.iter().collect::<FxHashSet<_>>().len(),
            closure_symbols.len(),
            "same-position closures in distinct body owners must have distinct stable symbols: {closure_symbols:#?}",
        );
    }

    #[test]
    fn receive_arm_closure_symbols_are_unique_and_order_independent() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///receive_arm_closure_symbols_are_unique_and_order_independent.fe")
                .unwrap(),
            Some(
                r#"
use core::functional::Fn

msg Msg {
    #[selector = 1]
    First -> u256,
    #[selector = 2]
    Second -> u256,
}

pub contract C {
    recv Msg {
        First -> u256 {
            let answer = || 1 as u256
            answer.call()
        }

        Second -> u256 {
            let answer = || 2 as u256
            answer.call()
        }
    }
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package =
            mir::build_runtime_package(&db, top_mod).expect("contract package should build");
        let mut closure_defs = Vec::new();
        let mut closure_owners = Vec::new();
        let mut closure_bases = Vec::new();
        let mut inputs = package
            .functions(&db)
            .iter()
            .filter_map(|function| {
                let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                    return None;
                };
                let BodyOwner::Closure {
                    ty,
                    def,
                    receiver_mode,
                } = semantic.key(&db).owner(&db)
                else {
                    return None;
                };
                let defining_owner = BodyOwner::from_body(&db, def.body)
                    .expect("runtime closure must have a defining body owner");
                let base = closure_symbol_base(&db, ty, receiver_mode);
                assert_eq!(function.symbol(&db), base);
                closure_defs.push(def);
                closure_owners.push(defining_owner);
                closure_bases.push(base);
                Some(FunctionSymbolInput {
                    owner: function.owner(&db).clone(),
                    fallback_symbol: function.symbol(&db).clone(),
                    variant_suffix: String::new(),
                    disambiguator: mir::runtime_instance_symbol_key(&db, function.instance(&db)),
                })
            })
            .collect::<Vec<_>>();

        assert_eq!(inputs.len(), 2);
        assert_eq!(
            closure_defs[0].expr, closure_defs[1].expr,
            "the regression requires the closures to share a body-local expression index",
        );
        assert!(
            closure_owners
                .iter()
                .all(|owner| matches!(owner, BodyOwner::ContractRecvArm { .. })),
            "receive-arm closures must resolve to receive-arm body owners: {closure_owners:#?}",
        );
        assert_ne!(
            closure_owners[0], closure_owners[1],
            "the closures must resolve to distinct receive-arm owners",
        );
        assert_eq!(
            closure_bases.iter().collect::<FxHashSet<_>>().len(),
            closure_bases.len(),
            "same-position closures in distinct receive arms must have distinct stable bases: {closure_bases:#?}",
        );
        assert_eq!(
            inputs
                .iter()
                .map(|input| &input.disambiguator)
                .collect::<FxHashSet<_>>()
                .len(),
            inputs.len(),
            "owner-based closure identities must produce distinct disambiguators",
        );
        let assigned = assign_function_symbols(&db, &inputs);
        assert_eq!(
            assigned.iter().collect::<FxHashSet<_>>().len(),
            assigned.len(),
            "same-position closures in distinct receive arms must have distinct symbols: {assigned:#?}",
        );
        let by_instance = inputs
            .iter()
            .zip(assigned)
            .map(|(input, symbol)| (input.disambiguator.clone(), symbol))
            .collect::<BTreeMap<_, _>>();

        inputs.reverse();
        let reversed = assign_function_symbols(&db, &inputs);
        let reversed_by_instance = inputs
            .iter()
            .zip(reversed)
            .map(|(input, symbol)| (input.disambiguator.clone(), symbol))
            .collect::<BTreeMap<_, _>>();
        assert_eq!(by_instance, reversed_by_instance);
    }

    #[test]
    fn contract_init_closure_symbol_uses_contract_init_owner() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///contract_init_closure_symbol_uses_contract_init_owner.fe").unwrap(),
            Some(
                r#"
use core::functional::Fn

pub contract C {
    init() {
        let answer = || 42 as u256
        assert(answer.call() == 42)
    }
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package =
            mir::build_runtime_package(&db, top_mod).expect("contract package should build");
        let closures = package
            .functions(&db)
            .iter()
            .filter_map(|function| {
                let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                    return None;
                };
                let BodyOwner::Closure {
                    ty,
                    def,
                    receiver_mode,
                } = semantic.key(&db).owner(&db)
                else {
                    return None;
                };
                let defining_owner = BodyOwner::from_body(&db, def.body)
                    .expect("runtime closure must have a defining body owner");
                Some((
                    defining_owner,
                    closure_symbol_base(&db, ty, receiver_mode),
                    function.symbol(&db),
                ))
            })
            .collect::<Vec<_>>();

        assert_eq!(closures.len(), 1, "{closures:#?}");
        assert!(
            matches!(closures[0].0, BodyOwner::ContractInit { .. }),
            "init closure must resolve to its contract-init owner: {closures:#?}",
        );
        assert_eq!(closures[0].1.as_str(), closures[0].2.as_str());
    }

    #[test]
    fn nested_closure_symbols_include_their_distinct_root_body_owners() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///nested_closure_symbols_include_their_distinct_root_body_owners.fe")
                .unwrap(),
            Some(
                r#"
use core::functional::Fn

#[test]
fn first() {
    let outer = || {
        let inner = || 1 as u256
        inner.call()
    }
    assert(outer.call() == 1)
}

#[test]
fn second() {
    let outer = || {
        let inner = || 2 as u256
        inner.call()
    }
    assert(outer.call() == 2)
}
"#
                .to_string(),
            ),
        );
        let top_mod = db.top_mod(file);
        let package =
            mir::build_test_runtime_package(&db, top_mod, None).expect("test package should build");
        let closures = package
            .functions(&db)
            .iter()
            .filter_map(|function| {
                let RuntimeFunctionOwner::Semantic(semantic) = function.owner(&db) else {
                    return None;
                };
                let BodyOwner::Closure {
                    ty,
                    def,
                    receiver_mode,
                } = semantic.key(&db).owner(&db)
                else {
                    return None;
                };
                Some((
                    def,
                    BodyOwner::from_body(&db, def.body)
                        .expect("runtime closure must have a defining body owner"),
                    closure_symbol_base(&db, ty, receiver_mode),
                ))
            })
            .collect::<Vec<_>>();

        assert_eq!(closures.len(), 4, "{closures:#?}");
        assert_eq!(
            closures
                .iter()
                .map(|(_, _, symbol)| symbol)
                .collect::<FxHashSet<_>>()
                .len(),
            closures.len(),
            "every nested closure must have a distinct stable symbol: {closures:#?}",
        );
        assert!(
            closures
                .iter()
                .all(|(_, owner, _)| matches!(owner, BodyOwner::Func(_))),
            "nested closure definitions retain their lexical root function body: {closures:#?}",
        );
        assert_eq!(
            closures
                .iter()
                .map(|(_, owner, _)| owner)
                .collect::<FxHashSet<_>>()
                .len(),
            2,
            "the two root function owners must stay distinct: {closures:#?}",
        );
        assert_eq!(
            closures
                .iter()
                .map(|(def, _, _)| def.expr)
                .collect::<FxHashSet<_>>()
                .len(),
            2,
            "corresponding inner and outer literals must reuse body-local expression indices across owners: {closures:#?}",
        );
    }
}
