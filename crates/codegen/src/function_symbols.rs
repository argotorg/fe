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
        generic_args_identity, ingot_component_for_scope, module_path_components_for_scope,
        semantic_owner_context_identity, stable_identity_fingerprint,
    },
};
use rustc_hash::FxHashSet;
use std::collections::BTreeMap;

const GENERIC_SUFFIX_FINGERPRINT_LEN: usize = 4;
const OWNER_CONTEXT_FINGERPRINT_LEN: usize = 4;
const CONFLICT_SUFFIX_FINGERPRINT_LEN: usize = 4;

struct OwnerContextCandidates {
    primary: Option<String>,
    fallback: Option<String>,
}

pub(crate) struct FunctionSymbolStyle {
    pub segment_separator: &'static str,
    pub variant_separator: &'static str,
    pub fallback_separator: &'static str,
    pub sanitize_segment: fn(&str) -> String,
    pub namespace_key: fn(&str) -> String,
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
    style: &FunctionSymbolStyle,
) -> Vec<String> {
    let candidates = inputs
        .iter()
        .map(|input| function_symbol_candidates(db, input, style))
        .collect::<Vec<_>>();
    let mut selected = vec![0usize; candidates.len()];

    loop {
        let conflicts = symbol_conflicts(&candidates, &selected, style);
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
    uniquify_function_symbols(&mut symbols, style, &candidates, inputs);
    symbols
}

fn function_symbol_candidates<'db>(
    db: &'db DriverDataBase,
    input: &FunctionSymbolInput<'db>,
    style: &FunctionSymbolStyle,
) -> Vec<String> {
    match &input.owner {
        RuntimeFunctionOwner::Semantic(semantic) => {
            semantic_function_symbol_candidates(db, *semantic, &input.variant_suffix, style)
        }
        RuntimeFunctionOwner::Synthetic(_) => {
            vec![render_raw_symbol(
                &input.fallback_symbol,
                &input.variant_suffix,
                style,
            )]
        }
    }
}

fn semantic_function_symbol_candidates<'db>(
    db: &'db DriverDataBase,
    semantic: SemanticInstance<'db>,
    variant_suffix: &str,
    style: &FunctionSymbolStyle,
) -> Vec<String> {
    let owner = semantic.key(db).owner(db);
    let leaf = semantic_leaf_component(db, owner);
    let generic = generic_component(db, semantic.key(db).subst(db).generic_args(db));
    let owner_context = semantic_owner_context_candidates(db, owner);
    let module_path = module_path_components_for_scope(db, owner.scope());
    let ingot = ingot_component_for_scope(db, owner.scope());

    let mut candidates = Vec::new();
    push_symbol_candidate(&mut candidates, vec![leaf.clone()], variant_suffix, style);
    if let Some(generic) = &generic {
        push_symbol_candidate(
            &mut candidates,
            vec![leaf.clone(), generic.clone()],
            variant_suffix,
            style,
        );
    }

    let mut scoped_leaf = vec![leaf];
    if let Some(generic) = generic {
        scoped_leaf.push(generic);
    }

    if let Some(owner_context) = &owner_context.primary {
        let mut parts = vec![owner_context.clone()];
        parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, parts, variant_suffix, style);
    }

    let mut module_parts = module_path.clone();
    if let Some(owner_context) = &owner_context.primary {
        module_parts.push(owner_context.clone());
    }
    module_parts.extend(scoped_leaf.clone());
    push_symbol_candidate(&mut candidates, module_parts.clone(), variant_suffix, style);

    let mut ingot_parts = vec![ingot];
    ingot_parts.extend(module_parts.clone());
    push_symbol_candidate(&mut candidates, ingot_parts, variant_suffix, style);

    if let Some(fallback_context) = &owner_context.fallback {
        let mut parts = vec![fallback_context.clone()];
        parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, parts, variant_suffix, style);

        let mut module_parts = module_path;
        module_parts.push(fallback_context.clone());
        module_parts.extend(scoped_leaf.clone());
        push_symbol_candidate(&mut candidates, module_parts.clone(), variant_suffix, style);

        let mut ingot_parts = vec![ingot_component_for_scope(db, owner.scope())];
        ingot_parts.extend(module_parts);
        push_symbol_candidate(&mut candidates, ingot_parts, variant_suffix, style);
    }
    candidates
}

fn push_symbol_candidate(
    candidates: &mut Vec<String>,
    parts: Vec<String>,
    variant_suffix: &str,
    style: &FunctionSymbolStyle,
) {
    let symbol = render_symbol(parts, variant_suffix, style);
    let emitted = (style.namespace_key)(&symbol);
    if candidates
        .iter()
        .all(|candidate| (style.namespace_key)(candidate) != emitted)
    {
        candidates.push(symbol);
    }
}

fn render_symbol(parts: Vec<String>, variant_suffix: &str, style: &FunctionSymbolStyle) -> String {
    let mut symbol = parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .map(|part| (style.sanitize_segment)(&part))
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(style.segment_separator);
    if symbol.is_empty() {
        symbol = (style.sanitize_segment)("symbol");
    }
    if !variant_suffix.is_empty() {
        symbol.push_str(style.variant_separator);
        symbol.push_str(&(style.sanitize_segment)(variant_suffix));
    }
    symbol
}

fn render_raw_symbol(symbol: &str, variant_suffix: &str, style: &FunctionSymbolStyle) -> String {
    render_symbol(vec![symbol.to_string()], variant_suffix, style)
}

fn symbol_conflicts(
    candidates: &[Vec<String>],
    selected: &[usize],
    style: &FunctionSymbolStyle,
) -> BTreeMap<String, Vec<usize>> {
    selected
        .iter()
        .enumerate()
        .fold(BTreeMap::new(), |mut conflicts, (idx, selected)| {
            conflicts
                .entry((style.namespace_key)(&candidates[idx][*selected]))
                .or_insert_with(Vec::new)
                .push(idx);
            conflicts
        })
}

fn uniquify_function_symbols(
    symbols: &mut [String],
    style: &FunctionSymbolStyle,
    candidates: &[Vec<String>],
    inputs: &[FunctionSymbolInput<'_>],
) {
    let conflicts = symbols.iter().enumerate().fold(
        BTreeMap::<String, Vec<usize>>::new(),
        |mut conflicts, (idx, symbol)| {
            conflicts
                .entry((style.namespace_key)(symbol))
                .or_default()
                .push(idx);
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
            let fingerprint = stable_identity_fingerprint(&inputs[idx].disambiguator);
            let candidate = format!(
                "{base}{}{}",
                style.fallback_separator,
                &fingerprint[..CONFLICT_SUFFIX_FINGERPRINT_LEN]
            );
            if used.insert((style.namespace_key)(&candidate)) {
                symbols[idx] = candidate;
                continue;
            }
            for suffix in 0.. {
                let candidate = format!(
                    "{base}{}{}{}{suffix}",
                    style.fallback_separator,
                    &fingerprint[..CONFLICT_SUFFIX_FINGERPRINT_LEN],
                    style.fallback_separator
                );
                if used.insert((style.namespace_key)(&candidate)) {
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
        .or_else(|| stable.clone().map(shorten_owner_context_fingerprint));
    let fallback = readable
        .zip(stable.as_deref().and_then(short_owner_context_fingerprint))
        .map(|(readable, fingerprint)| format!("{readable}${fingerprint}"))
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
        TyData::TyBase(TyBase::Prim(_) | TyBase::Func(_))
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

fn shorten_owner_context_fingerprint(context: String) -> String {
    if let Some(fingerprint) = short_owner_context_fingerprint(&context)
        && let Some((kind, _)) = context.split_once('$')
    {
        return format!("{kind}${fingerprint}");
    }
    context
}

fn short_owner_context_fingerprint(context: &str) -> Option<&str> {
    if let Some((kind, fingerprint)) = context.split_once('$')
        && matches!(kind, "impl" | "impl_trait")
        && fingerprint.len() > OWNER_CONTEXT_FINGERPRINT_LEN
    {
        return Some(&fingerprint[..OWNER_CONTEXT_FINGERPRINT_LEN]);
    }
    None
}

fn generic_component<'db>(db: &'db DriverDataBase, args: &[TyId<'db>]) -> Option<String> {
    (!args.is_empty()).then(|| {
        let fingerprint = stable_identity_fingerprint(&generic_args_identity(db, args));
        format!("g{}", &fingerprint[..GENERIC_SUFFIX_FINGERPRINT_LEN])
    })
}
