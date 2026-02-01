use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};

use common::indexmap::IndexMap;
use hir::analysis::ty::corelib::{resolve_core_trait, resolve_lib_type_path};
use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    diagnostics::format_diags,
    ty::{
        canonical::Canonicalized,
        const_ty::ConstTyData,
        effects::EffectKeyKind,
        fold::{TyFoldable, TyFolder},
        normalize::normalize_ty,
        trait_def::{TraitInstId, resolve_trait_method_instance},
        trait_resolution::{GoalSatisfiability, PredicateListId, is_goal_satisfiable},
        ty_check::check_func_body,
        ty_def::{TyBase, TyData, TyId},
    },
};
use hir::hir_def::{
    CallableDef, Func, HirIngot, IdentId, PathId, PathKind, item::ItemKind, scope_graph::ScopeId,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    CallOrigin, MirFunction, dedup::deduplicate_mir, ir::AddressSpaceKind, lower::lower_function,
};

/// Walks generic MIR templates, cloning them per concrete substitution so
/// downstream passes only ever see monomorphic MIR.
///
/// Create monomorphic MIR instances for every reachable generic instantiation.
///
/// The input `templates` are lowered once from HIR and may contain generic
/// placeholders. This routine discovers all concrete substitutions reachable
/// from `main`/exported roots, clones the required templates, and performs the
/// type substitution directly on MIR so later passes do not need to reason
/// about generics.
pub(crate) fn monomorphize_functions<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    templates: Vec<MirFunction<'db>>,
) -> Vec<MirFunction<'db>> {
    let mut monomorphizer = Monomorphizer::new(db, templates);
    monomorphizer.seed_roots();
    monomorphizer.process_worklist();
    deduplicate_mir(db, monomorphizer.into_instances())
}

/// Worklist-driven builder that instantiates concrete MIR bodies on demand.
struct Monomorphizer<'db> {
    db: &'db dyn SpannedHirAnalysisDb,
    templates: Vec<MirFunction<'db>>,
    func_index: FxHashMap<TemplateKey<'db>, usize>,
    func_defs: FxHashMap<Func<'db>, CallableDef<'db>>,
    instances: Vec<MirFunction<'db>>,
    instance_map: FxHashMap<InstanceKey<'db>, usize>,
    worklist: VecDeque<usize>,
    current_symbol: Option<String>,
    ambiguous_bases: FxHashSet<String>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct InstanceKey<'db> {
    origin: crate::ir::MirFunctionOrigin<'db>,
    args: Vec<TyId<'db>>,
    receiver_space: Option<AddressSpaceKind>,
    effect_param_space_overrides: Vec<Option<AddressSpaceKind>>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
struct TemplateKey<'db> {
    origin: crate::ir::MirFunctionOrigin<'db>,
    receiver_space: Option<AddressSpaceKind>,
}

/// How a call target should be handled during monomorphization.
#[derive(Clone, Copy)]
enum CallTarget<'db> {
    /// The callee has a body and should be instantiated like any other template.
    Template(Func<'db>),
    /// The callee is a declaration only (e.g. `extern`); no MIR body exists.
    Decl(Func<'db>),
    /// The callee is a MIR-synthetic function.
    Synthetic(crate::ir::MirFunctionOrigin<'db>),
}

fn resolve_default_root_effect_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    let target_ty = resolve_lib_type_path(db, scope, "std::evm::EvmTarget")?;
    let target_trait = resolve_core_trait(db, scope, &["contracts", "Target"])?;
    let inst_target = TraitInstId::new(db, target_trait, vec![target_ty], IndexMap::new());
    let root_ident = IdentId::new(db, "RootEffect".to_owned());
    Some(normalize_ty(
        db,
        TyId::assoc_ty(db, inst_target, root_ident),
        scope,
        assumptions,
    ))
}

impl<'db> InstanceKey<'db> {
    /// Pack a function and its (possibly empty) substitution list for hashing.
    fn new(
        origin: crate::ir::MirFunctionOrigin<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_param_space_overrides: &[Option<AddressSpaceKind>],
    ) -> Self {
        Self {
            origin,
            args: args.to_vec(),
            receiver_space,
            effect_param_space_overrides: effect_param_space_overrides.to_vec(),
        }
    }
}

impl<'db> Monomorphizer<'db> {
    /// Build the bookkeeping structures (template lookup + lowered FuncDef cache).
    fn new(db: &'db dyn SpannedHirAnalysisDb, templates: Vec<MirFunction<'db>>) -> Self {
        let func_index = templates
            .iter()
            .enumerate()
            .map(|(idx, func)| {
                (
                    TemplateKey {
                        origin: func.origin,
                        receiver_space: func.receiver_space,
                    },
                    idx,
                )
            })
            .collect();
        let mut func_defs = FxHashMap::default();
        for func in templates.iter().filter_map(|f| match f.origin {
            crate::ir::MirFunctionOrigin::Hir(func) => Some(func),
            crate::ir::MirFunctionOrigin::Synthetic(_) => None,
        }) {
            if let Some(def) = func.as_callable(db) {
                func_defs.insert(func, def);
            }
        }

        let mut monomorphizer = Self {
            db,
            templates,
            func_index,
            func_defs,
            instances: Vec::new(),
            instance_map: FxHashMap::default(),
            worklist: VecDeque::new(),
            current_symbol: None,
            ambiguous_bases: FxHashSet::default(),
        };
        monomorphizer.ambiguous_bases = monomorphizer.compute_ambiguous_bases();
        monomorphizer
    }

    fn compute_ambiguous_bases(&self) -> FxHashSet<String> {
        let mut qualifiers_by_base: FxHashMap<String, FxHashSet<String>> = FxHashMap::default();
        for template in &self.templates {
            let crate::ir::MirFunctionOrigin::Hir(func) = template.origin else {
                continue;
            };
            let receiver_space = canonicalize_receiver_space(template.receiver_space);
            let base = self.base_name_root_without_disambiguation(func, receiver_space);
            let qualifier = self.function_qualifier(func);
            qualifiers_by_base
                .entry(base)
                .or_default()
                .insert(qualifier);
        }

        qualifiers_by_base
            .into_iter()
            .filter_map(|(base, qualifiers)| (qualifiers.len() > 1).then_some(base))
            .collect()
    }

    /// Instantiate all non-generic templates up front so they are always emitted
    /// (even if they are never referenced by another generic instantiation).
    fn seed_roots(&mut self) {
        for idx in 0..self.templates.len() {
            let origin = self.templates[idx].origin;
            let receiver_space = canonicalize_receiver_space(self.templates[idx].receiver_space);

            if let crate::ir::MirFunctionOrigin::Synthetic(_) = origin {
                let _ = self.ensure_synthetic_instance(origin, receiver_space);
                continue;
            }

            let crate::ir::MirFunctionOrigin::Hir(func) = origin else {
                continue;
            };
            let Some(def) = self.func_defs.get(&func).copied() else {
                continue;
            };

            // Seed non-generic functions immediately so we always emit them.
            let params = def.params(self.db);
            if params.is_empty() {
                let _ = self.ensure_instance(func, &[], receiver_space, &[]);
                continue;
            }

            // Functions with only synthetic "type-effect provider" params should still get a
            // default instance emitted (mirrors the old `effect_kinds = [stor; N]` behavior).
            let provider_param_count = params
                .iter()
                .filter(
                    |ty| matches!(ty.data(self.db), TyData::TyParam(p) if p.is_effect_provider()),
                )
                .count();
            if provider_param_count == 0 || provider_param_count != params.len() {
                continue;
            }

            let stor_ptr_ctor =
                resolve_lib_type_path(self.db, func.scope(), "core::effect_ref::StorPtr")
                    .unwrap_or_else(|| panic!("missing core type `core::effect_ref::StorPtr`"));

            let mut args = Vec::with_capacity(provider_param_count);
            let assumptions = PredicateListId::empty_list(self.db);
            let root_effect_ty = resolve_default_root_effect_ty(self.db, func.scope(), assumptions);
            let mut can_seed = true;
            for binding in func.effect_bindings(self.db) {
                match binding.key_kind {
                    EffectKeyKind::Type => {
                        let Some(ty) = binding.key_ty else {
                            continue;
                        };
                        if !ty.is_star_kind(self.db) {
                            continue;
                        }
                        args.push(TyId::app(self.db, stor_ptr_ctor, ty));
                    }
                    EffectKeyKind::Trait => {
                        let Some(root_effect_ty) = root_effect_ty else {
                            can_seed = false;
                            break;
                        };
                        let Some(key_trait) = binding.key_trait else {
                            can_seed = false;
                            break;
                        };
                        let mut trait_args = key_trait.args(self.db).to_vec();
                        if trait_args.is_empty() {
                            can_seed = false;
                            break;
                        }
                        trait_args[0] = root_effect_ty;
                        let goal = Canonicalized::new(
                            self.db,
                            TraitInstId::new(
                                self.db,
                                key_trait.def(self.db),
                                trait_args,
                                key_trait.assoc_type_bindings(self.db).clone(),
                            ),
                        )
                        .value;
                        let ingot = func.top_mod(self.db).ingot(self.db);
                        if !matches!(
                            is_goal_satisfiable(self.db, ingot, goal, assumptions),
                            GoalSatisfiability::Satisfied(_)
                        ) {
                            can_seed = false;
                            break;
                        }
                        args.push(root_effect_ty);
                    }
                    EffectKeyKind::Other => continue,
                }
            }

            if can_seed && args.len() == provider_param_count {
                let _ = self.ensure_instance(func, &args, receiver_space, &[]);
            }
        }
    }

    /// Drain the worklist by resolving calls in each newly-created instance.
    fn process_worklist(&mut self) {
        let mut iterations: usize = 0;
        while let Some(func_idx) = self.worklist.pop_front() {
            self.current_symbol = Some(self.instances[func_idx].symbol_name.clone());
            iterations += 1;
            if iterations > 100_000 {
                panic!("monomorphization worklist exceeded 100k iterations; possible cycle");
            }
            self.resolve_calls(func_idx);
        }
    }

    /// Inspect every call inside the function at `func_idx` and enqueue its targets.
    fn resolve_calls(&mut self, func_idx: usize) {
        #[allow(clippy::type_complexity)] // TODO: refactor
        let call_sites: Vec<(
            usize,
            Option<usize>,
            CallTarget<'db>,
            Vec<TyId<'db>>,
            Option<AddressSpaceKind>,
            Vec<Option<AddressSpaceKind>>,
        )> = {
            let function = &self.instances[func_idx];
            let mut sites = Vec::new();
            for (bb_idx, block) in function.body.blocks.iter().enumerate() {
                for (inst_idx, inst) in block.insts.iter().enumerate() {
                    if let crate::MirInst::Assign {
                        rvalue: crate::ir::Rvalue::Call(call),
                        ..
                    } = inst
                        && let Some((target_func, args)) = self.resolve_call_target(call)
                    {
                        let effect_param_space_overrides = self.call_effect_param_space_overrides(
                            function,
                            call,
                            target_func,
                            &args,
                        );
                        sites.push((
                            bb_idx,
                            Some(inst_idx),
                            target_func,
                            args,
                            canonicalize_receiver_space(call.receiver_space),
                            effect_param_space_overrides,
                        ));
                    }
                }

                if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                    &block.terminator
                    && let Some((target_func, args)) = self.resolve_call_target(call)
                {
                    let effect_param_space_overrides =
                        self.call_effect_param_space_overrides(function, call, target_func, &args);
                    sites.push((
                        bb_idx,
                        None,
                        target_func,
                        args,
                        canonicalize_receiver_space(call.receiver_space),
                        effect_param_space_overrides,
                    ));
                }
            }
            sites
        };

        let func_item_sites = {
            let function = &self.instances[func_idx];
            function
                .body
                .values
                .iter()
                .enumerate()
                .filter_map(|(value_idx, value)| {
                    if let crate::ValueOrigin::FuncItem(root) = &value.origin {
                        Some((value_idx, root.clone()))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        };

        for (bb_idx, inst_idx, target, args, receiver_space, effect_param_space_overrides) in
            call_sites
        {
            let resolved_name = match target {
                CallTarget::Template(func) => {
                    let (_, symbol) = self
                        .ensure_instance(func, &args, receiver_space, &effect_param_space_overrides)
                        .unwrap_or_else(|| {
                            let name = func.pretty_print_signature(self.db);
                            panic!("failed to instantiate MIR for `{name}`");
                        });
                    Some(symbol)
                }
                CallTarget::Decl(func) => Some(self.mangled_name(
                    func,
                    &args,
                    receiver_space,
                    &effect_param_space_overrides,
                )),
                CallTarget::Synthetic(origin) => {
                    let (_, symbol) = self
                        .ensure_synthetic_instance(origin, receiver_space)
                        .unwrap_or_else(|| {
                            panic!("failed to instantiate synthetic MIR for `{origin:?}`")
                        });
                    Some(symbol)
                }
            };

            if let Some(name) = resolved_name {
                match inst_idx {
                    Some(inst_idx) => {
                        let inst =
                            &mut self.instances[func_idx].body.blocks[bb_idx].insts[inst_idx];
                        if let crate::MirInst::Assign {
                            rvalue: crate::ir::Rvalue::Call(call),
                            ..
                        } = inst
                        {
                            call.resolved_name = Some(name);
                        }
                    }
                    None => {
                        let term = &mut self.instances[func_idx].body.blocks[bb_idx].terminator;
                        if let crate::Terminator::TerminatingCall(
                            crate::ir::TerminatingCall::Call(call),
                        ) = term
                        {
                            call.resolved_name = Some(name);
                        }
                    }
                }
            }
        }

        for (value_idx, target) in func_item_sites {
            let (_, symbol) = match target.origin {
                crate::ir::MirFunctionOrigin::Hir(func) => self
                    .ensure_instance(func, &target.generic_args, None, &[])
                    .unwrap_or_else(|| {
                        let name = func.pretty_print(self.db);
                        panic!("failed to instantiate MIR for `{name}`");
                    }),
                crate::ir::MirFunctionOrigin::Synthetic(_) => self
                    .ensure_synthetic_instance(target.origin, None)
                    .unwrap_or_else(|| {
                        panic!("failed to instantiate synthetic MIR for `{target:?}`")
                    }),
            };
            if let crate::ValueOrigin::FuncItem(target) =
                &mut self.instances[func_idx].body.values[value_idx].origin
            {
                target.symbol = Some(symbol);
            }
        }
    }

    fn ensure_synthetic_instance(
        &mut self,
        origin: crate::ir::MirFunctionOrigin<'db>,
        receiver_space: Option<AddressSpaceKind>,
    ) -> Option<(usize, String)> {
        let receiver_space = canonicalize_receiver_space(receiver_space);
        debug_assert!(
            matches!(origin, crate::ir::MirFunctionOrigin::Synthetic(_)),
            "ensure_synthetic_instance called with non-synthetic origin"
        );

        let key = InstanceKey::new(origin, &[], receiver_space, &[]);
        if let Some(&idx) = self.instance_map.get(&key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let template_idx = *self.func_index.get(&TemplateKey {
            origin,
            receiver_space,
        })?;
        let instance = self.templates[template_idx].clone();
        let symbol = instance.symbol_name.clone();

        let idx = self.instances.len();
        self.instances.push(instance);
        self.instance_map.insert(key, idx);
        self.worklist.push_back(idx);
        Some((idx, symbol))
    }

    /// Ensure a `(func, args)` instance exists, cloning and substituting if needed.
    fn ensure_instance(
        &mut self,
        func: Func<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_param_space_overrides: &[Option<AddressSpaceKind>],
    ) -> Option<(usize, String)> {
        let receiver_space = canonicalize_receiver_space(receiver_space);
        let norm_scope = crate::ty::normalization_scope_for_args(self.db, func, args);
        let assumptions = PredicateListId::empty_list(self.db);
        let normalized_args: Vec<_> = args
            .iter()
            .copied()
            .map(|ty| normalize_ty(self.db, ty, norm_scope, assumptions))
            .collect();

        let normalized_effect_param_space_overrides =
            self.normalize_effect_param_space_overrides(func, effect_param_space_overrides);

        let key = InstanceKey::new(
            crate::ir::MirFunctionOrigin::Hir(func),
            &normalized_args,
            receiver_space,
            &normalized_effect_param_space_overrides,
        );
        if let Some(&idx) = self.instance_map.get(&key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let symbol_name = self.mangled_name(
            func,
            &normalized_args,
            receiver_space,
            &normalized_effect_param_space_overrides,
        );

        let mut instance = if args.is_empty() && normalized_effect_param_space_overrides.is_empty()
        {
            let template_idx = self.ensure_template(func, receiver_space)?;
            let mut instance = self.templates[template_idx].clone();
            instance.receiver_space = receiver_space;
            instance.symbol_name = symbol_name.clone();
            self.apply_substitution(&mut instance);
            instance
        } else {
            let (diags, typed_body) = check_func_body(self.db, func);
            if !diags.is_empty() {
                let name = func.pretty_print_signature(self.db);
                let rendered = format_diags(self.db, diags);
                panic!("analysis errors while lowering `{name}`:\n{rendered}");
            }
            let mut folder = ParamSubstFolder {
                args: &normalized_args,
            };
            let typed_body = typed_body.clone().fold_with(self.db, &mut folder);

            // After substitution, normalize any remaining associated types.
            let mut normalizer = NormalizeFolder {
                scope: norm_scope,
                assumptions,
            };
            let typed_body = typed_body.fold_with(self.db, &mut normalizer);
            let mut instance = lower_function(
                self.db,
                func,
                typed_body,
                receiver_space,
                normalized_args.clone(),
                normalized_effect_param_space_overrides.clone(),
            )
            .unwrap_or_else(|err| {
                let name = func.pretty_print_signature(self.db);
                panic!("failed to instantiate MIR for `{name}`: {err}");
            });
            instance.receiver_space = receiver_space;
            instance.symbol_name = symbol_name.clone();
            instance
        };

        let ret_ty = CallableDef::Func(func)
            .ret_ty(self.db)
            .instantiate(self.db, &normalized_args);
        let ret_ty = normalize_ty(self.db, ret_ty, norm_scope, assumptions);
        instance.ret_ty = ret_ty;
        instance.returns_value = !crate::layout::is_zero_sized_ty(self.db, ret_ty);
        instance.generic_args = normalized_args;

        let idx = self.instances.len();
        let symbol = instance.symbol_name.clone();
        self.instances.push(instance);
        self.instance_map.insert(key, idx);
        self.worklist.push_back(idx);
        Some((idx, symbol))
    }

    /// Returns the concrete HIR function targeted by the given call, accounting for trait impls.
    fn resolve_call_target(
        &self,
        call: &CallOrigin<'db>,
    ) -> Option<(CallTarget<'db>, Vec<TyId<'db>>)> {
        let hir_target = call.hir_target.as_ref()?;
        let base_args = hir_target.generic_args.clone();
        if let Some(inst) = hir_target.trait_inst {
            let method_name = call
                .hir_target
                .as_ref()
                .expect("trait method call missing hir target")
                .callable_def
                .name(self.db)
                .expect("trait method call missing name");
            if let Some(origin) = self.resolve_contract_metadata_call(inst, method_name) {
                return Some((CallTarget::Synthetic(origin), Vec::new()));
            }
            let trait_arg_len = inst.args(self.db).len();
            if base_args.len() < trait_arg_len {
                let inst_desc = inst.pretty_print(self.db, false);
                let name = method_name.data(self.db);
                panic!(
                    "trait method `{name}` args too short for `{inst_desc}`: got {}, expected at least {}",
                    base_args.len(),
                    trait_arg_len
                );
            }
            if let Some((func, impl_args)) =
                resolve_trait_method_instance(self.db, inst, method_name)
            {
                let mut resolved_args = impl_args;
                resolved_args.extend_from_slice(&base_args[trait_arg_len..]);
                return Some((CallTarget::Template(func), resolved_args));
            }

            if let CallableDef::Func(func) = hir_target.callable_def
                && func.body(self.db).is_some()
            {
                return Some((CallTarget::Template(func), base_args));
            }

            let inst_desc = inst.pretty_print(self.db, true);
            let name = method_name.data(self.db);
            let current = self
                .current_symbol
                .as_deref()
                .unwrap_or("<unknown function>");
            panic!(
                "failed to resolve trait method `{name}` for `{inst_desc}` while lowering `{current}` (no impl and no default)"
            );
        }

        if let CallableDef::Func(func) = hir_target.callable_def {
            if func.body(self.db).is_some() {
                return Some((CallTarget::Template(func), base_args));
            }
            if let Some(parent) = func.scope().parent(self.db)
                && let ScopeId::Item(item) = parent
                && matches!(item, ItemKind::Trait(_) | ItemKind::ImplTrait(_))
            {
                let name = func.pretty_print_signature(self.db);
                panic!("unresolved trait method `{name}` during monomorphization");
            }
            return Some((CallTarget::Decl(func), base_args));
        }
        None
    }

    fn resolve_contract_metadata_call(
        &self,
        inst: hir::analysis::ty::trait_def::TraitInstId<'db>,
        method_name: hir::hir_def::IdentId<'db>,
    ) -> Option<crate::ir::MirFunctionOrigin<'db>> {
        let trait_def = inst.def(self.db);
        let trait_name = trait_def.name(self.db).to_opt()?;
        if trait_name.data(self.db) != "Contract" {
            return None;
        }
        if trait_def.top_mod(self.db).ingot(self.db).kind(self.db) != common::ingot::IngotKind::Std
        {
            return None;
        }

        let contract = inst.args(self.db).first().copied()?.as_contract(self.db)?;
        let synth = match method_name.data(self.db).as_str() {
            "init_code_offset" => crate::ir::SyntheticId::ContractInitCodeOffset(contract),
            "init_code_len" => crate::ir::SyntheticId::ContractInitCodeLen(contract),
            _ => return None,
        };
        Some(crate::ir::MirFunctionOrigin::Synthetic(synth))
    }

    /// Substitute concrete type arguments directly into the MIR body values.
    fn apply_substitution(&self, function: &mut MirFunction<'db>) {
        let mut folder = ParamSubstFolder {
            args: &function.generic_args,
        };

        for value in &mut function.body.values {
            value.ty = value.ty.fold_with(self.db, &mut folder);
            if let crate::ValueOrigin::FuncItem(target) = &mut value.origin {
                target.generic_args = target
                    .generic_args
                    .iter()
                    .map(|ty| ty.fold_with(self.db, &mut folder))
                    .collect();
                target.symbol = match target.origin {
                    crate::ir::MirFunctionOrigin::Hir(func) => {
                        Some(self.mangled_name(func, &target.generic_args, None, &[]))
                    }
                    crate::ir::MirFunctionOrigin::Synthetic(_) => self
                        .func_index
                        .get(&TemplateKey {
                            origin: target.origin,
                            receiver_space: None,
                        })
                        .map(|idx| self.templates[*idx].symbol_name.clone()),
                };
            }
        }

        for block in &mut function.body.blocks {
            for inst in &mut block.insts {
                if let crate::MirInst::Assign {
                    rvalue: crate::ir::Rvalue::Call(call),
                    ..
                } = inst
                {
                    if let Some(target) = &mut call.hir_target {
                        target.generic_args = target
                            .generic_args
                            .iter()
                            .map(|ty| ty.fold_with(self.db, &mut folder))
                            .collect();
                        if let Some(inst) = target.trait_inst {
                            target.trait_inst = Some(inst.fold_with(self.db, &mut folder));
                        }
                    }
                    call.resolved_name = None;
                }
            }

            if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                &mut block.terminator
            {
                if let Some(target) = &mut call.hir_target {
                    target.generic_args = target
                        .generic_args
                        .iter()
                        .map(|ty| ty.fold_with(self.db, &mut folder))
                        .collect();
                    if let Some(inst) = target.trait_inst {
                        target.trait_inst = Some(inst.fold_with(self.db, &mut folder));
                    }
                }
                call.resolved_name = None;
            }
        }
    }

    /// Produce a globally unique (yet mostly readable) symbol name per instance.
    fn mangled_name(
        &self,
        func: Func<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_param_space_overrides: &[Option<AddressSpaceKind>],
    ) -> String {
        let receiver_space = canonicalize_receiver_space(receiver_space);
        let mut base = self.base_name_root_without_disambiguation(func, receiver_space);
        if self.ambiguous_bases.contains(&base) {
            let qualifier = self.function_qualifier(func);
            base = format!("{qualifier}_{base}");
        }

        let effect_suffix = effect_param_space_suffix(effect_param_space_overrides);
        if !effect_suffix.is_empty() {
            base = format!("{base}_{effect_suffix}");
        }

        if args.is_empty() {
            return base;
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let mut parts = Vec::with_capacity(args.len());
        for ty in args {
            let pretty = self.type_mangle_component(*ty);
            pretty.hash(&mut hasher);
            parts.push(sanitize_symbol_component(&pretty));
        }
        let hash = hasher.finish();
        let suffix = parts.join("_");
        format!("{base}__{suffix}__{hash:08x}")
    }

    fn module_path_segments(&self, mut scope: ScopeId<'db>) -> Vec<String> {
        let ingot = scope.ingot(self.db);
        let root_mod = ingot.root_mod(self.db);

        let mut parts = Vec::new();
        while let Some(parent) = scope.parent_module(self.db) {
            match parent {
                ScopeId::Item(ItemKind::Mod(mod_)) => {
                    if let Some(name) = mod_.name(self.db).to_opt() {
                        parts.push(name.data(self.db).to_string());
                    }
                }
                ScopeId::Item(ItemKind::TopMod(top_mod)) => {
                    if top_mod != root_mod {
                        parts.push(top_mod.name(self.db).data(self.db).to_string());
                    }
                }
                _ => {}
            }
            scope = parent;
        }

        parts.reverse();
        parts
    }

    fn qualified_path_hash(&self, parts: &[String]) -> u64 {
        stable_hash(parts)
    }

    fn ty_identity_hash(&self, ty: TyId<'db>) -> u64 {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Adt(adt)) => {
                let name = adt
                    .adt_ref(self.db)
                    .name(self.db)
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let mut parts = self.module_path_segments(adt.scope(self.db));
                parts.push(name);
                self.qualified_path_hash(&parts)
            }
            TyData::TyBase(TyBase::Contract(contract)) => {
                let name = contract
                    .name(self.db)
                    .to_opt()
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let mut parts = self.module_path_segments(contract.scope());
                parts.push(name);
                self.qualified_path_hash(&parts)
            }
            _ => stable_hash(&ty.pretty_print(self.db).to_string()),
        }
    }

    fn trait_ref_identity_hash(&self, trait_ref: hir::hir_def::TraitRefId<'db>) -> Option<u64> {
        let path = trait_ref.path(self.db).to_opt()?;
        let parts = self.path_ident_segments(path);
        Some(self.qualified_path_hash(&parts))
    }

    fn path_ident_segments(&self, path: PathId<'db>) -> Vec<String> {
        let mut parts = Vec::new();
        let mut current = Some(path);
        while let Some(path) = current {
            match path.kind(self.db) {
                PathKind::Ident { ident, .. } => {
                    if let Some(ident) = ident.to_opt() {
                        parts.push(ident.data(self.db).to_string());
                    }
                }
                PathKind::QualifiedType { .. } => {}
            }
            current = path.parent(self.db);
        }
        parts.reverse();
        parts
    }

    fn type_mangle_component(&self, ty: TyId<'db>) -> String {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Func(callable)) => match callable {
                CallableDef::Func(func) => {
                    let name = func
                        .name(self.db)
                        .to_opt()
                        .map(|ident| ident.data(self.db).to_string())
                        .unwrap_or_else(|| "<unknown>".to_string());
                    let mut parts = self.module_path_segments(func.scope());
                    parts.push(name.clone());
                    let hash = self.qualified_path_hash(&parts);
                    format!("fn {name}_h{hash:08x}")
                }
                CallableDef::VariantCtor(_) => ty.pretty_print(self.db).to_string(),
            },
            TyData::TyBase(TyBase::Adt(adt)) => {
                let name = adt
                    .adt_ref(self.db)
                    .name(self.db)
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let hash = self.ty_identity_hash(ty);
                format!("{name}_h{hash:08x}")
            }
            TyData::TyBase(TyBase::Contract(contract)) => {
                let name = contract
                    .name(self.db)
                    .to_opt()
                    .map(|id| id.data(self.db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let hash = self.ty_identity_hash(ty);
                format!("{name}_h{hash:08x}")
            }
            _ => ty.pretty_print(self.db).to_string(),
        }
    }

    fn base_name_root_without_disambiguation(
        &self,
        func: Func<'db>,
        receiver_space: Option<AddressSpaceKind>,
    ) -> String {
        let mut base = func
            .name(self.db)
            .to_opt()
            .map(|ident| ident.data(self.db).to_string())
            .unwrap_or_else(|| "<anonymous>".into());
        if let Some(prefix) = self.associated_prefix(func) {
            base = format!("{prefix}_{base}");
        }
        if let Some(space) = receiver_space {
            let suffix = match space {
                AddressSpaceKind::Memory => "mem",
                AddressSpaceKind::Calldata => "calldata",
                AddressSpaceKind::Storage => "stor",
                AddressSpaceKind::TransientStorage => "tstor",
            };
            base = format!("{base}_{suffix}");
        }
        base
    }

    fn function_qualifier(&self, func: Func<'db>) -> String {
        let parts = self.module_path_segments(func.scope());
        if parts.is_empty() {
            return "root".to_string();
        }

        let human = sanitize_symbol_component(&parts.join("_")).to_lowercase();
        let hash = self.qualified_path_hash(&parts);
        format!("{human}_h{hash:08x}")
    }

    /// Returns a sanitized prefix for associated functions/methods based on their owner.
    fn associated_prefix(&self, func: Func<'db>) -> Option<String> {
        let parent = func.scope().parent(self.db)?;
        let ScopeId::Item(item) = parent else {
            return None;
        };
        if let ItemKind::Impl(impl_block) = item {
            let ty = impl_block.ty(self.db);
            if ty.has_invalid(self.db) {
                return None;
            }
            let ty_name = sanitize_symbol_component(ty.pretty_print(self.db)).to_lowercase();
            let hash = self.ty_identity_hash(ty);
            Some(format!("{ty_name}_h{hash:08x}"))
        } else if let ItemKind::ImplTrait(impl_trait) = item {
            let ty = impl_trait.ty(self.db);
            if ty.has_invalid(self.db) {
                return None;
            }
            let self_name = sanitize_symbol_component(ty.pretty_print(self.db)).to_lowercase();
            let self_hash = self.ty_identity_hash(ty);
            let self_part = format!("{self_name}_h{self_hash:08x}");

            let trait_name = impl_trait
                .hir_trait_ref(self.db)
                .to_opt()
                .and_then(|trait_ref| trait_ref.path(self.db).to_opt())
                .and_then(|path| path.segment(self.db, path.segment_index(self.db)))
                .and_then(|segment| match segment.kind(self.db) {
                    PathKind::Ident { ident, .. } => ident.to_opt(),
                    PathKind::QualifiedType { .. } => None,
                })
                .map(|ident| sanitize_symbol_component(ident.data(self.db)).to_lowercase())
                .unwrap_or_else(|| "trait".to_string());

            let trait_hash = impl_trait
                .hir_trait_ref(self.db)
                .to_opt()
                .and_then(|trait_ref| self.trait_ref_identity_hash(trait_ref))
                .unwrap_or(0);
            let trait_part = format!("{trait_name}_h{trait_hash:08x}");

            Some(format!("{self_part}_{trait_part}"))
        } else {
            None
        }
    }

    fn into_instances(self) -> Vec<MirFunction<'db>> {
        self.instances
    }

    /// Ensure we have lowered MIR for `func`, lowering on demand for dependency ingots.
    fn ensure_template(
        &mut self,
        func: Func<'db>,
        receiver_space: Option<AddressSpaceKind>,
    ) -> Option<usize> {
        let receiver_space = canonicalize_receiver_space(receiver_space);
        let key = TemplateKey {
            origin: crate::ir::MirFunctionOrigin::Hir(func),
            receiver_space,
        };
        if let Some(&idx) = self.func_index.get(&key) {
            return Some(idx);
        }

        let (diags, typed_body) = check_func_body(self.db, func);
        if !diags.is_empty() {
            let name = func.pretty_print_signature(self.db);
            let rendered = format_diags(self.db, diags);
            panic!("analysis errors while lowering `{name}`:\n{rendered}");
        }
        let lowered = lower_function(
            self.db,
            func,
            typed_body.clone(),
            receiver_space,
            Vec::new(),
            Vec::new(),
        )
        .ok()?;
        let idx = self.templates.len();
        self.templates.push(lowered);
        self.func_index.insert(key, idx);
        if let Some(def) = func.as_callable(self.db) {
            self.func_defs.insert(func, def);
        }
        Some(idx)
    }

    fn call_effect_param_space_overrides(
        &self,
        caller: &MirFunction<'db>,
        call: &CallOrigin<'db>,
        target: CallTarget<'db>,
        args: &[TyId<'db>],
    ) -> Vec<Option<AddressSpaceKind>> {
        let func = match target {
            CallTarget::Template(func) | CallTarget::Decl(func) => func,
            CallTarget::Synthetic(_) => return Vec::new(),
        };

        if call.effect_args.is_empty() {
            return Vec::new();
        }

        let effect_count = func.effect_params(self.db).count();
        let mut overrides = vec![None; effect_count];

        let core = crate::core_lib::CoreLib::new(self.db, func.scope());
        let provider_arg_idx_by_effect =
            hir::analysis::ty::effects::place_effect_provider_param_index_map(self.db, func);

        for (param_ord, effect) in func.effect_params(self.db).enumerate() {
            let effect_idx = effect.index();
            let Some(provider_arg_idx) = provider_arg_idx_by_effect
                .get(effect_idx)
                .copied()
                .flatten()
            else {
                continue;
            };
            let Some(provider_ty) = args.get(provider_arg_idx).copied() else {
                continue;
            };

            if !matches!(
                crate::repr::repr_kind_for_ty(self.db, &core, provider_ty),
                crate::repr::ReprKind::Ref
            ) {
                continue;
            }

            // Effect pointer providers (e.g. `MemPtr`/`StorPtr`/`EffectHandle`) already encode
            // their address space in the type.
            if crate::repr::effect_provider_space_for_ty(self.db, &core, provider_ty).is_some() {
                continue;
            }

            let Some(&effect_arg_value) = call.effect_args.get(param_ord) else {
                continue;
            };
            let Some(space) = crate::ir::try_value_address_space_in(
                &caller.body.values,
                &caller.body.locals,
                effect_arg_value,
            ) else {
                continue;
            };

            // Only specialize when the provider is not memory-backed.
            if !matches!(space, AddressSpaceKind::Memory) {
                overrides[effect_idx] = Some(space);
            }
        }

        overrides
    }

    fn normalize_effect_param_space_overrides(
        &self,
        func: Func<'db>,
        overrides: &[Option<AddressSpaceKind>],
    ) -> Vec<Option<AddressSpaceKind>> {
        if overrides.is_empty() || overrides.iter().all(Option::is_none) {
            return Vec::new();
        }

        let effect_count = func.effect_params(self.db).count();
        let mut normalized = vec![None; effect_count];
        let len = std::cmp::min(effect_count, overrides.len());
        normalized[..len].copy_from_slice(&overrides[..len]);
        normalized
    }
}

/// Simple folder that replaces `TyParam` occurrences with the concrete args for
/// the instance under construction.
struct ParamSubstFolder<'db, 'a> {
    args: &'a [TyId<'db>],
}

impl<'db> TyFolder<'db> for ParamSubstFolder<'db, '_> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        match ty.data(db) {
            TyData::TyParam(param) => self.args.get(param.idx).copied().unwrap_or(ty),
            TyData::ConstTy(const_ty) => {
                if let ConstTyData::TyParam(param, _) = const_ty.data(db) {
                    return self.args.get(param.idx).copied().unwrap_or(ty);
                }
                ty.super_fold_with(db, self)
            }
            _ => ty.super_fold_with(db, self),
        }
    }
}

struct NormalizeFolder<'db> {
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
}

impl<'db> TyFolder<'db> for NormalizeFolder<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(db, ty, self.scope, self.assumptions)
    }
}

/// Replace any non-alphanumeric characters with `_` so the mangled symbol is a
/// valid Yul identifier while remaining somewhat recognizable.
fn sanitize_symbol_component(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

fn stable_hash<T: Hash + ?Sized>(value: &T) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn canonicalize_receiver_space(
    receiver_space: Option<AddressSpaceKind>,
) -> Option<AddressSpaceKind> {
    match receiver_space {
        None | Some(AddressSpaceKind::Memory) => None,
        Some(space) => Some(space),
    }
}

fn effect_param_space_suffix(spaces: &[Option<AddressSpaceKind>]) -> String {
    spaces
        .iter()
        .enumerate()
        .filter_map(|(idx, space)| space.map(|space| (idx, space)))
        .map(|(idx, space)| {
            let suffix = match space {
                AddressSpaceKind::Memory => "mem",
                AddressSpaceKind::Calldata => "calldata",
                AddressSpaceKind::Storage => "stor",
                AddressSpaceKind::TransientStorage => "tstor",
            };
            format!("eff{idx}_{suffix}")
        })
        .collect::<Vec<_>>()
        .join("_")
}
