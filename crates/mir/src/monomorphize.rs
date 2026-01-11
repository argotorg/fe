use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};

use hir::analysis::ty::corelib::resolve_lib_type_path;
use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    diagnostics::format_diags,
    ty::{
        const_ty::ConstTyData,
        fold::{TyFoldable, TyFolder},
        normalize::normalize_ty,
        trait_def::resolve_trait_method_instance,
        trait_resolution::PredicateListId,
        ty_check::check_func_body,
        ty_def::{TyData, TyId},
    },
};
use hir::hir_def::{CallableDef, Func, PathKind, item::ItemKind, scope_graph::ScopeId};
use rustc_hash::FxHashMap;

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
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct InstanceKey<'db> {
    origin: crate::ir::MirFunctionOrigin<'db>,
    args: Vec<TyId<'db>>,
    receiver_space: Option<AddressSpaceKind>,
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

impl<'db> InstanceKey<'db> {
    /// Pack a function and its (possibly empty) substitution list for hashing.
    fn new(
        origin: crate::ir::MirFunctionOrigin<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
    ) -> Self {
        Self {
            origin,
            args: args.to_vec(),
            receiver_space,
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

        Self {
            db,
            templates,
            func_index,
            func_defs,
            instances: Vec::new(),
            instance_map: FxHashMap::default(),
            worklist: VecDeque::new(),
            current_symbol: None,
        }
    }

    /// Instantiate all non-generic templates up front so they are always emitted
    /// (even if they are never referenced by another generic instantiation).
    fn seed_roots(&mut self) {
        for idx in 0..self.templates.len() {
            let origin = self.templates[idx].origin;
            let receiver_space = self.templates[idx].receiver_space;

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
                let _ = self.ensure_instance(func, &[], receiver_space);
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
            for effect in func.effect_params(self.db) {
                let Some(key_path) = effect.key_path(self.db) else {
                    continue;
                };
                let Ok(path_res) = hir::analysis::name_resolution::resolve_path(
                    self.db,
                    key_path,
                    func.scope(),
                    assumptions,
                    false,
                ) else {
                    continue;
                };
                let target_ty = match path_res {
                    hir::analysis::name_resolution::PathRes::Ty(ty)
                    | hir::analysis::name_resolution::PathRes::TyAlias(_, ty) => ty,
                    _ => continue,
                };
                if !target_ty.is_star_kind(self.db) {
                    continue;
                }
                args.push(TyId::app(self.db, stor_ptr_ctor, target_ty));
            }

            if args.len() == provider_param_count {
                let _ = self.ensure_instance(func, &args, receiver_space);
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
                        sites.push((
                            bb_idx,
                            Some(inst_idx),
                            target_func,
                            args,
                            call.receiver_space,
                        ));
                    }
                }

                if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                    &block.terminator
                    && let Some((target_func, args)) = self.resolve_call_target(call)
                {
                    sites.push((bb_idx, None, target_func, args, call.receiver_space));
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

        for (bb_idx, inst_idx, target, args, receiver_space) in call_sites {
            let resolved_name = match target {
                CallTarget::Template(func) => {
                    let (_, symbol) = self
                        .ensure_instance(func, &args, receiver_space)
                        .unwrap_or_else(|| {
                            let name = func.pretty_print_signature(self.db);
                            panic!("failed to instantiate MIR for `{name}`");
                        });
                    Some(symbol)
                }
                CallTarget::Decl(func) => Some(self.mangled_name(func, &args, receiver_space)),
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
                    .ensure_instance(func, &target.generic_args, None)
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
        debug_assert!(
            matches!(origin, crate::ir::MirFunctionOrigin::Synthetic(_)),
            "ensure_synthetic_instance called with non-synthetic origin"
        );

        let key = InstanceKey::new(origin, &[], receiver_space);
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
    ) -> Option<(usize, String)> {
        let norm_scope = crate::ty::normalization_scope_for_args(self.db, func, args);
        let assumptions = PredicateListId::empty_list(self.db);
        let normalized_args: Vec<_> = args
            .iter()
            .copied()
            .map(|ty| normalize_ty(self.db, ty, norm_scope, assumptions))
            .collect();

        let key = InstanceKey::new(
            crate::ir::MirFunctionOrigin::Hir(func),
            &normalized_args,
            receiver_space,
        );
        if let Some(&idx) = self.instance_map.get(&key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let symbol_name = self.mangled_name(func, &normalized_args, receiver_space);

        let mut instance = if args.is_empty() {
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
                        Some(self.mangled_name(func, &target.generic_args, None))
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

        if args.is_empty() {
            return base;
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let mut parts = Vec::with_capacity(args.len());
        for ty in args.iter().copied() {
            let pretty = ty.pretty_print(self.db);
            pretty.hash(&mut hasher);
            parts.push(sanitize_symbol_component(pretty));
        }
        if parts.is_empty() {
            return base;
        }
        let hash = hasher.finish();
        let suffix = parts.join("_");
        format!("{base}__{suffix}__{hash:08x}")
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
            Some(sanitize_symbol_component(ty.pretty_print(self.db)).to_lowercase())
        } else if let ItemKind::ImplTrait(impl_trait) = item {
            let ty = impl_trait.ty(self.db);
            if ty.has_invalid(self.db) {
                return None;
            }
            let self_part = sanitize_symbol_component(ty.pretty_print(self.db)).to_lowercase();
            let trait_part = impl_trait
                .hir_trait_ref(self.db)
                .to_opt()
                .and_then(|trait_ref| trait_ref.path(self.db).to_opt())
                .and_then(|path| path.segment(self.db, path.segment_index(self.db)))
                .and_then(|segment| match segment.kind(self.db) {
                    PathKind::Ident { ident, .. } => ident.to_opt(),
                    PathKind::QualifiedType { .. } => None,
                })
                .map(|ident| sanitize_symbol_component(ident.data(self.db)).to_lowercase());
            let prefix = if let Some(trait_part) = trait_part {
                format!("{self_part}_{trait_part}")
            } else {
                self_part
            };
            Some(prefix)
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
