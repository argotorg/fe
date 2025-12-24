use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};

use hir::analysis::{
    HirAnalysisDb,
    diagnostics::SpannedHirAnalysisDb,
    ty::{
        fold::{TyFoldable, TyFolder},
        trait_def::resolve_trait_method_instance,
        ty_check::check_func_body,
        ty_def::{TyData, TyId},
    },
};
use hir::hir_def::{CallableDef, Func, PathKind, item::ItemKind, scope_graph::ScopeId};
use rustc_hash::FxHashMap;

use crate::{
    CallOrigin, MirFunction, dedup::deduplicate_mir, ir::AddressSpaceKind, ir::EffectProviderKind,
    lower::lower_function,
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
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct InstanceKey<'db> {
    func: Func<'db>,
    args: Vec<TyId<'db>>,
    receiver_space: Option<AddressSpaceKind>,
    effect_kinds: Vec<EffectProviderKind>,
}
#[derive(Clone, PartialEq, Eq, Hash)]
struct TemplateKey<'db> {
    func: Func<'db>,
    receiver_space: Option<AddressSpaceKind>,
    effect_kinds: Vec<EffectProviderKind>,
}

/// How a call target should be handled during monomorphization.
#[derive(Clone, Copy)]
enum CallTarget<'db> {
    /// The callee has a body and should be instantiated like any other template.
    Template(Func<'db>),
    /// The callee is a declaration only (e.g. `extern`); no MIR body exists.
    Decl(Func<'db>),
}

impl<'db> InstanceKey<'db> {
    /// Pack a function and its (possibly empty) substitution list for hashing.
    fn new(
        func: Func<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_kinds: &[EffectProviderKind],
    ) -> Self {
        Self {
            func,
            args: args.to_vec(),
            receiver_space,
            effect_kinds: effect_kinds.to_vec(),
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
                        func: func.func,
                        receiver_space: func.receiver_space,
                        effect_kinds: func.effect_provider_kinds.clone(),
                    },
                    idx,
                )
            })
            .collect();
        let mut func_defs = FxHashMap::default();
        for func in templates.iter().map(|f| f.func) {
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
        }
    }

    /// Instantiate all non-generic templates up front so they are always emitted
    /// (even if they are never referenced by another generic instantiation).
    fn seed_roots(&mut self) {
        for idx in 0..self.templates.len() {
            let func = self.templates[idx].func;
            let should_seed = self
                .func_defs
                .get(&func)
                .is_none_or(|def| def.params(self.db).is_empty());
            if should_seed {
                // Seed non-generic functions immediately so we always emit them.
                let receiver_space = self.templates[idx].receiver_space;
                let default_effects =
                    vec![EffectProviderKind::Storage; func.effect_params(self.db).count()];
                let _ = self.ensure_instance(func, &[], receiver_space, &default_effects);
            }
        }
    }

    /// Drain the worklist by resolving calls in each newly-created instance.
    fn process_worklist(&mut self) {
        let mut iterations: usize = 0;
        while let Some(func_idx) = self.worklist.pop_front() {
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
            Vec<EffectProviderKind>,
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
                            call.effect_kinds.clone(),
                        ));
                    }
                }

                if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                    &block.terminator
                    && let Some((target_func, args)) = self.resolve_call_target(call)
                {
                    sites.push((
                        bb_idx,
                        None,
                        target_func,
                        args,
                        call.receiver_space,
                        call.effect_kinds.clone(),
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

        for (bb_idx, inst_idx, target, args, receiver_space, effect_kinds) in call_sites {
            let resolved_name = match target {
                CallTarget::Template(func) => {
                    let (_, symbol) = self
                        .ensure_instance(func, &args, receiver_space, &effect_kinds)
                        .unwrap_or_else(|| {
                            let name = func.pretty_print_signature(self.db);
                            panic!("failed to instantiate MIR for `{name}`");
                        });
                    Some(symbol)
                }
                CallTarget::Decl(func) => {
                    Some(self.mangled_name(func, &args, receiver_space, &effect_kinds))
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
            let (_, symbol) = self
                .ensure_instance(target.func, &target.generic_args, None, &[])
                .unwrap_or_else(|| {
                    let name = target.func.pretty_print(self.db);
                    panic!("failed to instantiate MIR for `{name}`");
                });
            if let crate::ValueOrigin::FuncItem(target) =
                &mut self.instances[func_idx].body.values[value_idx].origin
            {
                target.symbol = Some(symbol);
            }
        }
    }

    /// Ensure a `(func, args)` instance exists, cloning and substituting if needed.
    fn ensure_instance(
        &mut self,
        func: Func<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_kinds: &[EffectProviderKind],
    ) -> Option<(usize, String)> {
        let key = InstanceKey::new(func, args, receiver_space, effect_kinds);
        if let Some(&idx) = self.instance_map.get(&key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let symbol_name = self.mangled_name(func, args, receiver_space, effect_kinds);

        let instance = if args.is_empty() {
            let template_idx = self.ensure_template(func, receiver_space, effect_kinds)?;
            let mut instance = self.templates[template_idx].clone();
            instance.receiver_space = receiver_space;
            instance.effect_provider_kinds = effect_kinds.to_vec();
            instance.symbol_name = symbol_name.clone();
            self.apply_substitution(&mut instance);
            instance
        } else {
            let (_diags, typed_body) = check_func_body(self.db, func);
            let mut folder = ParamSubstFolder { args };
            let typed_body = typed_body.clone().fold_with(self.db, &mut folder);
            let mut instance = lower_function(
                self.db,
                func,
                typed_body,
                receiver_space,
                effect_kinds.to_vec(),
            )
            .ok()?;
            instance.generic_args = args.to_vec();
            instance.receiver_space = receiver_space;
            instance.effect_provider_kinds = effect_kinds.to_vec();
            instance.symbol_name = symbol_name.clone();
            instance
        };

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
        let base_args = call.callable.generic_args().to_vec();
        if let Some(inst) = call.callable.trait_inst() {
            let method_name = call
                .callable
                .callable_def
                .name(self.db)
                .expect("trait method call missing name");
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

            if let CallableDef::Func(func) = call.callable.callable_def
                && func.body(self.db).is_some()
            {
                return Some((CallTarget::Template(func), base_args));
            }

            let inst_desc = inst.pretty_print(self.db, false);
            let name = method_name.data(self.db);
            panic!(
                "failed to resolve trait method `{name}` for `{inst_desc}` (no impl and no default)"
            );
        }

        if let CallableDef::Func(func) = call.callable.callable_def {
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
                target.symbol =
                    Some(self.mangled_name(target.func, &target.generic_args, None, &[]));
            }
        }

        for block in &mut function.body.blocks {
            for inst in &mut block.insts {
                if let crate::MirInst::Assign {
                    rvalue: crate::ir::Rvalue::Call(call),
                    ..
                } = inst
                {
                    call.callable = call.callable.clone().fold_with(self.db, &mut folder);
                    call.resolved_name = None;
                }
            }

            if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                &mut block.terminator
            {
                call.callable = call.callable.clone().fold_with(self.db, &mut folder);
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
        effect_kinds: &[EffectProviderKind],
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
                AddressSpaceKind::Storage => "stor",
            };
            base = format!("{base}_{suffix}");
        }
        if effect_kinds
            .iter()
            .any(|kind| !matches!(kind, EffectProviderKind::Storage))
        {
            let suffix = effect_kinds
                .iter()
                .map(|kind| match kind {
                    EffectProviderKind::Memory => "mem",
                    EffectProviderKind::Storage => "stor",
                    EffectProviderKind::Calldata => "calldata",
                })
                .collect::<Vec<_>>()
                .join("_");
            base = format!("{base}__eff_{suffix}");
        }
        if args.is_empty() {
            return base;
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let mut parts = Vec::with_capacity(args.len());
        for ty in args {
            let pretty = ty.pretty_print(self.db);
            pretty.hash(&mut hasher);
            parts.push(sanitize_symbol_component(pretty));
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
        effect_kinds: &[EffectProviderKind],
    ) -> Option<usize> {
        let key = TemplateKey {
            func,
            receiver_space,
            effect_kinds: effect_kinds.to_vec(),
        };
        if let Some(&idx) = self.func_index.get(&key) {
            return Some(idx);
        }

        let (_diags, typed_body) = check_func_body(self.db, func);
        let lowered = lower_function(
            self.db,
            func,
            typed_body.clone(),
            receiver_space,
            effect_kinds.to_vec(),
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
            _ => ty.super_fold_with(db, self),
        }
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
