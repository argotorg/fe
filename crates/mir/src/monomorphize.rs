use std::{
    cell::RefCell,
    collections::VecDeque,
    hash::{Hash, Hasher},
};

use hir::analysis::{
    diagnostics::SpannedHirAnalysisDb,
    diagnostics::format_diags,
    ty::{
        binder::Binder,
        fold::TyFoldable,
        normalize::TypeNormalizer,
        trait_def::resolve_trait_method_instance,
        ty_check::check_func_body,
        ty_def::{TyBase, TyData, TyId},
    },
};
use hir::core::semantic::constraints_for;
use hir::hir_def::{CallableDef, Func, PathKind, item::ItemKind, scope_graph::ScopeId};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    CallOrigin, MirFunction, MirInst,
    dedup::deduplicate_mir,
    ir::{AddressSpaceKind, EffectProviderKind, Rvalue},
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
    current_symbol: Option<String>,
    ambiguous_bases: FxHashSet<String>,
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

/// Monomorphization + normalization context for a single [`InstanceKey`].
///
/// This centralizes generic substitution and associated-type normalization so downstream MIR
/// passes and codegen never need to special-case type folding.
struct InstanceCtx<'db> {
    db: &'db dyn SpannedHirAnalysisDb,
    key: InstanceKey<'db>,
    normalizer: RefCell<TypeNormalizer<'db>>,
}

impl<'db> InstanceCtx<'db> {
    fn new(
        db: &'db dyn SpannedHirAnalysisDb,
        func: Func<'db>,
        args: &[TyId<'db>],
        receiver_space: Option<AddressSpaceKind>,
        effect_kinds: &[EffectProviderKind],
    ) -> Self {
        let norm_scope = crate::ty::normalization_scope_for_args(db, func, args);
        let assumptions_template = constraints_for(db, ItemKind::Func(func));

        // Seed normalization with assumptions instantiated from the raw args, then re-instantiate
        // using the normalized args so `assumptions` and `generic_args` stay aligned.
        let seed_assumptions = Binder::bind(assumptions_template).instantiate(db, args);
        let mut seed_normalizer = TypeNormalizer::new(db, norm_scope, seed_assumptions);
        let normalized_args = args
            .iter()
            .copied()
            .map(|ty| ty.fold_with(db, &mut seed_normalizer))
            .collect::<Vec<_>>();

        let assumptions = Binder::bind(assumptions_template).instantiate(db, &normalized_args);

        let mut normalizer = TypeNormalizer::new(db, norm_scope, assumptions);
        let assumptions = assumptions.fold_with(db, &mut normalizer);

        let key = InstanceKey {
            func,
            args: normalized_args,
            receiver_space,
            effect_kinds: effect_kinds.to_vec(),
        };

        Self {
            db,
            key,
            normalizer: RefCell::new(TypeNormalizer::new(db, norm_scope, assumptions)),
        }
    }

    fn instantiate_and_normalize<T: TyFoldable<'db>>(&self, value: T) -> T {
        let b = Binder::bind(value).instantiate(self.db, &self.key.args);
        b.fold_with(self.db, &mut *self.normalizer.borrow_mut())
    }

    fn instantiated_ret_ty(&self) -> TyId<'db> {
        let ret_ty = CallableDef::Func(self.key.func)
            .ret_ty(self.db)
            .instantiate(self.db, &self.key.args);
        ret_ty.fold_with(self.db, &mut *self.normalizer.borrow_mut())
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
            let base = self.base_name_without_disambiguation(
                template.func,
                template.receiver_space,
                &template.effect_provider_kinds,
            );
            let qualifier = self.function_qualifier(template.func);
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
            Vec<EffectProviderKind>,
        )> = {
            let function = &self.instances[func_idx];
            let mut sites = Vec::new();
            for (bb_idx, block) in function.body.blocks.iter().enumerate() {
                for (inst_idx, inst) in block.insts.iter().enumerate() {
                    if let MirInst::Assign {
                        rvalue: Rvalue::Call(call),
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
                    let ctx = InstanceCtx::new(self.db, func, &args, receiver_space, &effect_kinds);
                    Some(self.mangled_name(&ctx))
                }
            };

            if let Some(name) = resolved_name {
                match inst_idx {
                    Some(inst_idx) => {
                        let inst =
                            &mut self.instances[func_idx].body.blocks[bb_idx].insts[inst_idx];
                        if let MirInst::Assign {
                            rvalue: Rvalue::Call(call),
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
        let ctx = InstanceCtx::new(self.db, func, args, receiver_space, effect_kinds);
        if let Some(&idx) = self.instance_map.get(&ctx.key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let symbol_name = self.mangled_name(&ctx);

        let mut instance = if ctx.key.args.is_empty() {
            let template_idx =
                self.ensure_template(func, ctx.key.receiver_space, &ctx.key.effect_kinds)?;
            let mut instance = self.templates[template_idx].clone();
            instance.receiver_space = ctx.key.receiver_space;
            instance.effect_provider_kinds = ctx.key.effect_kinds.clone();
            instance.symbol_name = symbol_name.clone();
            instance
        } else {
            let (diags, typed_body) = check_func_body(self.db, func);
            if !diags.is_empty() {
                let name = func.pretty_print_signature(self.db);
                let rendered = format_diags(self.db, diags);
                panic!("analysis errors while lowering `{name}`:\n{rendered}");
            }
            let typed_body = ctx.instantiate_and_normalize(typed_body.clone());
            let mut instance = lower_function(
                self.db,
                func,
                typed_body,
                ctx.key.receiver_space,
                ctx.key.effect_kinds.clone(),
                ctx.key.args.clone(),
            )
            .unwrap_or_else(|err| {
                let name = func.pretty_print_signature(self.db);
                panic!("failed to instantiate MIR for `{name}`: {err}");
            });
            instance.receiver_space = ctx.key.receiver_space;
            instance.effect_provider_kinds = ctx.key.effect_kinds.clone();
            instance.symbol_name = symbol_name.clone();
            instance
        };

        self.finalize_instance_types(&ctx, &mut instance);

        let idx = self.instances.len();
        let symbol = instance.symbol_name.clone();
        self.instances.push(instance);
        let InstanceCtx { key, .. } = ctx;
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

    fn finalize_instance_types(&self, ctx: &InstanceCtx<'db>, function: &mut MirFunction<'db>) {
        function.generic_args = ctx.key.args.clone();
        function.ret_ty = ctx.instantiated_ret_ty();
        function.returns_value = !crate::layout::is_zero_sized_ty(self.db, function.ret_ty);
        function.typed_body = ctx.instantiate_and_normalize(function.typed_body.clone());

        for local in &mut function.body.locals {
            local.ty = ctx.instantiate_and_normalize(local.ty);
        }

        for value in &mut function.body.values {
            value.ty = ctx.instantiate_and_normalize(value.ty);
            match &mut value.origin {
                crate::ValueOrigin::FuncItem(target) => {
                    target.generic_args = target
                        .generic_args
                        .iter()
                        .copied()
                        .map(|ty| ctx.instantiate_and_normalize(ty))
                        .collect();
                    target.symbol = None;
                }
                crate::ValueOrigin::PlaceRef(place) => self.finalize_place_types(ctx, place),
                _ => {}
            }
        }

        for block in &mut function.body.blocks {
            for inst in &mut block.insts {
                match inst {
                    MirInst::Assign { rvalue, .. } => match rvalue {
                        Rvalue::Call(call) => {
                            call.callable = ctx.instantiate_and_normalize(call.callable.clone());
                            call.resolved_name = None;
                        }
                        Rvalue::Load { place } => self.finalize_place_types(ctx, place),
                        _ => {}
                    },
                    MirInst::Store { place, .. } | MirInst::SetDiscriminant { place, .. } => {
                        self.finalize_place_types(ctx, place);
                    }
                    MirInst::InitAggregate { place, inits } => {
                        self.finalize_place_types(ctx, place);
                        for (path, _) in inits {
                            self.finalize_projection_path_types(ctx, path);
                        }
                    }
                    MirInst::BindValue { .. } => {}
                }
            }

            if let crate::Terminator::TerminatingCall(crate::ir::TerminatingCall::Call(call)) =
                &mut block.terminator
            {
                call.callable = ctx.instantiate_and_normalize(call.callable.clone());
                call.resolved_name = None;
            }
        }
    }

    fn finalize_place_types(&self, ctx: &InstanceCtx<'db>, place: &mut crate::ir::Place<'db>) {
        self.finalize_projection_path_types(ctx, &mut place.projection);
    }

    fn finalize_projection_path_types(
        &self,
        ctx: &InstanceCtx<'db>,
        path: &mut crate::ir::MirProjectionPath<'db>,
    ) {
        use hir::projection::Projection;
        let old = std::mem::take(path);
        for proj in old.iter() {
            match proj {
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    path.push(Projection::VariantField {
                        variant: *variant,
                        enum_ty: ctx.instantiate_and_normalize(*enum_ty),
                        field_idx: *field_idx,
                    });
                }
                _ => path.push(proj.clone()),
            }
        }
    }

    /// Produce a globally unique (yet mostly readable) symbol name per instance.
    fn mangled_name(&self, ctx: &InstanceCtx<'db>) -> String {
        let key = &ctx.key;
        let mut base =
            self.base_name_without_disambiguation(key.func, key.receiver_space, &key.effect_kinds);
        if self.ambiguous_bases.contains(&base) {
            let qualifier = self.function_qualifier(key.func);
            base = format!("{qualifier}_{base}");
        }
        if key.args.is_empty() {
            return base;
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        let mut parts = Vec::with_capacity(key.args.len());
        for ty in &key.args {
            let pretty = self.type_mangle_component(*ty);
            pretty.hash(&mut hasher);
            parts.push(sanitize_symbol_component(&pretty));
        }
        let hash = hasher.finish();
        let suffix = parts.join("_");
        format!("{base}__{suffix}__{hash:08x}")
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
                    if self.ambiguous_bases.contains(&name) {
                        let qualifier = self.function_qualifier(*func);
                        format!("fn {qualifier}::{name}")
                    } else {
                        format!("fn {name}")
                    }
                }
                CallableDef::VariantCtor(_) => ty.pretty_print(self.db).to_string(),
            },
            _ => ty.pretty_print(self.db).to_string(),
        }
    }

    fn base_name_without_disambiguation(
        &self,
        func: Func<'db>,
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
        base
    }

    fn function_qualifier(&self, func: Func<'db>) -> String {
        let mut parts = Vec::new();
        let mut scope = func.scope();
        while let Some(parent) = scope.parent_module(self.db) {
            match parent {
                ScopeId::Item(ItemKind::TopMod(_)) => break,
                ScopeId::Item(ItemKind::Mod(mod_)) => {
                    if let Some(name) = mod_.name(self.db).to_opt() {
                        parts.push(name.data(self.db).to_string());
                    }
                    scope = parent;
                }
                _ => scope = parent,
            }
        }

        parts.reverse();
        let qualifier = parts.join("_");
        let qualifier = sanitize_symbol_component(&qualifier).to_lowercase();
        if qualifier.is_empty() {
            "root".to_string()
        } else {
            qualifier
        }
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
            effect_kinds.to_vec(),
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

/// Replace any non-alphanumeric characters with `_` so the mangled symbol is a
/// valid Yul identifier while remaining somewhat recognizable.
fn sanitize_symbol_component(component: &str) -> String {
    component
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}
