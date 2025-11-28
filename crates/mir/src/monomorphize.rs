use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
};

use hir::analysis::{
    HirAnalysisDb,
    ty::{
        fold::{TyFoldable, TyFolder},
        trait_def::resolve_trait_method,
        ty_check::check_func_body,
        ty_def::{TyData, TyId},
    },
};
use hir::hir_def::{CallableDef, Func, item::ItemKind, scope_graph::ScopeId};
use rustc_hash::FxHashMap;

use crate::{
    CallOrigin, MirFunction, ValueOrigin, dedup::deduplicate_mir, ir::IntrinsicOp,
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
    db: &'db dyn HirAnalysisDb,
    templates: Vec<MirFunction<'db>>,
) -> Vec<MirFunction<'db>> {
    let mut monomorphizer = Monomorphizer::new(db, templates);
    monomorphizer.seed_roots();
    monomorphizer.process_worklist();
    let instances = monomorphizer.into_instances();
    deduplicate_mir(db, instances)
}

/// Worklist-driven builder that instantiates concrete MIR bodies on demand.
struct Monomorphizer<'db> {
    db: &'db dyn HirAnalysisDb,
    templates: Vec<MirFunction<'db>>,
    func_index: FxHashMap<Func<'db>, usize>,
    func_defs: FxHashMap<Func<'db>, CallableDef<'db>>,
    instances: Vec<MirFunction<'db>>,
    instance_map: FxHashMap<InstanceKey<'db>, usize>,
    worklist: VecDeque<usize>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct InstanceKey<'db> {
    func: Func<'db>,
    args: Vec<TyId<'db>>,
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
    fn new(func: Func<'db>, args: &[TyId<'db>]) -> Self {
        Self {
            func,
            args: args.to_vec(),
        }
    }
}

impl<'db> Monomorphizer<'db> {
    /// Build the bookkeeping structures (template lookup + lowered FuncDef cache).
    fn new(db: &'db dyn HirAnalysisDb, templates: Vec<MirFunction<'db>>) -> Self {
        let func_index = templates
            .iter()
            .enumerate()
            .map(|(idx, func)| (func.func, idx))
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
                let _ = self.ensure_instance(func, &[]);
            }
        }
    }

    /// Drain the worklist by resolving calls in each newly-created instance.
    fn process_worklist(&mut self) {
        while let Some(func_idx) = self.worklist.pop_front() {
            self.resolve_calls(func_idx);
        }
    }

    /// Inspect every call inside the function at `func_idx` and enqueue its targets.
    fn resolve_calls(&mut self, func_idx: usize) {
        let call_sites = {
            let function = &self.instances[func_idx];
            let mut sites: Vec<(usize, CallTarget<'db>, Vec<TyId<'db>>)> = Vec::new();
            for (value_idx, value) in function.body.values.iter().enumerate() {
                if let ValueOrigin::Call(call) = &value.origin
                    && let Some(target_func) = self.resolve_call_target(call)
                {
                    let args = call.callable.generic_args().to_vec();
                    sites.push((value_idx, target_func, args));
                }
            }
            sites
        };

        let code_region_sites = {
            let function = &self.instances[func_idx];
            function
                .body
                .values
                .iter()
                .enumerate()
                .filter_map(|(value_idx, value)| {
                    if let ValueOrigin::Intrinsic(intr) = &value.origin
                        && matches!(
                            intr.op,
                            IntrinsicOp::CodeRegionOffset | IntrinsicOp::CodeRegionLen
                        )
                        && let Some(target) = &intr.code_region
                    {
                        Some((value_idx, target.clone()))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        };

        for (value_idx, target, args) in call_sites {
            let resolved_name = match target {
                CallTarget::Template(func) => {
                    self.ensure_instance(func, &args).map(|(_, symbol)| symbol)
                }
                CallTarget::Decl(func) => Some(self.mangled_name(func, &args)),
            };

            if let Some(name) = resolved_name
                && let ValueOrigin::Call(call) =
                    &mut self.instances[func_idx].body.values[value_idx].origin
            {
                call.resolved_name = Some(name);
            }
        }

        for (value_idx, target) in code_region_sites {
            if let Some((_, symbol)) = self.ensure_instance(target.func, &target.generic_args)
                && let ValueOrigin::Intrinsic(intr) =
                    &mut self.instances[func_idx].body.values[value_idx].origin
                && let Some(target) = &mut intr.code_region
            {
                target.symbol = Some(symbol);
            }
        }
    }

    /// Ensure a `(func, args)` instance exists, cloning and substituting if needed.
    fn ensure_instance(&mut self, func: Func<'db>, args: &[TyId<'db>]) -> Option<(usize, String)> {
        let key = InstanceKey::new(func, args);
        if let Some(&idx) = self.instance_map.get(&key) {
            let symbol = self.instances[idx].symbol_name.clone();
            return Some((idx, symbol));
        }

        let template_idx = self.ensure_template(func)?;
        let mut instance = self.templates[template_idx].clone();
        instance.generic_args = args.to_vec();
        instance.symbol_name = self.mangled_name(func, &instance.generic_args);
        self.apply_substitution(&mut instance);

        let idx = self.instances.len();
        let symbol = instance.symbol_name.clone();
        self.instances.push(instance);
        self.instance_map.insert(key, idx);
        self.worklist.push_back(idx);
        Some((idx, symbol))
    }

    /// Returns the concrete HIR function targeted by the given call, accounting for trait impls.
    fn resolve_call_target(&self, call: &CallOrigin<'db>) -> Option<CallTarget<'db>> {
        if let CallableDef::Func(func) = call.callable.callable_def
            && func.body(self.db).is_some()
        {
            return Some(CallTarget::Template(func));
        }
        if let Some(inst) = call.callable.trait_inst()
            && let Some(method_name) = call.callable.callable_def.name(self.db)
            && let Some(func) = resolve_trait_method(self.db, inst, method_name)
        {
            return Some(CallTarget::Template(func));
        }
        if let CallableDef::Func(func) = call.callable.callable_def {
            return Some(CallTarget::Decl(func));
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
            if let ValueOrigin::Call(call) = &mut value.origin {
                call.callable = call.callable.clone().fold_with(self.db, &mut folder);
                call.resolved_name = None;
            }
            if let ValueOrigin::Intrinsic(intr) = &mut value.origin
                && let Some(target) = &mut intr.code_region
            {
                target.generic_args = target
                    .generic_args
                    .iter()
                    .map(|ty| ty.fold_with(self.db, &mut folder))
                    .collect();
                target.symbol = Some(self.mangled_name(target.func, &target.generic_args));
            }
        }
    }

    /// Produce a globally unique (yet mostly readable) symbol name per instance.
    fn mangled_name(&self, func: Func<'db>, args: &[TyId<'db>]) -> String {
        let mut base = func
            .name(self.db)
            .to_opt()
            .map(|ident| ident.data(self.db).to_string())
            .unwrap_or_else(|| "<anonymous>".into());
        if let Some(prefix) = self.associated_prefix(func) {
            base = format!("{prefix}_{base}");
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
        } else {
            None
        }
    }

    fn into_instances(self) -> Vec<MirFunction<'db>> {
        self.instances
    }

    /// Ensure we have lowered MIR for `func`, lowering on demand for dependency ingots.
    fn ensure_template(&mut self, func: Func<'db>) -> Option<usize> {
        if let Some(&idx) = self.func_index.get(&func) {
            return Some(idx);
        }

        let (_diags, typed_body) = check_func_body(self.db, func);
        let lowered = lower_function(self.db, func, typed_body.clone()).ok()?;
        let idx = self.templates.len();
        self.templates.push(lowered);
        self.func_index.insert(func, idx);
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
