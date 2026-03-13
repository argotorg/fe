use std::{collections::VecDeque, ops::Range};

use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::{
    analysis::{HirAnalysisDb, diagnostics::SpannedHirAnalysisDb},
    hir_def::{Contract, EnumVariant, Func, ItemKind},
    projection::{IndexSource, Projection},
    semantic::{ContractFieldInfo, EffectBinding, EffectSource, RecvArmView},
    span::LazySpan,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    CallOrigin, MirFunction, MirInst, MirProjectionPath, Rvalue, TerminatingCall, Terminator,
    ValueId, ValueOrigin,
    analysis::call_graph::{build_function_symbol_map, call_target_symbol},
    ir::{LocalId, MirFunctionOrigin, Place, SourceInfoId, SyntheticId},
};

#[derive(Debug, Clone)]
pub struct InitImmutableDiagnostic {
    pub func_name: String,
    pub diagnostic: CompleteDiagnostic,
}

#[derive(Debug, Clone)]
struct ImmutableFieldMeta<'db> {
    field: ContractFieldInfo<'db>,
    word_range: Range<usize>,
}

#[derive(Debug, Clone, Copy)]
struct EffectParamMeta<'db> {
    effect_idx: u32,
    target_ty: hir::analysis::ty::ty_def::TyId<'db>,
}

#[derive(Debug, Clone, Default)]
struct ImmutableFieldMap<'db> {
    by_local: FxHashMap<LocalId, ImmutableFieldMeta<'db>>,
    by_field_idx: FxHashMap<u32, ImmutableFieldMeta<'db>>,
    effect_params_by_local: FxHashMap<LocalId, EffectParamMeta<'db>>,
    total_words: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RelativeWordSpan {
    start: usize,
    end: usize,
}

impl RelativeWordSpan {
    fn new(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn offset(self, base: usize) -> Range<usize> {
        (self.start + base)..(self.end + base)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ReadSummaryRoot {
    Field(u32),
    EffectParam(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ImmutableReadSummary {
    Span {
        root: ReadSummaryRoot,
        span: RelativeWordSpan,
    },
    Unsupported(ReadSummaryRoot),
}

#[derive(Debug, Clone)]
enum PlaceWordAccess {
    Range(Range<usize>),
    Unsupported,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct AssignedWords {
    words: Vec<bool>,
}

impl AssignedWords {
    fn new(len: usize) -> Self {
        Self {
            words: vec![false; len],
        }
    }

    fn mark(&mut self, range: Range<usize>) {
        for idx in range {
            if let Some(word) = self.words.get_mut(idx) {
                *word = true;
            }
        }
    }

    fn covers(&self, range: Range<usize>) -> bool {
        range
            .into_iter()
            .all(|idx| self.words.get(idx).copied().unwrap_or(false))
    }

    fn intersect_with(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for (lhs, rhs) in self.words.iter_mut().zip(other.words.iter()) {
            let next = *lhs && *rhs;
            changed |= *lhs != next;
            *lhs = next;
        }
        changed
    }
}

pub fn check_init_immutables<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    functions: &[MirFunction<'db>],
) -> Vec<InitImmutableDiagnostic> {
    let function_symbols = build_function_symbol_map(functions);
    let immutable_fields: FxHashMap<_, _> = functions
        .iter()
        .filter_map(|func| {
            immutable_field_map(db, func).map(|fields| (func.symbol_name.clone(), fields))
        })
        .collect();
    let mut direct_reads: FxHashMap<String, FxHashSet<ImmutableReadSummary>> = FxHashMap::default();
    for func in functions {
        let reads = immutable_fields
            .get(&func.symbol_name)
            .map(|fields| collect_direct_reads(db, func, fields))
            .unwrap_or_default();
        direct_reads.insert(func.symbol_name.clone(), reads);
    }

    let mut transitive_reads = direct_reads.clone();
    loop {
        let mut changed = false;
        for func in functions {
            let mut reads = direct_reads
                .get(&func.symbol_name)
                .cloned()
                .unwrap_or_default();
            if let Some(fields) = immutable_fields.get(&func.symbol_name) {
                remap_callee_reads(
                    db,
                    func,
                    fields,
                    &transitive_reads,
                    &function_symbols,
                    &mut reads,
                );
            }
            match transitive_reads.get_mut(&func.symbol_name) {
                Some(existing) => {
                    if *existing != reads {
                        *existing = reads;
                        changed = true;
                    }
                }
                None => {
                    transitive_reads.insert(func.symbol_name.clone(), reads);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    let mut out = Vec::new();
    for func in functions {
        let MirFunctionOrigin::Synthetic(SyntheticId::ContractInitHandler(_)) = func.origin else {
            continue;
        };
        let Some(fields) = immutable_fields.get(&func.symbol_name) else {
            continue;
        };
        if fields.by_field_idx.is_empty() {
            continue;
        }
        out.extend(check_init_handler(
            db,
            func,
            fields,
            &transitive_reads,
            &function_symbols,
        ));
    }
    out
}

fn immutable_field_map<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
) -> Option<ImmutableFieldMap<'db>> {
    let contract = origin_contract(db, func);
    let bindings: Vec<EffectBinding<'db>> = match func.origin {
        MirFunctionOrigin::Hir(hir_func) => hir_func.effect_bindings(db).to_vec(),
        MirFunctionOrigin::Synthetic(SyntheticId::ContractInitHandler(contract)) => {
            if let Some(env) = contract.init_effect_env(db) {
                env.bindings(db).to_vec()
            } else {
                Vec::new()
            }
        }
        MirFunctionOrigin::Synthetic(SyntheticId::ContractRecvArmHandler {
            contract,
            recv_idx,
            arm_idx,
        }) => {
            let recv = hir::semantic::RecvView::new(db, contract, recv_idx);
            RecvArmView::new(db, recv, arm_idx)
                .effective_effect_bindings(db)
                .clone()
        }
        _ => return None,
    };

    let mut by_local = FxHashMap::default();
    let mut by_field_idx = contract_immutable_fields(db, contract);
    let mut effect_params_by_local = FxHashMap::default();
    let total_words = by_field_idx
        .values()
        .map(|field| field.word_range.end)
        .max()
        .unwrap_or(0);
    for binding in bindings {
        let Some(&local) = func
            .body
            .effect_param_locals
            .get(binding.binding_idx as usize)
        else {
            continue;
        };

        match binding.source {
            EffectSource::Field(field)
                if matches!(
                    field.field.kind,
                    hir::semantic::ContractFieldKind::ImmutableCode
                ) =>
            {
                let Some(meta) = by_field_idx.get(&field.field.index).cloned() else {
                    continue;
                };
                by_local.insert(local, meta.clone());
                by_field_idx.insert(field.field.index, meta);
            }
            EffectSource::Root => {
                let Some(target_ty) = binding.key_ty else {
                    continue;
                };
                if hir::layout::ty_memory_size(db, target_ty).is_some() {
                    effect_params_by_local.insert(
                        local,
                        EffectParamMeta {
                            effect_idx: binding.binding_idx,
                            target_ty,
                        },
                    );
                }
            }
            EffectSource::Field(_) => {}
        }
    }

    Some(ImmutableFieldMap {
        by_local,
        by_field_idx,
        effect_params_by_local,
        total_words,
    })
}

fn origin_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
) -> Option<Contract<'db>> {
    match func.origin {
        MirFunctionOrigin::Hir(hir_func) => match hir_func.scope().parent_item(db) {
            Some(ItemKind::Contract(contract)) => Some(contract),
            _ => None,
        },
        MirFunctionOrigin::Synthetic(SyntheticId::ContractInitHandler(contract))
        | MirFunctionOrigin::Synthetic(SyntheticId::ContractInitEntrypoint(contract))
        | MirFunctionOrigin::Synthetic(SyntheticId::ContractRuntimeEntrypoint(contract)) => {
            Some(contract)
        }
        MirFunctionOrigin::Synthetic(SyntheticId::ContractRecvArmHandler { contract, .. }) => {
            Some(contract)
        }
        _ => None,
    }
}

fn contract_immutable_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Option<Contract<'db>>,
) -> FxHashMap<u32, ImmutableFieldMeta<'db>> {
    let mut fields = FxHashMap::default();
    let Some(contract) = contract else {
        return fields;
    };

    for field in contract.fields(db).values().copied() {
        if !matches!(field.kind, hir::semantic::ContractFieldKind::ImmutableCode) {
            continue;
        }
        let Some(layout) = field.immutable_layout() else {
            continue;
        };
        fields.insert(
            field.index,
            ImmutableFieldMeta {
                field,
                word_range: (layout.byte_offset / 32)
                    ..((layout.byte_offset + layout.byte_len) / 32),
            },
        );
    }

    fields
}

fn collect_direct_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
) -> FxHashSet<ImmutableReadSummary> {
    let mut out = FxHashSet::default();
    for block in &func.body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Assign { rvalue, .. } => visit_rvalue_reads(rvalue, |read| match read {
                    MirRead::Value(value) => {
                        collect_value_read_fields(db, func, fields, value, &mut out);
                    }
                    MirRead::Place(place) => {
                        if let Some(read) = place_read_summary(db, func, fields, place) {
                            out.insert(read);
                        }
                    }
                    MirRead::CallSummary(_) => {}
                }),
                MirInst::Store { value, .. } => {
                    collect_value_read_fields(db, func, fields, *value, &mut out);
                }
                MirInst::InitAggregate { inits, .. } => {
                    for (_, value) in inits {
                        collect_value_read_fields(db, func, fields, *value, &mut out);
                    }
                }
                MirInst::BindValue { value, .. } => {
                    collect_value_read_fields(db, func, fields, *value, &mut out);
                }
                MirInst::SetDiscriminant { .. } => {}
            }
        }

        visit_terminator_reads(&block.terminator, |read| {
            if let MirRead::Value(value) = read {
                collect_value_read_fields(db, func, fields, value, &mut out);
            }
        });
    }
    out
}

fn remap_callee_reads<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    transitive_reads: &FxHashMap<String, FxHashSet<ImmutableReadSummary>>,
    function_symbols: &FxHashMap<Func<'db>, Vec<String>>,
    out: &mut FxHashSet<ImmutableReadSummary>,
) {
    visit_calls(func, |call| {
        let Some(callee) = call_target_symbol(db, call, function_symbols) else {
            return;
        };
        let Some(callee_reads) = transitive_reads.get(&callee) else {
            return;
        };
        for &read in callee_reads {
            if let Some(read) = remap_call_read(func, fields, call, read) {
                out.insert(read);
            }
        }
    });
}

fn collect_value_read_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    value: ValueId,
    out: &mut FxHashSet<ImmutableReadSummary>,
) {
    let mut visiting = FxHashSet::default();
    collect_value_read_fields_inner(db, func, fields, value, out, &mut visiting);
}

fn collect_value_read_fields_inner<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    value: ValueId,
    out: &mut FxHashSet<ImmutableReadSummary>,
    visiting: &mut FxHashSet<ValueId>,
) {
    if !visiting.insert(value) {
        return;
    }

    match &func.body.value(value).origin {
        ValueOrigin::Unary { inner, .. } => {
            collect_value_read_fields_inner(db, func, fields, *inner, out, visiting);
        }
        ValueOrigin::Binary { lhs, rhs, .. } => {
            collect_value_read_fields_inner(db, func, fields, *lhs, out, visiting);
            collect_value_read_fields_inner(db, func, fields, *rhs, out, visiting);
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            if let Some(read) = place_read_summary(db, func, fields, place) {
                out.insert(read);
            }
        }
        ValueOrigin::TransparentCast { value } => {
            collect_value_read_fields_inner(db, func, fields, *value, out, visiting);
        }
        ValueOrigin::Expr(_)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Synthetic(_)
        | ValueOrigin::Local(_)
        | ValueOrigin::PlaceRoot(_)
        | ValueOrigin::FuncItem(_)
        | ValueOrigin::FieldPtr(_) => {}
    }
}

fn place_read_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    place: &Place<'db>,
) -> Option<ImmutableReadSummary> {
    let (root, access) = place_read_root_access(db, func, fields, place)?;
    let root = match root {
        ReadRootMeta::Field(field) => ReadSummaryRoot::Field(field.field.index),
        ReadRootMeta::EffectParam(effect) => ReadSummaryRoot::EffectParam(effect.effect_idx),
    };
    Some(match access {
        PlaceWordAccess::Range(range) => ImmutableReadSummary::Span {
            root,
            span: RelativeWordSpan::new(range),
        },
        PlaceWordAccess::Unsupported => ImmutableReadSummary::Unsupported(root),
    })
}

fn check_init_handler<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    transitive_reads: &FxHashMap<String, FxHashSet<ImmutableReadSummary>>,
    function_symbols: &FxHashMap<Func<'db>, Vec<String>>,
) -> Vec<InitImmutableDiagnostic> {
    let mut out = Vec::new();
    let mut seen = FxHashSet::default();
    let mut read_checks = InitReadChecks {
        db,
        func,
        fields,
        transitive_reads,
        function_symbols,
        seen: &mut seen,
        out: &mut out,
    };
    let mut entry = vec![None; func.body.blocks.len()];
    let mut worklist = VecDeque::new();
    entry[func.body.entry.index()] = Some(AssignedWords::new(fields.total_words));
    worklist.push_back(func.body.entry);

    while let Some(block_id) = worklist.pop_front() {
        let Some(mut state) = entry[block_id.index()].clone() else {
            continue;
        };
        let block = &func.body.blocks[block_id.index()];
        for (inst_idx, inst) in block.insts.iter().enumerate() {
            match inst {
                MirInst::Assign { source, rvalue, .. } => {
                    let site = ReadSite::new(*source, block_id.index(), inst_idx);
                    visit_rvalue_reads(rvalue, |read| match read {
                        MirRead::Value(value) => read_checks.check_value_reads(&state, value, site),
                        MirRead::Place(place) => read_checks.check_place_reads(&state, place, site),
                        MirRead::CallSummary(call) => {
                            read_checks.check_call_summary_reads(&state, call, site);
                        }
                    });
                }
                MirInst::Store {
                    source,
                    place,
                    value,
                } => {
                    let site = ReadSite::new(*source, block_id.index(), inst_idx);
                    read_checks.check_value_reads(&state, *value, site);
                    if let Some(field_idx) = mark_place_write(db, func, fields, &mut state, place) {
                        read_checks.check_unsupported_access(field_idx, site, 4);
                    }
                }
                MirInst::InitAggregate {
                    source,
                    place,
                    inits,
                } => {
                    let site = ReadSite::new(*source, block_id.index(), inst_idx);
                    for (_, value) in inits {
                        read_checks.check_value_reads(&state, *value, site);
                    }
                    if let Some(field_idx) =
                        mark_init_aggregate_write(db, func, fields, &mut state, place, inits)
                    {
                        read_checks.check_unsupported_access(field_idx, site, 4);
                    }
                }
                MirInst::SetDiscriminant { source, place, .. } => {
                    let site = ReadSite::new(*source, block_id.index(), inst_idx);
                    if let Some(field_idx) =
                        mark_discriminant_write(db, func, fields, &mut state, place)
                    {
                        read_checks.check_unsupported_access(field_idx, site, 4);
                    }
                }
                MirInst::BindValue { source, value } => {
                    let site = ReadSite::new(*source, block_id.index(), inst_idx);
                    read_checks.check_value_reads(&state, *value, site)
                }
            }
        }

        match &block.terminator {
            Terminator::Return { source, value } => {
                let site = ReadSite::new(*source, block_id.index(), usize::MAX);
                if let Some(value) = value {
                    read_checks.check_value_reads(&state, *value, site);
                }
                for field in fields.by_field_idx.values() {
                    if !state.covers(field.word_range.clone())
                        && read_checks.seen.insert((
                            block_id.index(),
                            usize::MAX,
                            field.field.index,
                            1,
                        ))
                    {
                        read_checks.out.push(InitImmutableDiagnostic {
                            func_name: read_checks.func.symbol_name.clone(),
                            diagnostic: missing_assignment_diag(
                                read_checks.db,
                                read_checks.func,
                                *source,
                                field.field.name,
                            ),
                        });
                    }
                }
            }
            Terminator::TerminatingCall { source, .. } => {
                let site = ReadSite::new(*source, block_id.index(), usize::MAX);
                visit_terminator_reads(&block.terminator, |read| match read {
                    MirRead::Value(value) => read_checks.check_value_reads(&state, value, site),
                    MirRead::Place(_) => {}
                    MirRead::CallSummary(call) => {
                        read_checks.check_call_summary_reads(&state, call, site);
                    }
                });
            }
            Terminator::Branch { source, cond, .. } => {
                read_checks.check_value_reads(
                    &state,
                    *cond,
                    ReadSite::new(*source, block_id.index(), usize::MAX),
                );
            }
            Terminator::Switch { source, discr, .. } => {
                read_checks.check_value_reads(
                    &state,
                    *discr,
                    ReadSite::new(*source, block_id.index(), usize::MAX),
                );
            }
            Terminator::Goto { .. } | Terminator::Unreachable { .. } => {}
        }

        for succ in successors(&block.terminator) {
            let changed = match &mut entry[succ.index()] {
                Some(existing) => existing.intersect_with(&state),
                slot @ None => {
                    *slot = Some(state.clone());
                    true
                }
            };
            if changed {
                worklist.push_back(succ);
            }
        }
    }

    out
}

#[derive(Debug, Clone, Copy)]
struct ReadSite {
    source: SourceInfoId,
    block_idx: usize,
    inst_idx: usize,
}

impl ReadSite {
    fn new(source: SourceInfoId, block_idx: usize, inst_idx: usize) -> Self {
        Self {
            source,
            block_idx,
            inst_idx,
        }
    }
}

struct InitReadChecks<'a, 'db> {
    db: &'db dyn SpannedHirAnalysisDb,
    func: &'a MirFunction<'db>,
    fields: &'a ImmutableFieldMap<'db>,
    transitive_reads: &'a FxHashMap<String, FxHashSet<ImmutableReadSummary>>,
    function_symbols: &'a FxHashMap<Func<'db>, Vec<String>>,
    seen: &'a mut FxHashSet<(usize, usize, u32, u8)>,
    out: &'a mut Vec<InitImmutableDiagnostic>,
}

impl<'a, 'db> InitReadChecks<'a, 'db> {
    fn check_value_reads(&mut self, state: &AssignedWords, value: ValueId, site: ReadSite) {
        let mut visiting = FxHashSet::default();
        for read in value_read_fields(self.db, self.func, self.fields, value, &mut visiting) {
            self.check_direct_read(state, read, site, 0, 4);
        }
    }

    fn check_call_summary_reads(
        &mut self,
        state: &AssignedWords,
        call: &CallOrigin<'db>,
        site: ReadSite,
    ) {
        let Some(callee) = call_target_symbol(self.db, call, self.function_symbols) else {
            return;
        };
        let Some(reads) = self.transitive_reads.get(&callee) else {
            return;
        };
        for &read in reads {
            self.check_call_read(state, call, read, site, 2, 5);
        }
    }

    fn check_place_reads(&mut self, state: &AssignedWords, place: &Place<'db>, site: ReadSite) {
        let Some(read) = place_read_summary(self.db, self.func, self.fields, place) else {
            return;
        };
        self.check_direct_read(state, read, site, 0, 4);
    }

    fn check_direct_read(
        &mut self,
        state: &AssignedWords,
        read: ImmutableReadSummary,
        site: ReadSite,
        span_kind: u8,
        unsupported_kind: u8,
    ) {
        match read {
            ImmutableReadSummary::Span {
                root: ReadSummaryRoot::Field(field_idx),
                span,
            } => self.check_field_read(state, field_idx, span, site, span_kind),
            ImmutableReadSummary::Unsupported(ReadSummaryRoot::Field(field_idx)) => {
                self.check_unsupported_access(field_idx, site, unsupported_kind)
            }
            ImmutableReadSummary::Span {
                root: ReadSummaryRoot::EffectParam(_),
                ..
            }
            | ImmutableReadSummary::Unsupported(ReadSummaryRoot::EffectParam(_)) => {}
        }
    }

    fn check_call_read(
        &mut self,
        state: &AssignedWords,
        call: &CallOrigin<'db>,
        read: ImmutableReadSummary,
        site: ReadSite,
        span_kind: u8,
        unsupported_kind: u8,
    ) {
        match read {
            ImmutableReadSummary::Span {
                root: ReadSummaryRoot::Field(field_idx),
                span,
            } => self.check_field_read(state, field_idx, span, site, span_kind),
            ImmutableReadSummary::Unsupported(ReadSummaryRoot::Field(field_idx)) => {
                self.check_unsupported_access(field_idx, site, unsupported_kind)
            }
            ImmutableReadSummary::Span {
                root: ReadSummaryRoot::EffectParam(effect_idx),
                span,
            } => {
                let Some(field) = self.call_effect_field(call, effect_idx) else {
                    return;
                };
                self.check_field_read(state, field.field.index, span, site, span_kind);
            }
            ImmutableReadSummary::Unsupported(ReadSummaryRoot::EffectParam(effect_idx)) => {
                let Some(field) = self.call_effect_field(call, effect_idx) else {
                    return;
                };
                self.check_unsupported_access(field.field.index, site, unsupported_kind);
            }
        }
    }

    fn call_effect_field(
        &self,
        call: &CallOrigin<'db>,
        effect_idx: u32,
    ) -> Option<&ImmutableFieldMeta<'db>> {
        let value = *call.effect_args.get(effect_idx as usize)?;
        let local = value_root_local(self.func, value)?;
        self.fields.by_local.get(&local)
    }

    fn check_field_read(
        &mut self,
        state: &AssignedWords,
        field_idx: u32,
        read: RelativeWordSpan,
        site: ReadSite,
        kind: u8,
    ) {
        let Some(field) = self.fields.by_field_idx.get(&field_idx) else {
            return;
        };
        if !state.covers(read.offset(field.word_range.start))
            && self
                .seen
                .insert((site.block_idx, site.inst_idx, field_idx, kind))
        {
            self.out.push(InitImmutableDiagnostic {
                func_name: self.func.symbol_name.clone(),
                diagnostic: read_before_assign_diag(
                    self.db,
                    self.func,
                    site.source,
                    field.field.name,
                ),
            });
        }
    }

    fn check_unsupported_access(&mut self, field_idx: u32, site: ReadSite, kind: u8) {
        let Some(field) = self.fields.by_field_idx.get(&field_idx) else {
            return;
        };
        if self
            .seen
            .insert((site.block_idx, site.inst_idx, field_idx, kind))
        {
            self.out.push(InitImmutableDiagnostic {
                func_name: self.func.symbol_name.clone(),
                diagnostic: unsupported_access_diag(
                    self.db,
                    self.func,
                    site.source,
                    field.field.name,
                ),
            });
        }
    }
}

fn value_read_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    value: ValueId,
    visiting: &mut FxHashSet<ValueId>,
) -> FxHashSet<ImmutableReadSummary> {
    let mut out = FxHashSet::default();
    collect_value_read_fields_inner(db, func, fields, value, &mut out, visiting);
    out
}

enum MirRead<'a, 'db> {
    Value(ValueId),
    Place(&'a Place<'db>),
    CallSummary(&'a CallOrigin<'db>),
}

fn visit_rvalue_reads<'a, 'db>(rvalue: &'a Rvalue<'db>, mut visit: impl FnMut(MirRead<'a, 'db>)) {
    match rvalue {
        Rvalue::Value(value) => visit(MirRead::Value(*value)),
        Rvalue::Call(call) => visit_call_origin_reads(call, visit),
        Rvalue::Intrinsic { args, .. } => {
            for &value in args {
                visit(MirRead::Value(value));
            }
        }
        Rvalue::Load { place } => visit(MirRead::Place(place)),
        Rvalue::ZeroInit | Rvalue::Alloc { .. } | Rvalue::ConstAggregate { .. } => {}
    }
}

fn visit_terminator_reads<'a, 'db>(
    term: &'a Terminator<'db>,
    mut visit: impl FnMut(MirRead<'a, 'db>),
) {
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => visit(MirRead::Value(*value)),
        Terminator::TerminatingCall { call, .. } => visit_terminating_call_reads(call, visit),
        Terminator::Branch { cond, .. } => visit(MirRead::Value(*cond)),
        Terminator::Switch { discr, .. } => visit(MirRead::Value(*discr)),
        Terminator::Goto { .. }
        | Terminator::Return { value: None, .. }
        | Terminator::Unreachable { .. } => {}
    }
}

fn visit_call_origin_reads<'a, 'db>(
    call: &'a CallOrigin<'db>,
    mut visit: impl FnMut(MirRead<'a, 'db>),
) {
    for &value in &call.args {
        visit(MirRead::Value(value));
    }
    visit(MirRead::CallSummary(call));
}

fn visit_calls<'a, 'db>(func: &'a MirFunction<'db>, mut visit: impl FnMut(&'a CallOrigin<'db>)) {
    for block in &func.body.blocks {
        for inst in &block.insts {
            if let MirInst::Assign {
                rvalue: Rvalue::Call(call),
                ..
            } = inst
            {
                visit(call);
            }
        }

        if let Terminator::TerminatingCall {
            call: TerminatingCall::Call(call),
            ..
        } = &block.terminator
        {
            visit(call);
        }
    }
}

fn remap_call_read<'db>(
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    call: &CallOrigin<'db>,
    read: ImmutableReadSummary,
) -> Option<ImmutableReadSummary> {
    match read {
        ImmutableReadSummary::Span {
            root: ReadSummaryRoot::Field(field_idx),
            span,
        } => Some(ImmutableReadSummary::Span {
            root: ReadSummaryRoot::Field(field_idx),
            span,
        }),
        ImmutableReadSummary::Unsupported(ReadSummaryRoot::Field(field_idx)) => Some(
            ImmutableReadSummary::Unsupported(ReadSummaryRoot::Field(field_idx)),
        ),
        ImmutableReadSummary::Span {
            root: ReadSummaryRoot::EffectParam(effect_idx),
            span,
        } => remap_effect_arg_root(func, fields, *call.effect_args.get(effect_idx as usize)?)
            .map(|root| ImmutableReadSummary::Span { root, span }),
        ImmutableReadSummary::Unsupported(ReadSummaryRoot::EffectParam(effect_idx)) => {
            remap_effect_arg_root(func, fields, *call.effect_args.get(effect_idx as usize)?)
                .map(ImmutableReadSummary::Unsupported)
        }
    }
}

fn remap_effect_arg_root<'db>(
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    value: ValueId,
) -> Option<ReadSummaryRoot> {
    let local = value_root_local(func, value)?;
    if let Some(field) = fields.by_local.get(&local) {
        return Some(ReadSummaryRoot::Field(field.field.index));
    }
    fields
        .effect_params_by_local
        .get(&local)
        .map(|effect| ReadSummaryRoot::EffectParam(effect.effect_idx))
}

fn visit_terminating_call_reads<'a, 'db>(
    call: &'a TerminatingCall<'db>,
    mut visit: impl FnMut(MirRead<'a, 'db>),
) {
    match call {
        TerminatingCall::Call(call) => visit_call_origin_reads(call, visit),
        TerminatingCall::Intrinsic { args, .. } => {
            for &value in args {
                visit(MirRead::Value(value));
            }
        }
        TerminatingCall::DeployRuntime {
            runtime_offset,
            runtime_len,
            immutable_payload,
        } => {
            visit(MirRead::Value(*runtime_offset));
            visit(MirRead::Value(*runtime_len));
            if let Some((ptr, _)) = immutable_payload {
                visit(MirRead::Value(*ptr));
            }
        }
    }
}

fn mark_place_write<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    state: &mut AssignedWords,
    place: &Place<'db>,
) -> Option<u32> {
    let (field, projection) = place_field_access(func, fields, place)?;
    let access = place_word_access(db, field.field.target_ty, &projection)?;
    match access {
        PlaceWordAccess::Range(range) => {
            state.mark(RelativeWordSpan::new(range).offset(field.word_range.start))
        }
        PlaceWordAccess::Unsupported => return Some(field.field.index),
    }
    None
}

fn mark_init_aggregate_write<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    state: &mut AssignedWords,
    place: &Place<'db>,
    inits: &[(MirProjectionPath<'db>, ValueId)],
) -> Option<u32> {
    if inits.is_empty() {
        return mark_place_write(db, func, fields, state, place);
    }
    let (field, prefix) = place_field_access(func, fields, place)?;
    for (suffix, _) in inits {
        match place_word_access(db, field.field.target_ty, &prefix.concat(suffix))? {
            PlaceWordAccess::Range(range) => {
                state.mark(RelativeWordSpan::new(range).offset(field.word_range.start))
            }
            PlaceWordAccess::Unsupported => return Some(field.field.index),
        }
    }
    None
}

fn mark_discriminant_write<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &ImmutableFieldMap<'db>,
    state: &mut AssignedWords,
    place: &Place<'db>,
) -> Option<u32> {
    let (field, mut projection) = place_field_access(func, fields, place)?;
    projection.push(Projection::Discriminant);
    match place_word_access(db, field.field.target_ty, &projection)? {
        PlaceWordAccess::Range(range) => {
            state.mark(RelativeWordSpan::new(range).offset(field.word_range.start))
        }
        PlaceWordAccess::Unsupported => return Some(field.field.index),
    }
    None
}

fn place_field_access<'a, 'db>(
    func: &MirFunction<'db>,
    fields: &'a ImmutableFieldMap<'db>,
    place: &Place<'db>,
) -> Option<(&'a ImmutableFieldMeta<'db>, MirProjectionPath<'db>)> {
    let (local, projection) = place_root_projection(func, place)?;
    fields.by_local.get(&local).map(|field| (field, projection))
}

#[derive(Clone, Copy)]
enum ReadRootMeta<'a, 'db> {
    Field(&'a ImmutableFieldMeta<'db>),
    EffectParam(&'a EffectParamMeta<'db>),
}

impl<'a, 'db> ReadRootMeta<'a, 'db> {
    fn target_ty(self) -> hir::analysis::ty::ty_def::TyId<'db> {
        match self {
            Self::Field(field) => field.field.target_ty,
            Self::EffectParam(effect) => effect.target_ty,
        }
    }
}

fn place_read_root<'a, 'db>(
    func: &MirFunction<'db>,
    fields: &'a ImmutableFieldMap<'db>,
    place: &Place<'db>,
) -> Option<(ReadRootMeta<'a, 'db>, MirProjectionPath<'db>)> {
    let (local, projection) = place_root_projection(func, place)?;
    if let Some(field) = fields.by_local.get(&local) {
        return Some((ReadRootMeta::Field(field), projection));
    }
    fields
        .effect_params_by_local
        .get(&local)
        .map(|effect| (ReadRootMeta::EffectParam(effect), projection))
}

fn place_root_projection<'db>(
    func: &MirFunction<'db>,
    place: &Place<'db>,
) -> Option<(LocalId, MirProjectionPath<'db>)> {
    crate::ir::resolve_local_projection_root(&func.body.values, place.base)
        .map(|(local, prefix)| (local, prefix.concat(&place.projection)))
}

fn value_root_local<'db>(func: &MirFunction<'db>, value: ValueId) -> Option<LocalId> {
    crate::ir::resolve_local_projection_root(&func.body.values, value).map(|(local, _)| local)
}

fn place_read_root_access<'a, 'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    fields: &'a ImmutableFieldMap<'db>,
    place: &Place<'db>,
) -> Option<(ReadRootMeta<'a, 'db>, PlaceWordAccess)> {
    let (root, projection) = place_read_root(func, fields, place)?;
    Some((root, place_word_access(db, root.target_ty(), &projection)?))
}

fn place_word_access<'db>(
    db: &'db dyn HirAnalysisDb,
    mut ty: hir::analysis::ty::ty_def::TyId<'db>,
    projection: &MirProjectionPath<'db>,
) -> Option<PlaceWordAccess> {
    let mut byte_offset = 0;
    for proj in projection.iter() {
        match proj {
            Projection::Field(field_idx) => {
                byte_offset += hir::layout::field_offset_memory(db, ty, *field_idx);
                ty = *ty.field_types(db).get(*field_idx)?;
            }
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => {
                byte_offset +=
                    hir::layout::variant_field_offset_memory(db, *enum_ty, *variant, *field_idx);
                ty = enum_variant_field_ty(db, *variant, *enum_ty, *field_idx)?;
            }
            Projection::Discriminant => {
                let start = byte_offset / 32;
                return Some(PlaceWordAccess::Range(start..(start + 1)));
            }
            Projection::Index(IndexSource::Constant(idx)) => {
                let elem_ty = *ty.generic_args(db).first()?;
                let elem_size = hir::layout::ty_memory_size(db, elem_ty)?;
                byte_offset += idx * elem_size;
                ty = elem_ty;
            }
            Projection::Index(IndexSource::Dynamic(_)) | Projection::Deref => {
                return Some(PlaceWordAccess::Unsupported);
            }
        }
    }
    let byte_len = hir::layout::ty_memory_size(db, ty)?;
    Some(PlaceWordAccess::Range(
        (byte_offset / 32)..((byte_offset + byte_len) / 32),
    ))
}

fn enum_variant_field_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    variant: EnumVariant<'db>,
    enum_ty: hir::analysis::ty::ty_def::TyId<'db>,
    field_idx: usize,
) -> Option<hir::analysis::ty::ty_def::TyId<'db>> {
    let field_types =
        hir::analysis::ty::simplified_pattern::ConstructorKind::Variant(variant, enum_ty)
            .field_types(db);
    field_types.get(field_idx).copied()
}

fn successors(term: &Terminator<'_>) -> Vec<crate::BasicBlockId> {
    match term {
        Terminator::Goto { target, .. } => vec![*target],
        Terminator::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch {
            targets, default, ..
        } => targets
            .iter()
            .map(|target| target.block)
            .chain(std::iter::once(*default))
            .collect(),
        Terminator::Return { .. }
        | Terminator::TerminatingCall { .. }
        | Terminator::Unreachable { .. } => Vec::new(),
    }
}

fn read_before_assign_diag<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    field: hir::hir_def::IdentId<'db>,
) -> CompleteDiagnostic {
    let span = diagnostic_span(db, func, source);
    CompleteDiagnostic::new(
        Severity::Error,
        format!(
            "immutable field `{}` may be read before it is assigned",
            field.data(db)
        ),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            "this read requires an initialized immutable field".to_string(),
            Some(span),
        )],
        vec!["note: assign the field on all paths before reading it in `init`".to_string()],
        GlobalErrorCode::new(DiagnosticPass::Mir, 2),
    )
}

fn missing_assignment_diag<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    field: hir::hir_def::IdentId<'db>,
) -> CompleteDiagnostic {
    let span = diagnostic_span(db, func, source);
    CompleteDiagnostic::new(
        Severity::Error,
        format!(
            "immutable field `{}` may be uninitialized when `init` returns",
            field.data(db)
        ),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            "this return path does not definitely assign the immutable field".to_string(),
            Some(span),
        )],
        vec!["note: assign every immutable field on all non-aborting `init` paths".to_string()],
        GlobalErrorCode::new(DiagnosticPass::Mir, 3),
    )
}

fn unsupported_access_diag<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
    field: hir::hir_def::IdentId<'db>,
) -> CompleteDiagnostic {
    let span = diagnostic_span(db, func, source);
    CompleteDiagnostic::new(
        Severity::Error,
        format!(
            "immutable field `{}` cannot be accessed through a dynamic projection in `init`",
            field.data(db)
        ),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            "this access uses a dynamic index or dereference the init analysis cannot prove safe"
                .to_string(),
            Some(span),
        )],
        vec![
            "note: use fixed field and constant-index projections before reading immutable fields in `init`"
                .to_string(),
        ],
        GlobalErrorCode::new(DiagnosticPass::Mir, 4),
    )
}

fn diagnostic_span<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    func: &MirFunction<'db>,
    source: SourceInfoId,
) -> common::diagnostics::Span {
    func.body
        .source_span(source)
        .or_else(|| {
            origin_contract(db, func).and_then(|contract| contract.span().name().resolve(db))
        })
        .or_else(|| match func.origin {
            MirFunctionOrigin::Hir(hir_func) => hir_func.span().resolve(db),
            _ => None,
        })
        .or_else(|| {
            func.body
                .source_infos
                .iter()
                .find_map(|info| info.span.clone())
        })
        .expect("immutable init diagnostic missing a span")
}
