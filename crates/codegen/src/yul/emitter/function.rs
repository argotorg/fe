use driver::DriverDataBase;
use hir::analysis::semantic::FieldIndex;
use mir::{
    LayoutId, ScalarClass, ScalarRepr, VariantId, array_elem_size_bytes, enum_tag_size_bytes,
    enum_variant_field_offset_bytes, layout_size_bytes, struct_field_offset_bytes,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::yul::{
    doc::YulDoc,
    errors::YulError,
    legalize::{
        YBlockId, YBuiltin, YExpr, YLocalId, YStmt, YTerminator, YulAddressSpace, YulFunctionPlan,
        YulLocal, YulLocalRoot, YulPlace, YulPlaceElem, YulPlaceRoot, YulValueClass,
    },
    state::FunctionState,
};

use super::{module::PackageIndex, util::prefix_yul_name};
#[derive(Clone)]
pub(super) struct RenderedValue<'db> {
    pub(super) setup: Vec<YulDoc>,
    pub(super) value: String,
    pub(super) class: YulValueClass<'db>,
}

pub(super) struct FunctionEmitter<'a, 'db> {
    pub(super) db: &'db DriverDataBase,
    pub(super) index: &'a PackageIndex<'a, 'db>,
    pub(super) plan: &'a YulFunctionPlan<'db>,
    pub(super) section_label: String,
    pub(super) state: FunctionState,
    pub(super) cross_block_values: Vec<bool>,
    pub(super) ipdom: Vec<Option<YBlockId>>,
    pub(super) loop_headers: FxHashMap<YBlockId, YLoopInfo>,
}

#[derive(Clone, Debug)]
pub(super) struct YLoopInfo {
    pub(super) exit: Option<YBlockId>,
    pub(super) blocks: FxHashSet<YBlockId>,
}

pub(super) fn render_function_doc<'a, 'db>(
    index: &'a PackageIndex<'a, 'db>,
    plan: &'a YulFunctionPlan<'db>,
    object_label: &str,
) -> Result<YulDoc, YulError> {
    FunctionEmitter::new(index.db, index, plan, object_label).render()
}

impl<'a, 'db> FunctionEmitter<'a, 'db> {
    fn new(
        db: &'db DriverDataBase,
        index: &'a PackageIndex<'a, 'db>,
        plan: &'a YulFunctionPlan<'db>,
        object_label: &str,
    ) -> Self {
        let preds = compute_predecessors(plan);
        let ipdom = compute_immediate_postdominators(plan);
        Self {
            db,
            index,
            plan,
            section_label: object_label.to_string(),
            state: FunctionState::new(plan),
            cross_block_values: compute_cross_block_values(plan),
            ipdom: ipdom.clone(),
            loop_headers: compute_loop_headers(plan, &preds, &ipdom),
        }
    }

    fn render(mut self) -> Result<YulDoc, YulError> {
        let signature_params = self.bind_params();
        let mut docs = self.render_prologue(&signature_params)?;
        docs.extend(self.render_body()?);
        let ret_suffix = self.plan.ret.as_ref().map(|_| " -> ret").unwrap_or("");
        let params = signature_params.join(", ");
        let caption = format!(
            "function {}({params}){ret_suffix} ",
            prefix_yul_name(&self.plan.symbol)
        );
        Ok(YulDoc::block(caption, docs))
    }

    fn bind_params(&mut self) -> Vec<String> {
        let mut out = Vec::with_capacity(self.plan.param_locals.len());
        for (idx, local) in self.plan.param_locals.iter().enumerate() {
            let name = format!("p{idx}");
            if self.state.uses_value_name(*local) {
                self.state.assign_param_name(*local, name.clone());
            }
            out.push(name);
        }
        out
    }

    fn render_prologue(&mut self, param_inputs: &[String]) -> Result<Vec<YulDoc>, YulError> {
        let mut docs = Vec::new();
        for (idx, local) in self.plan.locals.iter().enumerate() {
            let local_id = YLocalId(idx as u32);
            let YulLocalRoot::MemorySlot { class } = &local.root else {
                continue;
            };
            if self.state.uses_value_name(local_id) {
                continue;
            }
            if !self.plan.param_locals.contains(&local_id) && !self.cross_block_values[idx] {
                continue;
            }
            let root_name = self
                .state
                .root_name(local_id)
                .ok_or_else(|| {
                    YulError::InvalidYulPackage(format!("missing root slot for local {idx}"))
                })?
                .to_string();
            self.state.mark_root_declared(local_id);
            docs.extend(self.alloc_memory_root_slot(&root_name, class)?);
        }
        for (idx, local) in self.plan.locals.iter().enumerate() {
            let local_id = YLocalId(idx as u32);
            if self.plan.param_locals.contains(&local_id) || !self.cross_block_values[idx] {
                continue;
            }
            let Some(name) = self.state.local_name(local_id).map(str::to_string) else {
                continue;
            };
            let Some(class) = &local.class else {
                continue;
            };
            self.state.mark_declared(local_id);
            docs.push(YulDoc::line(format!(
                "let {name} := {}",
                Self::zero_for_class(class)
            )));
        }

        for (param_idx, local) in self.plan.param_locals.iter().enumerate() {
            let Some(class) = self.plan.locals[local.index()].class.as_ref() else {
                continue;
            };
            if self.state.uses_value_name(*local) {
                continue;
            }
            let input = param_inputs.get(param_idx).cloned().unwrap_or_default();
            let value = RenderedValue {
                setup: Vec::new(),
                value: input,
                class: class.clone(),
            };
            if matches!(
                self.plan.locals[local.index()].root,
                YulLocalRoot::MemorySlot { .. }
            ) {
                docs.extend(self.write_local_storage(*local, value)?);
            }
        }
        Ok(docs)
    }

    fn render_body(&mut self) -> Result<Vec<YulDoc>, YulError> {
        self.emit_block(YBlockId(0))
    }

    pub(super) fn with_state<T>(
        &mut self,
        state: &mut FunctionState,
        f: impl FnOnce(&mut Self) -> Result<T, YulError>,
    ) -> Result<T, YulError> {
        let saved = std::mem::replace(&mut self.state, state.clone());
        let result = f(self);
        *state = std::mem::replace(&mut self.state, saved);
        result
    }

    pub(super) fn loop_info(&self, header: YBlockId) -> Option<&YLoopInfo> {
        self.loop_headers.get(&header)
    }

    pub(super) fn local(&self, local: YLocalId) -> Result<&YulLocal<'db>, YulError> {
        self.plan
            .locals
            .get(local.index())
            .ok_or_else(|| YulError::InvalidYulPackage(format!("missing local {}", local.index())))
    }

    pub(super) fn local_class(&self, local: YLocalId) -> Result<YulValueClass<'db>, YulError> {
        self.local(local)?
            .class
            .clone()
            .ok_or_else(|| YulError::InvalidYulPackage(format!("local {local:?} is erased")))
    }

    pub(super) fn root_slot_name(&self, local: YLocalId) -> Result<&str, YulError> {
        self.state.root_name(local).ok_or_else(|| {
            let local_info = self.plan.locals.get(local.index());
            YulError::InvalidYulPackage(format!(
                "function `{}` local {local:?} does not have a root slot (local={local_info:?})",
                self.plan.symbol
            ))
        })
    }

    pub(super) fn alloc_memory_slot(
        &self,
        root_name: &str,
        class: &YulValueClass<'db>,
    ) -> Result<Vec<YulDoc>, YulError> {
        let bytes = self.class_size_bytes(class)?;
        Ok(self.alloc_memory_name(root_name, &bytes.to_string()))
    }

    pub(super) fn alloc_memory_root_slot(
        &self,
        root_name: &str,
        class: &mir::RuntimeClass<'db>,
    ) -> Result<Vec<YulDoc>, YulError> {
        let bytes = self.runtime_class_size_bytes(class)?;
        Ok(self.alloc_memory_name(root_name, &bytes.to_string()))
    }

    pub(super) fn ensure_root_slot(&mut self, local: YLocalId) -> Result<Vec<YulDoc>, YulError> {
        if self.state.is_root_declared(local) {
            return Ok(Vec::new());
        }
        let YulLocalRoot::MemorySlot { class } = &self.local(local)?.root else {
            return Err(YulError::InvalidYulPackage(format!(
                "local {local:?} does not have a memory root slot"
            )));
        };
        let class = class.clone();
        let root_name = self.root_slot_name(local)?.to_string();
        self.state.mark_root_declared(local);
        let mut docs = self.alloc_memory_root_slot(&root_name, &class)?;
        if matches!(self.local(local)?.class, Some(YulValueClass::Word(_)))
            && let Some(name) = self.state.local_name(local)
            && self.state.is_declared(local)
        {
            docs.push(YulDoc::line(format!("mstore({root_name}, {name})")));
        }
        Ok(docs)
    }

    pub(super) fn alloc_temp_memory(&mut self, size: &str) -> (Vec<YulDoc>, String) {
        let temp = self.state.alloc_temp();
        (self.alloc_memory_name(&temp, size), temp)
    }

    pub(super) fn alloc_memory_name(&self, name: &str, size: &str) -> Vec<YulDoc> {
        vec![
            YulDoc::line(format!("let {name} := mload(0x40)")),
            YulDoc::block(
                format!("if iszero({name}) "),
                vec![YulDoc::line(format!("{name} := 0x80"))],
            ),
            YulDoc::line(format!("mstore(0x40, add({name}, {size}))")),
        ]
    }

    pub(super) fn class_size_bytes(&self, class: &YulValueClass<'db>) -> Result<usize, YulError> {
        Ok(match class {
            YulValueClass::Word(_) => self.index.package_layout().word_size_bytes,
            YulValueClass::MemoryPtr { layout }
            | YulValueClass::CodePtr { layout }
            | YulValueClass::StoragePtr { layout }
            | YulValueClass::TransientPtr { layout }
            | YulValueClass::CalldataPtr { layout } => {
                layout_size_bytes(self.db, *layout, self.index.package_layout())
            }
        })
    }

    pub(super) fn runtime_class_size_bytes(
        &self,
        class: &mir::RuntimeClass<'db>,
    ) -> Result<usize, YulError> {
        Ok(match class {
            mir::RuntimeClass::Scalar(_)
            | mir::RuntimeClass::Ref { .. }
            | mir::RuntimeClass::RawAddr { .. } => self.index.package_layout().word_size_bytes,
            mir::RuntimeClass::AggregateValue { layout } => {
                layout_size_bytes(self.db, *layout, self.index.package_layout())
            }
        })
    }

    pub(super) fn class_layout(
        &self,
        class: &YulValueClass<'db>,
    ) -> Result<LayoutId<'db>, YulError> {
        match class {
            YulValueClass::Word(_) => Err(YulError::Layout("word class has no layout".to_string())),
            YulValueClass::MemoryPtr { layout }
            | YulValueClass::CodePtr { layout }
            | YulValueClass::StoragePtr { layout }
            | YulValueClass::TransientPtr { layout }
            | YulValueClass::CalldataPtr { layout } => Ok(*layout),
        }
    }

    pub(super) fn field_offset_bytes(&self, layout: LayoutId<'db>, field: FieldIndex) -> usize {
        struct_field_offset_bytes(self.db, layout, field, self.index.package_layout())
    }

    pub(super) fn variant_field_offset_bytes(
        &self,
        layout: LayoutId<'db>,
        variant: VariantId<'db>,
        field: FieldIndex,
    ) -> usize {
        enum_tag_size_bytes(self.db, layout, self.index.package_layout())
            + enum_variant_field_offset_bytes(
                self.db,
                layout,
                variant,
                field,
                self.index.package_layout(),
            )
    }

    pub(super) fn index_stride_bytes(&self, layout: LayoutId<'db>) -> usize {
        array_elem_size_bytes(self.db, layout, self.index.package_layout())
    }

    pub(super) fn storage_word_offset_bytes(&self, offset: usize) -> usize {
        offset / self.index.package_layout().word_size_bytes
    }

    pub(super) fn zero_for_class(class: &YulValueClass<'db>) -> String {
        match class {
            YulValueClass::Word(_)
            | YulValueClass::MemoryPtr { .. }
            | YulValueClass::CodePtr { .. }
            | YulValueClass::StoragePtr { .. }
            | YulValueClass::TransientPtr { .. }
            | YulValueClass::CalldataPtr { .. } => "0".to_string(),
        }
    }

    pub(super) fn raw_word_mask(class: &ScalarClass<'db>) -> Option<String> {
        match class.repr {
            ScalarRepr::Bool => Some("0xff".to_string()),
            ScalarRepr::Int { bits, .. } | ScalarRepr::Address { bits } if bits < 256 => {
                Some(format!("0x{}", "ff".repeat(bits.div_ceil(8) as usize)))
            }
            ScalarRepr::FixedBytes { len } if len < 32 => {
                Some(format!("0x{}", "ff".repeat(len as usize)))
            }
            ScalarRepr::Int { .. } | ScalarRepr::FixedBytes { .. } | ScalarRepr::Address { .. } => {
                None
            }
        }
    }

    pub(super) fn scalar_word_expr(&self, local: YLocalId) -> Result<String, YulError> {
        let class = self.local_class(local)?;
        match &class {
            YulValueClass::Word(word) => self.render_word_local_read(local, word),
            _ => Err(YulError::InvalidYulPackage(format!(
                "local {local:?} is not word-valued: {class:?}"
            ))),
        }
    }

    pub(super) fn local_value(&mut self, local: YLocalId) -> Result<RenderedValue<'db>, YulError> {
        let class = self.local_class(local)?;
        let (setup, value) = match &class {
            YulValueClass::Word(word) => (Vec::new(), self.render_word_local_read(local, word)?),
            YulValueClass::MemoryPtr { .. }
            | YulValueClass::CodePtr { .. }
            | YulValueClass::StoragePtr { .. }
            | YulValueClass::TransientPtr { .. }
            | YulValueClass::CalldataPtr { .. } => {
                if let Some(name) = self.state.value_name(local) {
                    (Vec::new(), name.to_string())
                } else if self.state.root_name(local).is_some() {
                    (
                        self.ensure_root_slot(local)?,
                        self.root_slot_name(local)?.to_string(),
                    )
                } else {
                    return Err(YulError::InvalidYulPackage(format!(
                        "local {local:?} used before declaration in `{}`",
                        self.plan.symbol
                    )));
                }
            }
        };
        Ok(RenderedValue {
            setup,
            value,
            class,
        })
    }

    fn render_word_local_read(
        &self,
        local: YLocalId,
        word: &ScalarClass<'db>,
    ) -> Result<String, YulError> {
        if self.state.root_name(local).is_some() && self.state.is_root_declared(local) {
            Ok(self.render_word_slot_load(self.root_slot_name(local)?, word))
        } else if let Some(name) = self.state.value_name(local) {
            Ok(self.canonicalize_scalar_expr(name.to_string(), word))
        } else {
            Err(YulError::InvalidYulPackage(format!(
                "word local {local:?} used before declaration in `{}`",
                self.plan.symbol
            )))
        }
    }

    fn render_word_slot_load(&self, root: &str, word: &ScalarClass<'db>) -> String {
        self.canonicalize_scalar_expr(format!("mload({root})"), word)
    }

    pub(super) fn root_space_for_class(
        class: &YulValueClass<'db>,
    ) -> Result<YulAddressSpace, YulError> {
        match class {
            YulValueClass::MemoryPtr { .. } => Ok(YulAddressSpace::Memory),
            YulValueClass::CodePtr { .. } => Ok(YulAddressSpace::Code),
            YulValueClass::StoragePtr { .. } => Ok(YulAddressSpace::Storage),
            YulValueClass::TransientPtr { .. } => Ok(YulAddressSpace::Transient),
            YulValueClass::CalldataPtr { .. } => Ok(YulAddressSpace::Calldata),
            YulValueClass::Word(_) => Err(YulError::Layout(
                "word class cannot act as a place root".to_string(),
            )),
        }
    }

    pub(super) fn direct_word_slot_local(&self, place: &YulPlace<'db>) -> Option<YLocalId> {
        let YulPlaceRoot::Slot(local) = place.root else {
            return None;
        };
        (place.path.is_empty()
            && matches!(place.result_class, YulValueClass::Word(_))
            && self.state.local_name(local).is_some())
        .then_some(local)
    }

    pub(super) fn write_local_storage(
        &mut self,
        local: YLocalId,
        src: RenderedValue<'db>,
    ) -> Result<Vec<YulDoc>, YulError> {
        let Some(class) = self.plan.locals[local.index()].class.as_ref() else {
            return Ok(src.setup);
        };
        if let Some(name) = self.state.local_name(local).map(str::to_string) {
            let mut docs = src.setup;
            if self.state.is_declared(local) {
                docs.push(YulDoc::line(format!("{name} := {}", src.value)));
            } else {
                self.state.mark_declared(local);
                docs.push(YulDoc::line(format!("let {name} := {}", src.value)));
            }
            if matches!(class, YulValueClass::Word(_))
                && self.state.root_name(local).is_some()
                && self.state.is_root_declared(local)
            {
                docs.push(YulDoc::line(format!(
                    "mstore({}, {name})",
                    self.root_slot_name(local)?
                )));
            }
            return Ok(docs);
        }
        self.write_place_from_value(
            YulPlace {
                root: match &self.plan.locals[local.index()].root {
                    YulLocalRoot::MemorySlot { .. } => YulPlaceRoot::Slot(local),
                    YulLocalRoot::PtrRoot { class } => YulPlaceRoot::Ptr {
                        local,
                        space: Self::root_space_for_class(class)?,
                        class: class.clone(),
                    },
                    YulLocalRoot::None => {
                        return Err(YulError::InvalidYulPackage(format!(
                            "local {local:?} has no storage root in `{}`",
                            self.plan.symbol
                        )));
                    }
                },
                path: Box::default(),
                storage_kind: match &self.plan.locals[local.index()].root {
                    YulLocalRoot::MemorySlot { class } => match class {
                        mir::RuntimeClass::AggregateValue { .. } => {
                            crate::yul::legalize::YulStorageKind::Bytes
                        }
                        mir::RuntimeClass::Scalar(_)
                        | mir::RuntimeClass::Ref { .. }
                        | mir::RuntimeClass::RawAddr { .. } => {
                            crate::yul::legalize::YulStorageKind::Cell
                        }
                    },
                    YulLocalRoot::None | YulLocalRoot::PtrRoot { .. } => {
                        crate::yul::legalize::YulStorageKind::Bytes
                    }
                },
                packed_byte_access: false,
                runtime_result_class: match &self.plan.locals[local.index()].root {
                    YulLocalRoot::MemorySlot { class } => class.clone(),
                    YulLocalRoot::None | YulLocalRoot::PtrRoot { .. } => match class {
                        YulValueClass::Word(word) => mir::RuntimeClass::Scalar(word.clone()),
                        YulValueClass::MemoryPtr { layout }
                        | YulValueClass::CodePtr { layout }
                        | YulValueClass::StoragePtr { layout }
                        | YulValueClass::TransientPtr { layout }
                        | YulValueClass::CalldataPtr { layout } => {
                            mir::RuntimeClass::AggregateValue { layout: *layout }
                        }
                    },
                },
                result_class: class.clone(),
            },
            src,
        )
    }
}

pub(super) fn block_successors<'db>(terminator: &YTerminator<'db>) -> Vec<YBlockId> {
    match terminator {
        YTerminator::Goto(target) => vec![*target],
        YTerminator::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        YTerminator::SwitchWord { cases, default, .. } => {
            let mut out = cases.iter().map(|(_, block)| *block).collect::<Vec<_>>();
            out.push(*default);
            out
        }
        YTerminator::MatchEnumTag { cases, default, .. } => {
            let mut out = cases.iter().map(|(_, block)| *block).collect::<Vec<_>>();
            if let Some(default) = default {
                out.push(*default);
            }
            out
        }
        YTerminator::TerminalCall { .. }
        | YTerminator::ReturnData { .. }
        | YTerminator::Revert { .. }
        | YTerminator::SelfDestruct { .. }
        | YTerminator::Trap
        | YTerminator::Return(_)
        | YTerminator::Stop => Vec::new(),
    }
}

fn compute_predecessors<'db>(plan: &YulFunctionPlan<'db>) -> Vec<Vec<YBlockId>> {
    let mut preds = vec![Vec::new(); plan.blocks.len()];
    for (idx, block) in plan.blocks.iter().enumerate() {
        for succ in block_successors(&block.terminator) {
            preds[succ.index()].push(YBlockId(idx as u32));
        }
    }
    preds
}

fn compute_reachable<'db>(plan: &YulFunctionPlan<'db>) -> Vec<bool> {
    let mut reachable = vec![false; plan.blocks.len()];
    let mut stack = vec![YBlockId(0)];
    while let Some(block) = stack.pop() {
        if reachable[block.index()] {
            continue;
        }
        reachable[block.index()] = true;
        stack.extend(block_successors(&plan.blocks[block.index()].terminator));
    }
    reachable
}

fn compute_dominators<'db>(
    plan: &YulFunctionPlan<'db>,
    preds: &[Vec<YBlockId>],
    reachable: &[bool],
) -> Vec<FxHashSet<YBlockId>> {
    let all_reachable = reachable
        .iter()
        .enumerate()
        .filter_map(|(idx, is_reachable)| is_reachable.then_some(YBlockId(idx as u32)))
        .collect::<FxHashSet<_>>();
    let mut doms = vec![FxHashSet::default(); plan.blocks.len()];
    for (idx, dom) in doms.iter_mut().enumerate() {
        let block = YBlockId(idx as u32);
        if !reachable[idx] {
            continue;
        }
        if block.index() == 0 {
            dom.insert(block);
        } else {
            *dom = all_reachable.clone();
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for (idx, _) in plan.blocks.iter().enumerate().skip(1) {
            if !reachable[idx] {
                continue;
            }
            let block = YBlockId(idx as u32);
            let mut new_dom = preds[idx]
                .iter()
                .filter(|pred| reachable[pred.index()])
                .map(|pred| doms[pred.index()].clone())
                .reduce(|acc, pred| acc.intersection(&pred).copied().collect())
                .unwrap_or_default();
            new_dom.insert(block);
            if new_dom != doms[idx] {
                doms[idx] = new_dom;
                changed = true;
            }
        }
    }
    doms
}

fn compute_immediate_postdominators<'db>(plan: &YulFunctionPlan<'db>) -> Vec<Option<YBlockId>> {
    let reachable = compute_reachable(plan);
    let reachable_blocks = reachable
        .iter()
        .enumerate()
        .filter_map(|(idx, is_reachable)| is_reachable.then_some(YBlockId(idx as u32)))
        .collect::<FxHashSet<_>>();
    let mut postdoms = vec![FxHashSet::default(); plan.blocks.len()];
    for (idx, postdom) in postdoms.iter_mut().enumerate() {
        if !reachable[idx] {
            continue;
        }
        let block = YBlockId(idx as u32);
        if block_successors(&plan.blocks[idx].terminator).is_empty() {
            postdom.insert(block);
        } else {
            *postdom = reachable_blocks.clone();
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for (idx, block) in plan.blocks.iter().enumerate() {
            if !reachable[idx] {
                continue;
            }
            let block_id = YBlockId(idx as u32);
            let succs = block_successors(&block.terminator);
            if succs.is_empty() {
                continue;
            }
            let mut new_postdom = succs
                .into_iter()
                .filter(|succ| reachable[succ.index()])
                .map(|succ| postdoms[succ.index()].clone())
                .reduce(|acc, succ| acc.intersection(&succ).copied().collect())
                .unwrap_or_default();
            new_postdom.insert(block_id);
            if new_postdom != postdoms[idx] {
                postdoms[idx] = new_postdom;
                changed = true;
            }
        }
    }

    let mut ipdom = vec![None; plan.blocks.len()];
    for (idx, set) in postdoms.iter().enumerate() {
        if !reachable[idx] {
            continue;
        }
        let strict = set
            .iter()
            .copied()
            .filter(|candidate| candidate.index() != idx)
            .collect::<Vec<_>>();
        ipdom[idx] = strict.iter().copied().find(|candidate| {
            strict
                .iter()
                .copied()
                .filter(|other| other != candidate)
                .all(|other| !postdoms[other.index()].contains(candidate))
        });
    }
    ipdom
}

fn compute_loop_headers<'db>(
    plan: &YulFunctionPlan<'db>,
    preds: &[Vec<YBlockId>],
    ipdom: &[Option<YBlockId>],
) -> FxHashMap<YBlockId, YLoopInfo> {
    let reachable = compute_reachable(plan);
    let doms = compute_dominators(plan, preds, &reachable);
    let mut backedges = FxHashMap::<YBlockId, Vec<YBlockId>>::default();

    for (idx, block) in plan.blocks.iter().enumerate() {
        if !reachable[idx] {
            continue;
        }
        let pred = YBlockId(idx as u32);
        for succ in block_successors(&block.terminator) {
            if doms[pred.index()].contains(&succ) {
                backedges.entry(succ).or_default().push(pred);
            }
        }
    }

    let mut loops = FxHashMap::default();
    for (header, latches) in backedges {
        let mut blocks = FxHashSet::default();
        blocks.insert(header);
        let mut stack = Vec::new();
        for latch in latches {
            if blocks.insert(latch) {
                stack.push(latch);
            }
        }
        while let Some(block) = stack.pop() {
            for pred in &preds[block.index()] {
                if blocks.insert(*pred) && *pred != header {
                    stack.push(*pred);
                }
            }
        }

        let exits = blocks
            .iter()
            .flat_map(|block| block_successors(&plan.blocks[block.index()].terminator))
            .filter(|succ| !blocks.contains(succ))
            .collect::<FxHashSet<_>>();
        let header_exits = block_successors(&plan.blocks[header.index()].terminator)
            .into_iter()
            .filter(|succ| !blocks.contains(succ))
            .collect::<FxHashSet<_>>();
        let nonterminal_exits = exits
            .iter()
            .copied()
            .filter(|succ| !block_successors(&plan.blocks[succ.index()].terminator).is_empty())
            .collect::<FxHashSet<_>>();
        let exit = if header_exits.len() == 1 {
            Some(
                header_exits
                    .iter()
                    .copied()
                    .next()
                    .expect("header_exits.len() == 1 guarantees an exit"),
            )
        } else if nonterminal_exits.len() == 1 {
            Some(
                nonterminal_exits
                    .iter()
                    .copied()
                    .next()
                    .expect("nonterminal_exits.len() == 1 guarantees an exit"),
            )
        } else if let Some(candidate) =
            ipdom[header.index()].filter(|candidate| !blocks.contains(candidate))
        {
            Some(candidate)
        } else if exits.len() == 1 {
            Some(
                exits
                    .iter()
                    .copied()
                    .next()
                    .expect("exits.len() == 1 guarantees an exit"),
            )
        } else if nonterminal_exits.is_empty() {
            None
        } else {
            panic!(
                "structured Yul emission requires a unique loop continuation for header {:?}, found exits {:?}",
                header, exits
            )
        };
        loops.insert(header, YLoopInfo { exit, blocks });
    }

    loops
}

fn compute_cross_block_values<'db>(plan: &YulFunctionPlan<'db>) -> Vec<bool> {
    let mut blocks_by_local = vec![FxHashSet::default(); plan.locals.len()];
    for (idx, block) in plan.blocks.iter().enumerate() {
        let block_id = YBlockId(idx as u32);
        let mut locals = FxHashSet::default();
        collect_stmt_locals(&block.stmts, &mut locals);
        collect_terminator_locals(&block.terminator, &mut locals);
        for local in locals {
            blocks_by_local[local.index()].insert(block_id);
        }
    }
    blocks_by_local
        .into_iter()
        .map(|blocks| blocks.len() > 1)
        .collect()
}

fn collect_stmt_locals<'db>(stmts: &[YStmt<'db>], out: &mut FxHashSet<YLocalId>) {
    for stmt in stmts {
        match stmt {
            YStmt::Assign { dst, expr } => {
                out.insert(*dst);
                collect_expr_locals(expr, out);
            }
            YStmt::Call { args, .. } => out.extend(args.iter().copied()),
            YStmt::Builtin(builtin) => collect_builtin_locals(builtin, out),
            YStmt::Store { dst, src } | YStmt::CopyInto { dst, src } => {
                collect_place_locals(dst, out);
                out.insert(*src);
            }
            YStmt::EnumAssertVariant { value, .. } | YStmt::EnumSetTag { root: value, .. } => {
                out.insert(*value);
            }
            YStmt::EnumWriteVariant { root, fields, .. } => {
                out.insert(*root);
                out.extend(fields.iter().copied());
            }
        }
    }
}

fn collect_expr_locals<'db>(expr: &YExpr<'db>, out: &mut FxHashSet<YLocalId>) {
    match expr {
        YExpr::Use(local)
        | YExpr::Unary { value: local, .. }
        | YExpr::Cast { value: local, .. }
        | YExpr::EnumTagOfValue { value: local }
        | YExpr::ProviderToRaw { value: local }
        | YExpr::EnumGetTag { root: local }
        | YExpr::EnumAssertVariantRef { root: local, .. }
        | YExpr::EnumIsVariant { value: local, .. }
        | YExpr::EnumExtract { value: local, .. } => {
            out.insert(*local);
        }
        YExpr::Placeholder { .. }
        | YExpr::ConstWord(_)
        | YExpr::ConstRef { .. }
        | YExpr::AllocObject { .. } => {}
        YExpr::Builtin(builtin) => collect_builtin_locals(builtin, out),
        YExpr::Binary { lhs, rhs, .. } => {
            out.insert(*lhs);
            out.insert(*rhs);
        }
        YExpr::MaterializeToObject { src, .. }
        | YExpr::ProviderFromRaw { raw: src, .. }
        | YExpr::WordToRawAddr { value: src, .. } => {
            out.insert(*src);
        }
        YExpr::MaterializePlaceToObject { place, .. }
        | YExpr::AddrOf { place }
        | YExpr::Load { place } => collect_place_locals(place, out),
        YExpr::Call { args, .. } | YExpr::EnumMake { fields: args, .. } => {
            out.extend(args.iter().copied());
        }
    }
}

fn collect_builtin_locals<'db>(builtin: &YBuiltin<'db>, out: &mut FxHashSet<YLocalId>) {
    match builtin {
        YBuiltin::Msize
        | YBuiltin::CallValue
        | YBuiltin::ReturnDataSize
        | YBuiltin::CallDataSize
        | YBuiltin::CodeSize
        | YBuiltin::Address
        | YBuiltin::Caller
        | YBuiltin::Origin
        | YBuiltin::GasPrice
        | YBuiltin::CoinBase
        | YBuiltin::Timestamp
        | YBuiltin::Number
        | YBuiltin::PrevRandao
        | YBuiltin::GasLimit
        | YBuiltin::ChainId
        | YBuiltin::BaseFee
        | YBuiltin::SelfBalance
        | YBuiltin::Gas
        | YBuiltin::CurrentCodeRegionLen
        | YBuiltin::CodeRegionOffset { .. }
        | YBuiltin::CodeRegionLen { .. }
        | YBuiltin::CallDataSelector
        | YBuiltin::MakeContractFieldRef { .. } => {}
        YBuiltin::Mload { addr }
        | YBuiltin::Sload { slot: addr }
        | YBuiltin::CallDataLoad { offset: addr }
        | YBuiltin::BlockHash { block: addr }
        | YBuiltin::Malloc { size: addr } => {
            out.insert(*addr);
        }
        YBuiltin::Mstore { addr, value }
        | YBuiltin::Mstore8 { addr, value }
        | YBuiltin::Sstore { slot: addr, value } => {
            out.insert(*addr);
            out.insert(*value);
        }
        YBuiltin::Mcopy { dst, src, len } => {
            out.insert(*dst);
            out.insert(*src);
            out.insert(*len);
        }
        YBuiltin::ReturnDataCopy { dst, offset, len }
        | YBuiltin::CallDataCopy { dst, offset, len }
        | YBuiltin::CodeCopy { dst, offset, len } => {
            out.insert(*dst);
            out.insert(*offset);
            out.insert(*len);
        }
        YBuiltin::Keccak256 { offset, len } => {
            out.insert(*offset);
            out.insert(*len);
        }
        YBuiltin::AddMod { lhs, rhs, modulus } | YBuiltin::MulMod { lhs, rhs, modulus } => {
            out.insert(*lhs);
            out.insert(*rhs);
            out.insert(*modulus);
        }
        YBuiltin::SignExtend { byte, value } => {
            out.insert(*byte);
            out.insert(*value);
        }
        YBuiltin::IntrinsicArith { lhs, rhs, .. } | YBuiltin::Saturating { lhs, rhs, .. } => {
            out.insert(*lhs);
            out.insert(*rhs);
        }
        YBuiltin::Call {
            gas,
            addr,
            value,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => {
            out.extend([
                *gas,
                *addr,
                *value,
                *args_offset,
                *args_len,
                *ret_offset,
                *ret_len,
            ]);
        }
        YBuiltin::StaticCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        }
        | YBuiltin::DelegateCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => {
            out.extend([*gas, *addr, *args_offset, *args_len, *ret_offset, *ret_len]);
        }
        YBuiltin::Create { value, offset, len } => out.extend([*value, *offset, *len]),
        YBuiltin::Create2 {
            value,
            offset,
            len,
            salt,
        } => out.extend([*value, *offset, *len, *salt]),
        YBuiltin::Log0 { offset, len } => out.extend([*offset, *len]),
        YBuiltin::Log1 {
            offset,
            len,
            topic0,
        } => out.extend([*offset, *len, *topic0]),
        YBuiltin::Log2 {
            offset,
            len,
            topic0,
            topic1,
        } => out.extend([*offset, *len, *topic0, *topic1]),
        YBuiltin::Log3 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
        } => out.extend([*offset, *len, *topic0, *topic1, *topic2]),
        YBuiltin::Log4 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
            topic3,
        } => out.extend([*offset, *len, *topic0, *topic1, *topic2, *topic3]),
    }
}

fn collect_place_locals<'db>(place: &YulPlace<'db>, out: &mut FxHashSet<YLocalId>) {
    match &place.root {
        YulPlaceRoot::Slot(local) | YulPlaceRoot::Ptr { local, .. } => {
            out.insert(*local);
        }
    }
    for elem in place.path.iter() {
        if let YulPlaceElem::Index {
            index: hir::projection::IndexSource::Dynamic(index),
            ..
        } = elem
        {
            out.insert(*index);
        }
    }
}

fn collect_terminator_locals<'db>(terminator: &YTerminator<'db>, out: &mut FxHashSet<YLocalId>) {
    match terminator {
        YTerminator::Goto(_) | YTerminator::Trap | YTerminator::Stop => {}
        YTerminator::Branch { cond, .. } => {
            out.insert(*cond);
        }
        YTerminator::SwitchWord { discr, .. } | YTerminator::MatchEnumTag { tag: discr, .. } => {
            out.insert(*discr);
        }
        YTerminator::TerminalCall { args, .. } => out.extend(args.iter().copied()),
        YTerminator::ReturnData { offset, len } | YTerminator::Revert { offset, len } => {
            out.insert(*offset);
            out.insert(*len);
        }
        YTerminator::SelfDestruct { beneficiary } => {
            out.insert(*beneficiary);
        }
        YTerminator::Return(Some(value)) => {
            out.insert(*value);
        }
        YTerminator::Return(None) => {}
    }
}
