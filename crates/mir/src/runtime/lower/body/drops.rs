//! Runtime destruction. Each `Drop` of a semantic body destroys the parts
//! drop elaboration found initialized there, each guarded by the flags of
//! the paths only some executions move. A part whose type implements `Drop`
//! is passed to its `drop`; any other part destroys its components: an
//! enum's are those of the variant it holds. EVM memory is never reclaimed,
//! so there a part whose destruction has no effect is skipped, with the flags
//! only it consults.
use std::mem;

use common::compilation::Target;
use cranelift_entity::EntityRef;
use hir::analysis::{
    semantic::{
        DropFlagPoint, DropPart, DropStep, FieldIndex, SemanticInstance, VariantIndex,
        core_method_callee_key,
        normalized::{
            NDataProjection, NDropTarget, NIndex, NPlace, NPlaceBase, NRootKind, NStatementId,
            NStatementKind, NValueDefinition,
        },
        runtime_drop_elaboration,
    },
    ty::{
        adt_def::AdtRef,
        drop_glue::{adt_drop_impl, implements_drop, ty_has_drop_glue},
        normalize::normalize_ty,
        trait_resolution::PredicateListId,
        ty_check::BodyOwner,
        ty_def::TyId,
    },
};
use rustc_hash::FxHashMap;

use super::RmirEmitter;
use crate::{
    db::MirDb,
    instance::{RuntimeInstance, runtime::runtime_instance_lowered_body},
    runtime::{
        ConstScalar, RBlock, RBlockId, RExpr, RLocalId, RStmt, RTerminator, RuntimeBody,
        RuntimeCarrier, RuntimeClass, RuntimeLocalRoot, ScalarClass, ScalarRepr, ScalarRole,
        VariantId,
        lower::type_info::runtime_zero_sized_ty,
        package::runtime_instance_for_semantic,
        place::{project_field_class, project_index_class, project_variant_field_class},
    },
};

/// What a drop reads its parts from.
#[derive(Clone)]
pub(super) enum DropSource<'db> {
    /// An owned value no place holds.
    Value(RLocalId),
    Place(NPlace<'db>),
}

impl<'db> RmirEmitter<'db> {
    fn semantic_instance(&self) -> SemanticInstance<'db> {
        self.semantic_body.owner()
    }

    /// Allocates the flags this body's drops consult on its target, set to
    /// their entry values, and indexes where each changes.
    pub(super) fn prepare_drop_flags(&mut self) {
        let semantic = self.semantic_instance();
        let Some(elaboration) = runtime_drop_elaboration(self.db, semantic) else {
            return;
        };
        let mut used = vec![false; elaboration.flags.len()];
        let mut placeholders = Vec::new();
        let drops: Vec<_> = self
            .semantic_body
            .normalized
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| match &statement.kind {
                NStatementKind::Drop { target } => Some((statement.id, target.clone())),
                _ => None,
            })
            .collect();
        for (statement, target) in &drops {
            let ty = self.drop_target_ty(target);
            for part in elaboration.parts(*statement) {
                if self.needs_drop(self.part_ty(ty, &part.path)) {
                    for &flag in &part.flags {
                        used[flag as usize] = true;
                    }
                    if !part.flags.is_empty()
                        && let NDropTarget::Place(place) = target
                        && let NPlaceBase::Root(root) = place.base
                        && let Some(local) = self.semantic_body.root_local(root)
                    {
                        placeholders.push(self.runtime_value(local));
                    }
                }
            }
        }
        let entry = RBlockId::from_u32(self.semantic_body.normalized.entry.as_u32());
        // A part only some executions initialize is read under its flags,
        // perhaps on a path that never assigned its root. A root kept as a
        // value needs a definition on every path: this one, never read.
        placeholders.sort_unstable();
        placeholders.dedup();
        for local in placeholders {
            if matches!(self.locals[local.index()].root, RuntimeLocalRoot::None)
                && let Some(class) = self.value_class(local).cloned()
            {
                self.push_stmt(
                    entry,
                    RStmt::Assign {
                        dst: local,
                        expr: RExpr::Placeholder { class },
                    },
                );
            }
        }
        self.drop_flags = elaboration
            .flags
            .iter()
            .zip(&used)
            .map(|(&initialized, &used)| {
                used.then(|| {
                    let flag = self.alloc_bool_temp();
                    self.assign_bool(entry, flag, initialized);
                    flag
                })
            })
            .collect();
        let mut updates: FxHashMap<DropFlagPoint, Vec<(u32, bool)>> = FxHashMap::default();
        for update in &elaboration.flag_updates {
            if used[update.flag as usize] {
                updates
                    .entry(update.at)
                    .or_default()
                    .push((update.flag, update.initialized));
            }
        }
        self.drop_flag_updates = updates;
    }

    /// Sets the flags whose paths are moved or initialized at `at`.
    pub(super) fn update_drop_flags(&mut self, bb: RBlockId, at: DropFlagPoint) {
        for (flag, initialized) in self.drop_flag_updates.get(&at).cloned().unwrap_or_default() {
            let flag = self.drop_flags[flag as usize].expect("an updated flag is allocated");
            self.assign_bool(bb, flag, initialized);
        }
    }

    /// Destroys what the drop `statement` finds initialized; lowering goes
    /// on in the block it returns.
    pub(super) fn lower_drop(
        &mut self,
        bb: RBlockId,
        statement: NStatementId,
        target: &NDropTarget<'db>,
    ) -> RBlockId {
        let Some(elaboration) = runtime_drop_elaboration(self.db, self.semantic_instance()) else {
            return bb;
        };
        let ty = self.drop_target_ty(target);
        // A drop's own `self` is destroyed field by field, never passed back
        // to the `drop` running. Whether its fields need destroying decides,
        // not whether its type does, which is what this `drop` is lowered to
        // find out on EVM.
        let field_wise = self.drops_own_self(target);
        let parts: Vec<_> = elaboration
            .parts(statement)
            .iter()
            .filter_map(|part| {
                let part_ty = self.part_ty(ty, &part.path);
                ((field_wise && part.path.is_empty()) || self.needs_drop(part_ty))
                    .then(|| (part.clone(), part_ty))
            })
            .collect();
        if parts.is_empty() {
            return bb;
        }
        let source = match target {
            NDropTarget::Value(value) => DropSource::Value(self.read_normalized_value(bb, *value)),
            NDropTarget::Place(place) => {
                // The drop before `a[i] = v` reads `a[i]`, so `i` is checked
                // before the old element is destroyed, not only at the store.
                self.lower_place_index_checks(bb, place);
                if self.terminated_blocks[bb.index()] {
                    return bb;
                }
                DropSource::Place(place.clone())
            }
        };
        let mut bb = bb;
        for (part, part_ty) in parts {
            bb = self.lower_drop_part(bb, &source, &part, part_ty, field_wise);
        }
        bb
    }

    fn lower_drop_part(
        &mut self,
        bb: RBlockId,
        source: &DropSource<'db>,
        part: &DropPart,
        ty: TyId<'db>,
        field_wise: bool,
    ) -> RBlockId {
        let path: Vec<_> = part.path.iter().map(|step| projection(*step)).collect();
        let guarded = !part.flags.is_empty()
            || path
                .iter()
                .any(|projection| matches!(projection, NDataProjection::VariantField { .. }));
        if !guarded {
            return self.destroy(bb, source, path, ty, field_wise && part.path.is_empty());
        }
        let join = self.new_block();
        let mut current = bb;
        for &flag in &part.flags {
            let flag = self.drop_flags[flag as usize].expect("a consulted flag is allocated");
            current = self.branch_into(current, flag, join);
        }
        for (index, projection) in path.iter().enumerate() {
            if let NDataProjection::VariantField { variant, .. } = projection {
                let is_variant = self.is_variant(current, source, &path[..index], *variant);
                current = self.branch_into(current, is_variant, join);
            }
        }
        let current = self.destroy(current, source, path, ty, false);
        self.terminate(current, RTerminator::Goto(join));
        join
    }

    /// Destroys the value of type `ty` at `path` of `source`.
    fn destroy(
        &mut self,
        bb: RBlockId,
        source: &DropSource<'db>,
        mut path: Vec<NDataProjection>,
        ty: TyId<'db>,
        field_wise: bool,
    ) -> RBlockId {
        if !field_wise && !self.needs_drop(ty) {
            return bb;
        }
        if !field_wise && has_drop_impl(self.db, ty) {
            let value = self.read_part(bb, source, &path, ty);
            let drop = self.resolve_core_method(&["ops", "Drop"], "drop", vec![ty], ty);
            self.call_generated(bb, drop, &[value]);
            return bb;
        }
        let mut bb = bb;
        if let Some(adt) = ty.adt_def(self.db)
            && matches!(adt.adt_ref(self.db), AdtRef::Enum(_))
        {
            // Destroy the fields of the variant the value holds.
            let join = self.new_block();
            for variant in 0..adt.fields(self.db).len() as u16 {
                let variant = VariantIndex(variant);
                let fields = self
                    .semantic_instance()
                    .normalized_enum_variant_field_tys(self.db, ty, variant);
                if !fields.iter().any(|&field| self.needs_drop(field)) {
                    continue;
                }
                let is_variant = self.is_variant(bb, source, &path, variant);
                let case = self.new_block();
                let next = self.new_block();
                self.terminate(
                    bb,
                    RTerminator::Branch {
                        cond: is_variant,
                        then_bb: case,
                        else_bb: next,
                    },
                );
                let mut case_bb = case;
                for (field, &field_ty) in fields.iter().enumerate() {
                    path.push(NDataProjection::VariantField {
                        variant,
                        field: FieldIndex(field as u16),
                    });
                    case_bb = self.destroy(case_bb, source, path.clone(), field_ty, false);
                    path.pop();
                }
                self.terminate(case_bb, RTerminator::Goto(join));
                bb = next;
            }
            self.terminate(bb, RTerminator::Goto(join));
            return join;
        }
        for (projection, component_ty) in self.drop_components(ty) {
            path.push(projection);
            bb = self.destroy(bb, source, path.clone(), component_ty, false);
            path.pop();
        }
        bb
    }

    /// The fields of a struct, tuple or closure, or an array's elements.
    fn drop_components(&self, ty: TyId<'db>) -> Vec<(NDataProjection, TyId<'db>)> {
        let semantic = self.semantic_instance();
        if ty.is_array(self.db)
            && let Some(len) = ty.array_len(self.db)
        {
            let element = semantic.normalized_ty(self.db, ty.generic_args(self.db)[0]);
            return (0..len)
                .map(|index| (NDataProjection::Index(NIndex::Const(index)), element))
                .collect();
        }
        semantic
            .normalized_field_types(self.db, ty)
            .iter()
            .enumerate()
            .map(|(field, &ty)| (NDataProjection::Field(FieldIndex(field as u16)), ty))
            .collect()
    }

    /// Reads, by value, the part at `path` of `source`. A zero-sized part has
    /// no runtime class and is read as an erased value, as a call passes one.
    fn read_part(
        &mut self,
        bb: RBlockId,
        source: &DropSource<'db>,
        path: &[NDataProjection],
        ty: TyId<'db>,
    ) -> RLocalId {
        match source {
            DropSource::Value(value) => {
                if path.is_empty() {
                    return *value;
                }
                let Some(class) = self.value_class(*value).cloned() else {
                    return self.erased_part(ty);
                };
                let projected = project_class(self.db, class.clone(), path);
                let dst = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(projected));
                assert!(
                    self.lower_value_extract_from_value(bb, dst, *value, class, path),
                    "a dropped value's parts can be extracted"
                );
                dst
            }
            DropSource::Place(place) => {
                let mut part = place.clone();
                for projection in path {
                    part.path.push(*projection);
                }
                part.ty = ty;
                let Some(class) = self
                    .with_current_body_cx(|cx| cx.env.normalized_place_class(cx.carriers, &part))
                else {
                    return self.erased_part(ty);
                };
                let dst = self.alloc_runtime_temp(ty, RuntimeCarrier::Value(class.clone()));
                self.lower_semantic_place_read_into(bb, dst, &part, &class);
                dst
            }
        }
    }

    /// A part with no runtime class, which only a zero-sized part lacks.
    fn erased_part(&mut self, ty: TyId<'db>) -> RLocalId {
        debug_assert!(
            self.with_current_body_cx(|cx| runtime_zero_sized_ty(
                self.db,
                ty,
                cx.env.scope(),
                cx.env.assumptions()
            )),
            "a part without a runtime class is zero-sized"
        );
        self.alloc_runtime_temp(ty, RuntimeCarrier::Erased)
    }

    /// Whether the enum at `path` of `source` holds `variant`.
    fn is_variant(
        &mut self,
        bb: RBlockId,
        source: &DropSource<'db>,
        path: &[NDataProjection],
        variant: VariantIndex,
    ) -> RLocalId {
        let ty = self.prefix_ty(source, path);
        let value = self.read_part(bb, source, path, ty);
        let enum_layout = self
            .value_class(value)
            .and_then(RuntimeClass::aggregate_layout)
            .expect("an enum value has an aggregate layout");
        let dst = self.alloc_bool_temp();
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::EnumIsVariant {
                    value,
                    variant: VariantId {
                        enum_layout,
                        index: variant.0,
                    },
                },
            },
        );
        dst
    }

    /// Continues in a new block where `cond` holds, else at `otherwise`.
    fn branch_into(&mut self, bb: RBlockId, cond: RLocalId, otherwise: RBlockId) -> RBlockId {
        let then_bb = self.new_block();
        self.terminate(
            bb,
            RTerminator::Branch {
                cond,
                then_bb,
                else_bb: otherwise,
            },
        );
        then_bb
    }

    fn terminate(&mut self, bb: RBlockId, terminator: RTerminator<'db>) {
        if !self.terminated_blocks[bb.index()] {
            self.blocks[bb.index()].terminator = terminator;
            self.terminator_origins[bb.index()] = self.current_origin;
            self.terminated_blocks[bb.index()] = true;
        }
    }

    pub(super) fn new_block(&mut self) -> RBlockId {
        let block = RBlockId::from_u32(self.blocks.len() as u32);
        self.blocks.push(RBlock {
            stmts: Vec::new(),
            terminator: RTerminator::Return(None),
        });
        self.stmt_origins.push(Vec::new());
        self.terminator_origins.push(self.current_origin);
        self.terminated_blocks.push(false);
        block
    }

    pub(super) fn alloc_bool_temp(&mut self) -> RLocalId {
        self.alloc_runtime_temp(
            TyId::bool(self.db),
            RuntimeCarrier::Value(RuntimeClass::Scalar(ScalarClass {
                repr: ScalarRepr::Bool,
                role: ScalarRole::Plain,
            })),
        )
    }

    pub(super) fn assign_bool(&mut self, bb: RBlockId, dst: RLocalId, value: bool) {
        self.push_stmt(
            bb,
            RStmt::Assign {
                dst,
                expr: RExpr::ConstScalar(ConstScalar::Bool(value)),
            },
        );
    }

    fn drop_target_ty(&self, target: &NDropTarget<'db>) -> TyId<'db> {
        match target {
            NDropTarget::Value(value) => self.semantic_body.normalized.values[value.index()].ty,
            NDropTarget::Place(place) => place.ty,
        }
    }

    /// The type at `path` from a value of `ty`.
    fn part_ty(&self, ty: TyId<'db>, path: &[DropStep]) -> TyId<'db> {
        let semantic = self.semantic_instance();
        path.iter().fold(ty, |ty, step| match *step {
            DropStep::Field(field) => semantic.normalized_field_types(self.db, ty)[field as usize],
            DropStep::Variant { variant, field } => {
                semantic.normalized_enum_variant_field_tys(self.db, ty, VariantIndex(variant))
                    [field as usize]
            }
            DropStep::Index(_) => semantic.normalized_ty(self.db, ty.generic_args(self.db)[0]),
        })
    }

    fn prefix_ty(&self, source: &DropSource<'db>, path: &[NDataProjection]) -> TyId<'db> {
        let ty = match source {
            DropSource::Value(value) => self.locals[value.index()].semantic_ty,
            DropSource::Place(place) => place.ty,
        };
        let steps: Vec<_> = path
            .iter()
            .map(|projection| drop_step(*projection))
            .collect();
        self.part_ty(ty, &steps)
    }

    /// Whether `target` is the `self` of the `Drop::drop` being lowered.
    fn drops_own_self(&self, target: &NDropTarget<'db>) -> bool {
        let normalized = &self.semantic_body.normalized;
        let self_param = match target {
            NDropTarget::Value(value) => matches!(
                normalized.values[value.index()].definition,
                NValueDefinition::EntryParam { param: 0 }
            ),
            NDropTarget::Place(place) => {
                place.path.is_empty()
                    && matches!(
                        place.base,
                        NPlaceBase::Root(root)
                            if matches!(
                                normalized.roots[root.index()].kind,
                                NRootKind::ParamPlace {
                                    param: 0
                                }
                            )
                    )
            }
        };
        let BodyOwner::Func(func, _) = self.current_semantic_key().owner(self.db) else {
            return false;
        };
        self_param && implements_drop(self.db, func)
    }

    pub(super) fn needs_drop(&self, ty: TyId<'db>) -> bool {
        runtime_needs_drop(self.db, ty)
    }
}

fn has_drop_impl<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> bool {
    ty.adt_def(db)
        .is_some_and(|adt| adt_drop_impl(db, adt.adt_ref(db)).is_some())
}

fn projection(step: DropStep) -> NDataProjection {
    match step {
        DropStep::Field(field) => NDataProjection::Field(FieldIndex(field)),
        DropStep::Variant { variant, field } => NDataProjection::VariantField {
            variant: VariantIndex(variant),
            field: FieldIndex(field),
        },
        DropStep::Index(index) => NDataProjection::Index(NIndex::Const(index)),
    }
}

fn drop_step(projection: NDataProjection) -> DropStep {
    match projection {
        NDataProjection::Field(FieldIndex(field)) => DropStep::Field(field),
        NDataProjection::VariantField {
            variant: VariantIndex(variant),
            field: FieldIndex(field),
        } => DropStep::Variant { variant, field },
        NDataProjection::Index(NIndex::Const(index)) => DropStep::Index(index),
        NDataProjection::Index(NIndex::Value(_)) | NDataProjection::Entry(_) => {
            unreachable!("a drop's parts lie at fields, variant fields and constant indices")
        }
    }
}

fn project_class<'db>(
    db: &'db dyn MirDb,
    class: RuntimeClass<'db>,
    path: &[NDataProjection],
) -> RuntimeClass<'db> {
    path.iter()
        .fold(class, |class, projection| match *projection {
            NDataProjection::Field(field) => project_field_class(db, class, field),
            NDataProjection::VariantField { variant, field } => {
                let enum_layout = class
                    .aggregate_layout()
                    .expect("a variant field lies in an enum's layout");
                project_variant_field_class(
                    db,
                    class,
                    VariantId {
                        enum_layout,
                        index: variant.0,
                    },
                    field,
                )
            }
            NDataProjection::Index(_) | NDataProjection::Entry(_) => project_index_class(db, class),
        })
}

fn runtime_needs_drop_cycle_initial<'db>(_: &'db dyn MirDb, _: salsa::Id, _: TyId<'db>) -> bool {
    false
}

/// Whether destroying a value of `ty` does anything on the compilation
/// target. Natively,
/// every value with drop glue is destroyed. On EVM, a part is destroyed when
/// its `drop` has an effect there. As for effects, recursion adds nothing: a
/// type that holds itself, such as a list of boxed nodes, needs destroying
/// only if something it holds does.
#[salsa::tracked(returns(copy), cycle_initial = runtime_needs_drop_cycle_initial)]
pub(crate) fn runtime_needs_drop<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> bool {
    if !ty_has_drop_glue(db, ty) {
        return false;
    }
    if db.compilation_settings().target(db) == Target::Native {
        return true;
    }
    if let Some(adt) = ty.adt_def(db)
        && adt_drop_impl(db, adt.adt_ref(db)).is_some()
    {
        let Some(scope) = ty.as_scope(db) else {
            return true;
        };
        let Ok(key) = core_method_callee_key(
            db,
            scope,
            PredicateListId::empty_list(db),
            &["ops", "Drop"],
            "drop",
            vec![ty],
        ) else {
            return true;
        };
        let semantic = SemanticInstance::new(db, key);
        return runtime_instance_has_effect(db, runtime_instance_for_semantic(db, semantic));
    }
    drop_glue_components(db, ty)
        .into_iter()
        .any(|component| runtime_needs_drop(db, component))
}

/// The components a value of a type without a `Drop` implementation
/// destroys: its fields, every variant's fields, or its elements. A field
/// typed by an associated type, such as `T::Out`, is the type it names.
fn drop_glue_components<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> Vec<TyId<'db>> {
    if let Some(adt) = ty.adt_def(db) {
        let args = ty.generic_args(db);
        return adt
            .fields(db)
            .iter()
            .flat_map(|variant| variant.iter_types(db))
            .map(|field| {
                normalize_ty(
                    db,
                    field.instantiate(db, args),
                    adt.scope(db),
                    PredicateListId::empty_list(db),
                )
            })
            .collect();
    }
    if ty.is_array(db) {
        return ty.generic_args(db)[..1].to_vec();
    }
    if let Some(closure) = ty.as_closure(db) {
        return closure.captures(db).clone();
    }
    ty.field_types(db)
}

fn runtime_instance_has_effect_cycle_initial<'db>(
    _: &'db dyn MirDb,
    _: salsa::Id,
    _: RuntimeInstance<'db>,
) -> bool {
    false
}

/// Whether running `instance` can be observed: it writes memory, calls
/// something observable or outside the program, checks something that can
/// revert, or ends other than by returning. Recursion adds no effect.
#[salsa::tracked(returns(copy), cycle_initial = runtime_instance_has_effect_cycle_initial)]
fn runtime_instance_has_effect<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> bool {
    if instance.is_external_declaration(db) {
        return true;
    }
    let Ok(lowered) = runtime_instance_lowered_body(db, instance) else {
        return true;
    };
    has_effect(db, lowered.body(db))
}

fn has_effect<'db>(db: &'db dyn MirDb, body: &RuntimeBody<'db>) -> bool {
    // A branch on a constant false, such as a `needs_drop` that is false,
    // takes its `else` side only; a true one takes both. A `needs_drop` that a
    // cycle decides starts false, and effects then only grow as answers turn
    // true, so the cycle settles even when a `drop`'s effect depends on
    // `needs_drop` of its own type.
    // A local assigned once holds a constant it is assigned or copies.
    let mut definitions: FxHashMap<RLocalId, Option<&RExpr<'db>>> = FxHashMap::default();
    for statement in body.blocks.iter().flat_map(|block| &block.stmts) {
        if let RStmt::Assign { dst, expr } = statement {
            definitions
                .entry(*dst)
                .and_modify(|definition| *definition = None)
                .or_insert(Some(expr));
        }
    }
    let constant = |mut local: RLocalId| {
        for _ in 0..definitions.len() {
            match definitions.get(&local).copied().flatten()? {
                RExpr::ConstScalar(ConstScalar::Bool(value)) => return Some(*value),
                RExpr::Use(source) => local = *source,
                _ => return None,
            }
        }
        None
    };
    let mut reached = vec![false; body.blocks.len()];
    let mut pending = vec![RBlockId::from_u32(0)];
    while let Some(block) = pending.pop() {
        if mem::replace(&mut reached[block.index()], true) {
            continue;
        }
        let block = &body.blocks[block.index()];
        if block_has_effect(db, block) {
            return true;
        }
        match block.terminator {
            RTerminator::Branch { cond, else_bb, .. } if constant(cond) == Some(false) => {
                pending.push(else_bb)
            }
            ref terminator => pending.extend(terminator.successors()),
        }
    }
    false
}

fn block_has_effect<'db>(db: &'db dyn MirDb, block: &RBlock<'db>) -> bool {
    !matches!(
        block.terminator,
        RTerminator::Return(_)
            | RTerminator::Goto(_)
            | RTerminator::Branch { .. }
            | RTerminator::SwitchScalar { .. }
            | RTerminator::MatchEnumTag { .. }
    ) || block.stmts.iter().any(|statement| match statement {
        RStmt::Assign { expr, .. } => match expr {
            RExpr::Call { callee, .. } => runtime_instance_has_effect(db, *callee),
            RExpr::Builtin(builtin) => builtin.has_effect(),
            RExpr::EnumAssertVariantRef { .. } => true,
            _ => false,
        },
        RStmt::End { .. } => false,
        RStmt::AssertIndexInBounds { .. }
        | RStmt::EnumAssertVariant { .. }
        | RStmt::Store { .. }
        | RStmt::CopyInto { .. }
        | RStmt::EnumSetTag { .. }
        | RStmt::EnumWriteVariant { .. } => true,
    })
}
