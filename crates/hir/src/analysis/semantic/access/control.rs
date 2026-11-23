//! Which paths of a body can execute, given callees that never return and
//! uninhabited types.
use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;
use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SBlockId, SExpr, SStmtId, SStmtKind, STerminatorKind, SemanticInstance, VariantIndex,
            normalized::{
                NDropTarget, NExpr, NPlace, NStatement, NStatementKind, NSuccessor,
                NTerminatorKind, NValueDefinition, NValueId, NormalizedBody,
                normalize_semantic_body,
            },
        },
        ty::{trait_resolution::PredicateListId, ty_def::TyId},
    },
    hir_def::scope_graph::ScopeId,
};

/// The control flow a body can execute. Block indices and successor
/// positions are those of the raw semantic body, which every normalization of
/// an instance preserves; a diverging call or trapping cut is named by its raw
/// statement.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ExecutableControlFlow(pub Box<[ExecutableBlock]>);

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ExecutableBlock {
    Unreachable,
    /// Execution ends after this call, whose callee does not return.
    Diverges(SStmtId),
    /// Execution traps before reaching an uninhabited operation. Only the
    /// statements up to and including the anchor run: the last statement
    /// before the trapping one that has a raw statement, named by that raw
    /// statement, or none when no such statement precedes it.
    ///
    /// Normalization gives each raw statement at most one statement that
    /// names it, and the helpers it adds name none, so the anchor is the only
    /// statement naming its raw statement and the trapping one names another.
    /// Runtime normalization normalizes the same raw statements with constants
    /// folded into their expressions, which leaves each its statement, so
    /// runtime lowering finds the anchor there.
    ///
    /// The helpers after the anchor that belong to its raw statement do not
    /// run either: a rooted local's assignment repacks the value and stores it
    /// into the local's root, a frame slot or a provider root. Nothing after
    /// the cut reads that root, and the trap ends execution with a panic,
    /// which on EVM reverts every state write, so the store is never seen.
    TrapsAfter(Option<SStmtId>),
    /// The terminator executes; each successor edge is feasible or not.
    Continues(Box<[bool]>),
}

/// Which values and places of a body's statements are uninhabited, in its
/// owner's scope.
struct Uninhabited<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedBody<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
}

impl<'db> Uninhabited<'_, 'db> {
    fn ty(&self, ty: TyId<'db>) -> bool {
        ty.is_uninhabited_in(self.db, self.scope, self.assumptions)
    }

    fn value(&self, value: NValueId) -> bool {
        self.body
            .value(value)
            .is_some_and(|value| self.ty(value.ty))
    }

    fn variant(&self, enum_ty: TyId<'db>, variant: usize) -> bool {
        enum_ty.is_enum_variant_uninhabited_in(self.db, variant, self.scope, self.assumptions)
    }

    /// Whether `place`, or a place it projects from, is uninhabited.
    fn place(&self, place: &NPlace<'db>) -> bool {
        self.ty(place.ty)
            || (0..=place.path.len()).any(|len| {
                self.body
                    .place_prefix_ty(self.db, place, len)
                    .is_some_and(|ty| self.ty(ty))
            })
    }

    /// Whether `statement` reads, writes, drops or ends an uninhabited value
    /// or place: for a call, an argument.
    fn operands(&self, statement: &NStatement<'db>) -> bool {
        match &statement.kind {
            NStatementKind::Define { expr, .. } => {
                let mut uninhabited = false;
                expr.for_each_value_operand(|operand| uninhabited |= self.value(operand.value));
                expr.for_each_place_operand(|place| uninhabited |= self.place(place));
                uninhabited
            }
            NStatementKind::Store { destination, value } => {
                self.value(value.value) || self.place(destination)
            }
            NStatementKind::End { access: value }
            | NStatementKind::Drop {
                target: NDropTarget::Value(value),
            } => self.value(*value),
            NStatementKind::Drop {
                target: NDropTarget::Place(place),
            } => self.place(place),
            NStatementKind::CheckIndex { .. } => false,
        }
    }

    /// Whether `statement` defines an uninhabited value, projects a value
    /// through one, or builds an uninhabited variant.
    fn defines(&self, statement: &NStatement<'db>) -> bool {
        let NStatementKind::Define { result, expr } = &statement.kind else {
            return false;
        };
        self.value(*result)
            || match expr {
                NExpr::ProjectValue { value, path } => (0..=path.0.len()).any(|len| {
                    self.body
                        .value_prefix_ty(self.db, value.value, &path.0, len)
                        .is_some_and(|ty| self.ty(ty))
                }),
                NExpr::EnumMake {
                    enum_ty, variant, ..
                } => self.variant(*enum_ty, usize::from(variant.0)),
                _ => false,
            }
    }

    /// Whether `successor` passes an uninhabited value or enters a block with
    /// an uninhabited parameter.
    fn successor(&self, successor: &NSuccessor) -> bool {
        successor.args.iter().any(|arg| self.value(arg.value))
            || self.body.blocks[successor.block.index()]
                .params
                .iter()
                .any(|&param| self.value(param))
    }

    /// Whether a variant of `enum_ty` that no case of `cases` matches is
    /// inhabited, so a default successor can run.
    fn uncovered_inhabited_variant(
        &self,
        enum_ty: TyId<'db>,
        cases: &[(VariantIndex, NSuccessor)],
    ) -> bool {
        let Some(enum_) = enum_ty.as_enum(self.db) else {
            return true;
        };
        let covered: FxHashSet<usize> = cases
            .iter()
            .map(|(variant, _)| usize::from(variant.0))
            .collect();
        (0..enum_.len_variants(self.db))
            .any(|idx| !covered.contains(&idx) && !self.variant(enum_ty, idx))
    }
}

fn executable_flow<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
) -> (ExecutableControlFlow, bool) {
    let uninhabited = Uninhabited {
        db,
        body,
        scope: Some(body.owner.key(db).owner(db).scope()),
        assumptions: body.owner.assumptions(db),
    };
    let mut blocks = vec![ExecutableBlock::Unreachable; body.blocks.len()];
    if body.values.iter().any(|value| {
        matches!(value.definition, NValueDefinition::EntryParam { .. }) && uninhabited.ty(value.ty)
    }) {
        return (ExecutableControlFlow(blocks.into()), false);
    }
    let mut reached = vec![false; body.blocks.len()];
    let mut stack = vec![body.entry];
    reached[body.entry.index()] = true;
    let mut may_return = false;
    while let Some(block) = stack.pop() {
        let data = &body.blocks[block.index()];
        let mut last_source = None;
        let mut cut = None;
        for statement in &data.statements {
            // A call traps before it is made when an argument is uninhabited.
            let operands = uninhabited.operands(statement);
            if !operands
                && let NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                } = &statement.kind
                && !semantic_may_return(db, SemanticInstance::new(db, callee.key))
                && let Some(call) = statement.source
            {
                cut = Some(ExecutableBlock::Diverges(call));
                break;
            }
            if operands || uninhabited.defines(statement) {
                cut = Some(ExecutableBlock::TrapsAfter(last_source));
                break;
            }
            if let Some(source) = statement.source {
                last_source = Some(source);
            }
        }
        if let Some(cut) = cut {
            blocks[block.index()] = cut;
            continue;
        }
        let terminator_uninhabited = match &data.terminator.kind {
            NTerminatorKind::Branch { cond, .. } => uninhabited.value(cond.value),
            NTerminatorKind::MatchEnum { value, enum_ty, .. } => {
                uninhabited.ty(*enum_ty) || uninhabited.value(value.value)
            }
            NTerminatorKind::Return(Some(value)) | NTerminatorKind::Yield { value, .. } => {
                uninhabited.value(value.value)
            }
            NTerminatorKind::Goto(_)
            | NTerminatorKind::Fail(_)
            | NTerminatorKind::Unreachable
            | NTerminatorKind::Return(None) => false,
        };
        if terminator_uninhabited {
            blocks[block.index()] = ExecutableBlock::TrapsAfter(last_source);
            continue;
        }
        may_return |= matches!(
            data.terminator.kind,
            NTerminatorKind::Return(_) | NTerminatorKind::Yield { .. }
        );
        let successors = data.terminator.kind.successors();
        let feasible: Vec<bool> = match &data.terminator.kind {
            NTerminatorKind::MatchEnum {
                enum_ty,
                cases,
                default,
                ..
            } => cases
                .iter()
                .map(|(variant, target)| {
                    !uninhabited.variant(*enum_ty, usize::from(variant.0))
                        && !uninhabited.successor(target)
                })
                .chain(default.iter().map(|target| {
                    uninhabited.uncovered_inhabited_variant(*enum_ty, cases)
                        && !uninhabited.successor(target)
                }))
                .collect(),
            _ => successors
                .iter()
                .map(|target| !uninhabited.successor(target))
                .collect(),
        };
        blocks[block.index()] = ExecutableBlock::Continues(feasible.clone().into());
        for (successor, is_feasible) in successors.into_iter().zip(feasible) {
            if is_feasible && !reached[successor.block.index()] {
                reached[successor.block.index()] = true;
                stack.push(successor.block);
            }
        }
    }
    (ExecutableControlFlow(blocks.into()), may_return)
}

#[salsa::tracked]
fn executable_flow_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<ExecutableControlFlow> {
    let artifacts = normalize_semantic_body(db, instance).ok()?;
    Some(executable_flow(db, &artifacts.body).0)
}

/// The control flow runtime lowering emits, so representation choices and
/// return inference see the same returning paths as callers.
pub fn semantic_executable_control_flow<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<&'db ExecutableControlFlow> {
    executable_flow_query(db, instance).as_ref()
}

/// Whether a call to `instance` can return normally. A body that cannot be
/// normalized is assumed to return.
#[salsa::tracked(returns(copy), cycle_initial=may_return_cycle_initial)]
pub fn semantic_may_return<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    if instance.is_intrinsically_never_returning(db, false) {
        return false;
    }
    normalize_semantic_body(db, instance)
        .map_or(true, |artifacts| executable_flow(db, &artifacts.body).1)
}

/// Whether a call to `instance` can return normally, by its provisional body.
/// A provisional analysis cuts its blocks by this rather than
/// [`semantic_may_return`], whose final bodies depend on the refinements the
/// analysis computes.
#[salsa::tracked(returns(copy), cycle_initial=may_return_cycle_initial)]
pub(crate) fn provisional_may_return<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    if instance.is_intrinsically_never_returning(db, true) {
        return false;
    }
    let Ok(body) = instance.admitted_provisional_body(db) else {
        return true;
    };
    let mut reached = vec![false; body.blocks.len()];
    let mut stack = vec![SBlockId::new(0)];
    reached[0] = true;
    while let Some(block) = stack.pop() {
        let data = &body.blocks[block.index()];
        if data.stmts.iter().any(|stmt| {
            matches!(&stmt.kind, SStmtKind::Assign { expr: SExpr::Call { callee, .. }, .. }
                if !provisional_may_return(db, SemanticInstance::new(db, callee.key)))
        }) {
            continue;
        }
        if matches!(
            data.terminator.kind,
            STerminatorKind::Return(_) | STerminatorKind::Yield { .. }
        ) {
            return true;
        }
        for successor in data.terminator.kind.successors() {
            if !reached[successor.index()] {
                reached[successor.index()] = true;
                stack.push(successor);
            }
        }
    }
    false
}

fn may_return_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> bool {
    false
}
