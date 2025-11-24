use std::{error::Error, fmt};

use common::ingot::IngotKind;
use hir::hir_def::{
    Body, CallableDef, Const, Expr, ExprId, Field, FieldIndex, Func, IdentId, LitKind, MatchArm,
    Partial, Pat, PatId, PathId, Stmt, StmtId, TopLevelMod, scope_graph::ScopeId,
};
use hir::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, path_resolver::resolve_path},
    ty::{
        trait_resolution::PredicateListId,
        ty_check::{Callable, RecordLike, TypedBody, check_func_body},
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};

use crate::{
    ir::{
        BasicBlock, BasicBlockId, CallOrigin, IntrinsicOp, IntrinsicValue, LoopInfo,
        MatchArmLowering, MatchArmPattern, MatchLoweringInfo, MirBody, MirFunction, MirInst,
        MirModule, SwitchOrigin, SwitchTarget, SwitchValue, SyntheticValue, Terminator, ValueData,
        ValueId, ValueOrigin,
    },
    monomorphize::monomorphize_functions,
};
use num_bigint::BigUint;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};

/// Errors that can occur while lowering HIR into MIR.
#[derive(Debug)]
pub enum MirLowerError {
    /// The function had no body in HIR even though we attempted to lower it.
    MissingBody { func_name: String },
}

impl fmt::Display for MirLowerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirLowerError::MissingBody { func_name } => {
                write!(f, "function `{func_name}` is missing a body")
            }
        }
    }
}

struct FieldAccessInfo<'db> {
    field_ty: TyId<'db>,
    offset_bytes: u64,
}

/// Returns the width (in bits) for a primitive integer type, if supported.
fn prim_int_bits(prim: PrimTy) -> Option<u16> {
    use PrimTy::*;
    match prim {
        U8 | I8 => Some(8),
        U16 | I16 => Some(16),
        U32 | I32 => Some(32),
        U64 | I64 => Some(64),
        U128 | I128 => Some(128),
        U256 | I256 | Usize | Isize => Some(256),
        _ => None,
    }
}

impl Error for MirLowerError {}

/// Convenient result alias for MIR lowering routines.
pub type MirLowerResult<T> = Result<T, MirLowerError>;

/// Lowers every function inside `top_mod` into MIR and packs the results into a module.
pub fn lower_module<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> MirLowerResult<MirModule<'db>> {
    let mut templates = Vec::new();
    let mut funcs_to_lower = Vec::new();
    let mut seen = FxHashSet::default();

    let mut queue_func = |func: Func<'db>| {
        if seen.insert(func) {
            funcs_to_lower.push(func);
        }
    };

    for &func in top_mod.all_funcs(db) {
        queue_func(func);
    }
    for &impl_block in top_mod.all_impls(db) {
        for func in impl_block.funcs(db) {
            queue_func(func);
        }
    }
    for &impl_trait in top_mod.all_impl_traits(db) {
        for func in impl_trait.methods(db) {
            queue_func(func);
        }
    }

    for func in funcs_to_lower {
        // Trait methods without a body (signatures only) should be ignored by MIR.
        if func.body(db).is_none() {
            continue;
        }

        let (_diags, typed_body) = check_func_body(db, func);
        let lowered = lower_function(db, func, typed_body.clone())?;
        templates.push(lowered);
    }

    let functions = monomorphize_functions(db, templates);
    Ok(MirModule { top_mod, functions })
}

/// Lowers a single HIR function (plus its typed body) into MIR form.
pub(crate) fn lower_function<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    typed_body: TypedBody<'db>,
) -> MirLowerResult<MirFunction<'db>> {
    let Some(body) = func.body(db) else {
        let func_name = func
            .name(db)
            .to_opt()
            .map(|ident| ident.data(db).to_string())
            .unwrap_or_else(|| "<anonymous>".to_string());
        return Err(MirLowerError::MissingBody { func_name });
    };

    let mut builder = MirBuilder::new(db, body, &typed_body);
    let entry = builder.alloc_block();
    let fallthrough = builder.lower_root(entry, body.expr(db));
    builder.ensure_field_expr_values();
    let ret_val = builder.ensure_value(body.expr(db));
    if let Some(block) = fallthrough {
        builder.set_terminator(block, Terminator::Return(Some(ret_val)));
    }
    let mir_body = builder.finish();
    let symbol_name = func
        .name(db)
        .to_opt()
        .map(|ident| ident.data(db).to_string())
        .unwrap_or_else(|| "<anonymous>".into());

    Ok(MirFunction {
        func,
        body: mir_body,
        typed_body,
        generic_args: Vec::new(),
        symbol_name,
    })
}

/// Stateful helper that incrementally builds a MIR body while walking HIR.
struct MirBuilder<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    mir_body: MirBody<'db>,
    /// Cached `core::ptr::get_field` definition used for field reads.
    get_field_func: Option<CallableDef<'db>>,
    /// Cached `core::ptr::store_field` definition used for record writes.
    store_field_func: Option<CallableDef<'db>>,
    /// Cached `core::mem::alloc` definition used for record allocation.
    alloc_func: Option<CallableDef<'db>>,
    address_space_ty: Option<TyId<'db>>,
    loop_stack: Vec<LoopScope>,
    /// Memoized literal values for resolved `const` items.
    const_cache: FxHashMap<Const<'db>, ValueId>,
}

/// Keeps track of the active loop's continue/break targets so `break`/`continue`
/// statements can wire jumps correctly.
#[derive(Clone, Copy)]
struct LoopScope {
    continue_target: BasicBlockId,
    break_target: BasicBlockId,
}

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Create a new MIR builder for the given HIR body and its typed info.
    fn new(db: &'db dyn HirAnalysisDb, body: Body<'db>, typed_body: &'a TypedBody<'db>) -> Self {
        Self {
            db,
            body,
            typed_body,
            mir_body: MirBody::new(),
            get_field_func: None,
            store_field_func: None,
            alloc_func: None,
            address_space_ty: None,
            loop_stack: Vec::new(),
            const_cache: FxHashMap::default(),
        }
    }

    /// Consume the builder and return the constructed MIR body.
    fn finish(self) -> MirBody<'db> {
        self.mir_body
    }

    /// Allocate a fresh basic block and return its identifier.
    fn alloc_block(&mut self) -> BasicBlockId {
        self.mir_body.push_block(BasicBlock::new())
    }

    /// Set the terminator for a basic block.
    fn set_terminator(&mut self, block: BasicBlockId, term: Terminator) {
        self.mir_body.block_mut(block).set_terminator(term);
    }

    /// Append an instruction to the given block.
    fn push_inst(&mut self, block: BasicBlockId, inst: MirInst<'db>) {
        self.mir_body.block_mut(block).push_inst(inst);
    }

    /// Lower the root expression of a body into MIR, starting at `block`.
    fn lower_root(&mut self, block: BasicBlockId, expr: ExprId) -> Option<BasicBlockId> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => self.lower_block(block, expr, stmts),
            _ => {
                let value = self.ensure_value(expr);
                self.mir_body.expr_values.insert(expr, value);
                Some(block)
            }
        }
    }

    /// Lower a literal-only `match` expression into a MIR `Switch`.
    ///
    /// The scrutinee is evaluated exactly once, each arm body gets its own block, and a
    /// merge block is allocated on demand so the `match` still yields a value. We also
    /// record per-arm metadata so codegen can recover which expression belongs to each
    /// case without re-walking HIR.
    fn lower_match_expr(
        &mut self,
        block: BasicBlockId,
        match_expr: ExprId,
        scrutinee: ExprId,
        arms: &[MatchArm],
        patterns: &[MatchArmPattern],
    ) -> (Option<BasicBlockId>, ValueId) {
        debug_assert_eq!(arms.len(), patterns.len());
        let (scrut_block_opt, discr_value) = self.lower_expr_in(block, scrutinee);
        let Some(scrut_block) = scrut_block_opt else {
            let value = self.ensure_value(match_expr);
            return (None, value);
        };

        let mut merge_block: Option<BasicBlockId> = None;
        let mut arm_blocks = Vec::with_capacity(arms.len());
        for arm in arms {
            let arm_entry = self.alloc_block();
            let (arm_end, _) = self.lower_expr_in(arm_entry, arm.body);
            if let Some(end_block) = arm_end {
                let merge = match merge_block {
                    Some(block) => block,
                    None => {
                        let block = self.alloc_block();
                        merge_block = Some(block);
                        block
                    }
                };
                self.set_terminator(end_block, Terminator::Goto { target: merge });
            }
            arm_blocks.push(arm_entry);
        }

        let mut targets = Vec::new();
        let mut default_block = None;
        for (idx, pattern) in patterns.iter().enumerate() {
            let block_id = arm_blocks[idx];
            match pattern {
                MatchArmPattern::Literal(value) => targets.push(SwitchTarget {
                    value: value.clone(),
                    block: block_id,
                }),
                MatchArmPattern::Enum { variant_index, .. } => targets.push(SwitchTarget {
                    value: SwitchValue::Enum(*variant_index),
                    block: block_id,
                }),
                MatchArmPattern::Wildcard => default_block = Some(block_id),
            }
        }

        let default_block = default_block.unwrap_or_else(|| {
            let unreachable_block = self.alloc_block();
            self.set_terminator(unreachable_block, Terminator::Unreachable);
            unreachable_block
        });

        self.set_terminator(
            scrut_block,
            Terminator::Switch {
                discr: discr_value,
                targets,
                default: default_block,
                origin: SwitchOrigin::MatchExpr(match_expr),
            },
        );

        let arms_info = arms
            .iter()
            .zip(patterns.iter())
            .map(|(arm, pattern)| MatchArmLowering {
                pattern: pattern.clone(),
                body: arm.body,
            })
            .collect();

        self.mir_body
            .match_info
            .insert(match_expr, MatchLoweringInfo { arms: arms_info });

        let value_id = self.ensure_value(match_expr);
        (merge_block, value_id)
    }

    /// Collect the literal/wildcard patterns for the given arms.
    ///
    /// Only matches consisting of unique integer/bool literals plus at most one `_`
    /// wildcard are supported. Everything else falls back to the existing lowering
    /// paths by returning `None`.
    fn match_arm_patterns(&self, arms: &[MatchArm]) -> Option<Vec<MatchArmPattern>> {
        if arms.is_empty() {
            return None;
        }

        let mut seen_values: FxHashSet<SwitchValue> = FxHashSet::default();
        let mut has_wildcard = false;
        let mut patterns = Vec::with_capacity(arms.len());

        for arm in arms {
            if self.is_wildcard_pat(arm.pat) {
                if has_wildcard {
                    return None;
                }
                has_wildcard = true;
                patterns.push(MatchArmPattern::Wildcard);
                continue;
            }

            if let Some(value) = self.literal_pat_value(arm.pat) {
                if !seen_values.insert(value.clone()) {
                    return None;
                }
                patterns.push(MatchArmPattern::Literal(value));
                continue;
            }

            if let Some(pattern) = self.enum_pat_value(arm.pat) {
                if let MatchArmPattern::Enum { variant_index, .. } = pattern
                    && !seen_values.insert(SwitchValue::Enum(variant_index))
                {
                    return None;
                }
                patterns.push(pattern);
                continue;
            }

            return None;
        }

        Some(patterns)
    }

    /// Returns the literal value encoded by a pattern if it is supported.
    fn literal_pat_value(&self, pat: PatId) -> Option<SwitchValue> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return None;
        };

        match pat_data {
            Pat::Lit(lit) => {
                let Partial::Present(lit) = lit else {
                    return None;
                };
                match lit {
                    LitKind::Int(value) => {
                        let ty = self.typed_body.pat_ty(self.db, pat);
                        let bits = self.int_type_bits(ty)?;
                        if bits > 256 {
                            return None;
                        }
                        let literal = value.data(self.db).clone();
                        let literal_bits = literal.bits();
                        if literal_bits > bits as u64 {
                            return None;
                        }
                        Some(SwitchValue::Int(literal))
                    }
                    LitKind::Bool(value) => {
                        if !self.typed_body.pat_ty(self.db, pat).is_bool(self.db) {
                            return None;
                        }
                        Some(SwitchValue::Bool(*value))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn enum_pat_value(&self, pat: PatId) -> Option<MatchArmPattern> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return None;
        };

        if let Pat::Path(path, ..) = pat_data
            && let Ok(PathRes::EnumVariant(variant)) = resolve_path(
                self.db,
                path.to_opt().unwrap(),
                self.typed_body.body().unwrap().scope(),
                PredicateListId::empty_list(self.db),
                false,
            )
        {
            let enum_name = variant
                .enum_(self.db)
                .name(self.db)
                .to_opt()
                .unwrap()
                .data(self.db)
                .to_string();
            return Some(MatchArmPattern::Enum {
                variant_index: variant.variant.idx as u64,
                enum_name,
            });
        }

        None
    }

    /// Returns true if the pattern is a wildcard (`_`).
    fn is_wildcard_pat(&self, pat: PatId) -> bool {
        matches!(
            pat.data(self.db, self.body),
            Partial::Present(Pat::WildCard)
        )
    }

    /// Returns the bit width for the given type if it is a primitive integer.
    fn int_type_bits(&self, ty: TyId<'db>) -> Option<u16> {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Prim(prim)) => prim_int_bits(*prim),
            _ => None,
        }
    }

    /// Lower a block expression by sequentially lowering its statements.
    fn lower_block(
        &mut self,
        block: BasicBlockId,
        _block_expr: ExprId,
        stmts: &[StmtId],
    ) -> Option<BasicBlockId> {
        let mut current = Some(block);
        for &stmt_id in stmts {
            let Some(curr_block) = current else { break };
            current = self.lower_stmt(curr_block, stmt_id).0;
        }

        current
    }

    /// Ensure that the given expression has a corresponding MIR value.
    fn ensure_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(&val) = self.mir_body.expr_values.get(&expr) {
            return val;
        }

        let value = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let last_expr = stmts.iter().rev().find_map(|stmt_id| {
                    let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
                        return None;
                    };
                    if let Stmt::Expr(expr_id) = stmt {
                        Some(*expr_id)
                    } else {
                        None
                    }
                });
                if let Some(inner) = last_expr {
                    let val = self.ensure_value(inner);
                    self.mir_body.expr_values.insert(expr, val);
                    return val;
                }
                self.alloc_expr_value(expr)
            }
            _ => self.alloc_expr_value(expr),
        };

        self.mir_body.expr_values.insert(expr, value);
        value
    }

    /// Force every field expression in the body to have a lowered MIR value so later
    /// stages can look up the synthesized `get_field` call even when the field only
    /// appears inside larger expressions (e.g. arithmetic).
    fn ensure_field_expr_values(&mut self) {
        let exprs = self.body.exprs(self.db);
        for expr_id in exprs.keys() {
            let Partial::Present(expr) = &exprs[expr_id] else {
                continue;
            };
            if matches!(expr, Expr::Field(..)) {
                self.ensure_value(expr_id);
            }
        }
    }

    /// Lower an expression inside a concrete block, returning the exit block and value.
    fn lower_expr_in(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, ValueId) {
        if let Some(result) = self.try_lower_intrinsic_stmt(block, expr) {
            return result;
        }
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let next_block = self.lower_block(block, expr, stmts);
                let val = self.ensure_value(expr);
                (next_block, val)
            }
            Partial::Present(Expr::RecordInit(_, fields)) => {
                self.try_lower_record(block, expr, fields)
            }
            Partial::Present(Expr::Match(scrutinee, arms)) => {
                if let Partial::Present(arms) = arms {
                    // Try to lower `match` into a `Switch` if it only uses supported patterns.
                    if let Some(patterns) = self.match_arm_patterns(arms) {
                        return self.lower_match_expr(block, expr, *scrutinee, arms, &patterns);
                    }
                }
                let val = self.ensure_value(expr);
                (Some(block), val)
            }
            _ => {
                let val = self.ensure_value(expr);
                (Some(block), val)
            }
        }
    }

    /// Allocate the MIR value slot for an expression, handling special cases.
    fn alloc_expr_value(&mut self, expr: ExprId) -> ValueId {
        if let Some(value) = self.try_lower_call(expr) {
            return value;
        }
        if let Some(value) = self.try_lower_field(expr) {
            return value;
        }
        if let Some(value) = self.try_const_expr(expr) {
            return value;
        }

        let ty = self.typed_body.expr_ty(self.db, expr);
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Expr(expr),
        })
    }

    /// Attempt to lower a function or method call into a call-origin MIR value.
    fn try_lower_call(&mut self, expr: ExprId) -> Option<ValueId> {
        let (args, callable) = match expr.data(self.db, self.body) {
            Partial::Present(Expr::Call(_, call_args)) => {
                let callable = self.typed_body.callable_expr(expr)?;
                let mut args = Vec::with_capacity(call_args.len());
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
                (args, callable)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                let callable = self.typed_body.callable_expr(expr)?;
                let mut args = Vec::with_capacity(call_args.len() + 1);
                args.push(self.ensure_value(*receiver));
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
                (args, callable)
            }
            _ => return None,
        };

        let ty = self.typed_body.expr_ty(self.db, expr);
        if let Some(kind) = self.intrinsic_kind(callable.callable_def) {
            if !kind.returns_value() {
                return None;
            }
            return Some(self.mir_body.alloc_value(ValueData {
                ty,
                origin: ValueOrigin::Intrinsic(IntrinsicValue { op: kind, args }),
            }));
        }
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: callable.clone(),
                args,
                resolved_name: None,
            }),
        }))
    }

    /// Rewrites `Expr::Field` expressions into calls to the standard library
    /// `get_field` helper so later stages do not need bespoke field logic.
    fn try_lower_field(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Field(lhs, field_index)) = expr.data(self.db, self.body) else {
            return None;
        };
        let field_index = field_index.to_opt()?;
        let lhs_ty = self.typed_body.expr_ty(self.db, *lhs);
        let info = self.field_access_info(lhs_ty, field_index)?;

        let addr_value = self.ensure_value(*lhs);
        let space_value = self.synthetic_address_space_memory()?;
        let offset_value = self.synthetic_u256(BigUint::from(info.offset_bytes));
        let callable = self.get_field_callable(expr, lhs_ty, info.field_ty)?;

        Some(self.mir_body.alloc_value(ValueData {
            ty: info.field_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable,
                args: vec![addr_value, space_value, offset_value],
                resolved_name: None,
            }),
        }))
    }

    /// Attempts to resolve a path expression to a literal `const` value.
    ///
    /// Returns a MIR `ValueId` referencing a synthetic literal when the path
    /// points to a constant that ultimately evaluates to an integer or boolean.
    fn try_const_expr(&mut self, expr: ExprId) -> Option<ValueId> {
        let Partial::Present(Expr::Path(path)) = expr.data(self.db, self.body) else {
            return None;
        };
        let path = path.to_opt()?;
        let mut visited = FxHashSet::default();
        self.const_literal_from_path(path, self.body.scope(), &mut visited)
    }

    /// Resolves the given path to a const definition in `scope`.
    ///
    /// Returns the `ValueId` for its literal, or `None` if the const is not a
    /// literal or fails to resolve.
    fn const_literal_from_path(
        &mut self,
        path: PathId<'db>,
        scope: ScopeId<'db>,
        visited: &mut FxHashSet<Const<'db>>,
    ) -> Option<ValueId> {
        let PathRes::Const(const_def, ty) = resolve_path(
            self.db,
            path,
            scope,
            PredicateListId::empty_list(self.db),
            true,
        )
        .ok()?
        else {
            return None;
        };
        self.const_literal_from_def(const_def, ty, visited)
    }

    /// Converts a concrete const definition into a MIR literal.
    ///
    /// Returns a cached or newly allocated `ValueId` representing the literal,
    /// or `None` if the const body is not a literal or forms a cycle.
    fn const_literal_from_def(
        &mut self,
        const_def: Const<'db>,
        ty: TyId<'db>,
        visited: &mut FxHashSet<Const<'db>>,
    ) -> Option<ValueId> {
        if let Some(&value) = self.const_cache.get(&const_def) {
            return Some(value);
        }
        if !visited.insert(const_def) {
            return None;
        }
        let body = const_def.body(self.db).to_opt()?;
        let expr_id = body.expr(self.db);
        let expr = match expr_id.data(self.db, body) {
            Partial::Present(expr) => expr,
            Partial::Absent => {
                visited.remove(&const_def);
                return None;
            }
        };
        let const_scope = body.scope();
        let result = match expr {
            Expr::Lit(LitKind::Int(value)) => Some(
                self.alloc_synthetic_value(ty, SyntheticValue::Int(value.data(self.db).clone())),
            ),
            Expr::Lit(LitKind::Bool(flag)) => {
                Some(self.alloc_synthetic_value(ty, SyntheticValue::Bool(*flag)))
            }
            Expr::Path(path) => {
                if let Some(inner_path) = path.to_opt() {
                    self.const_literal_from_path(inner_path, const_scope, visited)
                } else {
                    None
                }
            }
            _ => None,
        };
        visited.remove(&const_def);
        if let Some(value_id) = result {
            self.const_cache.insert(const_def, value_id);
        }
        result
    }

    /// Allocates a synthetic literal value with the provided type.
    ///
    /// Returns the new `ValueId` stored in the MIR body.
    fn alloc_synthetic_value(&mut self, ty: TyId<'db>, value: SyntheticValue) -> ValueId {
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(value),
        })
    }

    /// Lowers a record literal into an `alloc` call followed by `store_field` writes so the
    /// expression evaluates to the concrete pointer returned by `alloc`.
    fn try_lower_record(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
        fields: &[Field<'db>],
    ) -> (Option<BasicBlockId>, ValueId) {
        // First lower each field expression so we know the value to write later.
        let mut current = Some(block);
        let mut lowered_fields = Vec::with_capacity(fields.len());
        for field in fields {
            let Some(curr_block) = current else {
                break;
            };
            let (next_block, value) = self.lower_expr_in(curr_block, field.expr);
            current = next_block;
            let Some(label) = field.label_eagerly(self.db, self.body) else {
                let value_id = self.ensure_value(expr);
                return (current, value_id);
            };
            lowered_fields.push((label, value));
        }

        let Some(curr_block) = current else {
            let value_id = self.ensure_value(expr);
            return (None, value_id);
        };

        let record_ty = self.typed_body.expr_ty(self.db, expr);
        let record_like = RecordLike::from_ty(record_ty);
        let Some(size_bytes) = self.record_size_bytes(&record_like) else {
            let value_id = self.ensure_value(expr);
            return (Some(curr_block), value_id);
        };

        // Emit the call to `alloc(size)` and bind its result so subsequent stores can re-use it.
        let Some(alloc_callable) = self.get_alloc_callable(expr) else {
            let value_id = self.ensure_value(expr);
            return (Some(curr_block), value_id);
        };
        let alloc_ret_ty = alloc_callable.ret_ty(self.db);
        let size_value = self.synthetic_u256(BigUint::from(size_bytes));
        let alloc_value = self.mir_body.alloc_value(ValueData {
            ty: alloc_ret_ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: alloc_callable,
                args: vec![size_value],
                resolved_name: None,
            }),
        });
        self.push_inst(
            curr_block,
            MirInst::EvalExpr {
                expr,
                value: alloc_value,
                bind_value: true,
            },
        );

        let value_id = self.ensure_value(expr);
        let Some(space_value) = self.synthetic_address_space_memory() else {
            return (Some(curr_block), value_id);
        };

        // Call `store_field` for every initialized member, re-using the allocated pointer.
        for (label, field_value) in lowered_fields {
            let field_index = FieldIndex::Ident(label);
            let Some(info) = self.field_access_info(record_ty, field_index) else {
                continue;
            };
            let Some(store_callable) = self.get_store_field_callable(expr, info.field_ty) else {
                continue;
            };
            let offset_value = self.synthetic_u256(BigUint::from(info.offset_bytes));
            let store_ret_ty = store_callable.ret_ty(self.db);
            let store_call = self.mir_body.alloc_value(ValueData {
                ty: store_ret_ty,
                origin: ValueOrigin::Call(CallOrigin {
                    expr,
                    callable: store_callable,
                    args: vec![value_id, space_value, offset_value, field_value],
                    resolved_name: None,
                }),
            });
            self.push_inst(
                curr_block,
                MirInst::EvalExpr {
                    expr,
                    value: store_call,
                    bind_value: false,
                },
            );
        }

        (Some(curr_block), value_id)
    }

    /// Attempts to lower a statement-only intrinsic call (`mstore`, `sstore`, …).
    fn try_lower_intrinsic_stmt(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> Option<(Option<BasicBlockId>, ValueId)> {
        let (op, args) = self.intrinsic_stmt_args(expr)?;
        let value_id = self.ensure_value(expr);
        self.push_inst(block, MirInst::IntrinsicStmt { expr, op, args });
        Some((Some(block), value_id))
    }

    fn intrinsic_stmt_args(&mut self, expr: ExprId) -> Option<(IntrinsicOp, Vec<ValueId>)> {
        let callable = self.typed_body.callable_expr(expr)?;
        let op = self.intrinsic_kind(callable.callable_def)?;
        if op.returns_value() {
            return None;
        }

        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return None;
        };

        let mut args = Vec::new();
        match expr_data {
            Expr::Call(_, call_args) => {
                args.reserve(call_args.len());
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
            }
            Expr::MethodCall(receiver, _, _, call_args) => {
                args.reserve(call_args.len() + 1);
                args.push(self.ensure_value(*receiver));
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
            }
            _ => return None,
        }
        Some((op, args))
    }

    /// Returns the field type and byte offset for a given receiver/field pair.
    fn field_access_info(
        &self,
        owner_ty: TyId<'db>,
        field_index: FieldIndex<'db>,
    ) -> Option<FieldAccessInfo<'db>> {
        let record_like = RecordLike::from_ty(owner_ty);
        let idx = match field_index {
            FieldIndex::Ident(label) => record_like.record_field_idx(self.db, label)?,
            FieldIndex::Index(integer) => integer.data(self.db).to_usize()?,
        };
        let (field_ty, offset_bytes) = self.field_layout_by_index(&record_like, idx)?;
        Some(FieldAccessInfo {
            field_ty,
            offset_bytes,
        })
    }

    /// Computes `(field_ty, offset_bytes)` for the `idx`th field of a record.
    fn field_layout_by_index(
        &self,
        record_like: &RecordLike<'db>,
        idx: usize,
    ) -> Option<(TyId<'db>, u64)> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::Variant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);
        if idx >= field_types.len() {
            return None;
        }

        let mut offset = 0u64;
        for i in 0..idx {
            let size = self.ty_size_bytes(field_types[i])?;
            offset += size;
        }
        Some((field_types[idx], offset))
    }

    /// Computes the total byte width of a record by summing its fields.
    fn record_size_bytes(&self, record_like: &RecordLike<'db>) -> Option<u64> {
        let ty = match record_like {
            RecordLike::Type(ty) => *ty,
            RecordLike::Variant(variant) => variant.ty,
        };
        let field_types = ty.field_types(self.db);

        let mut size = 0u64;
        for field_ty in field_types {
            let field_size = self.ty_size_bytes(field_ty)?;
            size += field_size;
        }
        Some(size)
    }

    /// Returns the byte width of primitive integer/bool types we can layout today.
    fn ty_size_bytes(&self, ty: TyId<'db>) -> Option<u64> {
        match ty.data(self.db) {
            TyData::TyBase(TyBase::Prim(prim)) => {
                if *prim == PrimTy::Bool {
                    Some(1)
                } else {
                    prim_int_bits(*prim).map(|bits| (bits / 8) as u64)
                }
            }
            _ => None,
        }
    }

    /// Builds the callable metadata for the `get_field` helper.
    fn get_field_callable(
        &mut self,
        expr: ExprId,
        owner_ty: TyId<'db>,
        field_ty: TyId<'db>,
    ) -> Option<Callable<'db>> {
        let func_def = self.resolve_get_field_func()?;
        let params = func_def.params(self.db);
        if params.len() < 2 {
            return None;
        }
        let ty = TyId::foldl(
            self.db,
            TyId::func(self.db, func_def),
            &[owner_ty, field_ty],
        );
        Callable::new(self.db, ty, expr.span(self.body).into(), None).ok()
    }

    /// Builds the callable metadata for the `store_field` helper.
    fn get_store_field_callable(
        &mut self,
        expr: ExprId,
        field_ty: TyId<'db>,
    ) -> Option<Callable<'db>> {
        let func_def = self.resolve_store_field_func()?;
        let ty = TyId::foldl(self.db, TyId::func(self.db, func_def), &[field_ty]);
        Callable::new(self.db, ty, expr.span(self.body).into(), None).ok()
    }

    /// Builds the callable metadata for `core::mem::alloc`.
    fn get_alloc_callable(&mut self, expr: ExprId) -> Option<Callable<'db>> {
        let func_def = self.resolve_alloc_func()?;
        let ty = TyId::func(self.db, func_def);
        Callable::new(self.db, ty, expr.span(self.body).into(), None).ok()
    }

    fn resolve_get_field_func(&mut self) -> Option<CallableDef<'db>> {
        if let Some(func) = self.get_field_func {
            return Some(func);
        }
        if let Some(func) = self.resolve_get_field_via_path() {
            self.get_field_func = Some(func);
            return Some(func);
        }
        if let Some(func) = self.find_local_get_field() {
            self.get_field_func = Some(func);
            return Some(func);
        }
        None
    }

    fn resolve_store_field_func(&mut self) -> Option<CallableDef<'db>> {
        if let Some(func) = self.store_field_func {
            return Some(func);
        }
        if let Some(func) = self.resolve_store_field_via_path() {
            self.store_field_func = Some(func);
            return Some(func);
        }
        if let Some(func) = self.find_local_store_field() {
            self.store_field_func = Some(func);
            return Some(func);
        }
        None
    }

    fn resolve_alloc_func(&mut self) -> Option<CallableDef<'db>> {
        if let Some(func) = self.alloc_func {
            return Some(func);
        }
        if let Some(func) = self.resolve_alloc_via_path() {
            self.alloc_func = Some(func);
            return Some(func);
        }
        if let Some(func) = self.find_local_alloc() {
            self.alloc_func = Some(func);
            return Some(func);
        }
        None
    }

    fn resolve_get_field_via_path(&self) -> Option<CallableDef<'db>> {
        let mut path = self.resolve_core_path(&["core", "ptr", "get_field"]);
        if path.is_none() {
            path = self.resolve_core_path(&["core", "get_field"]);
        }
        let PathRes::Func(func_ty) = path? else {
            return None;
        };
        let base = func_ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
            Some(*func_def)
        } else {
            None
        }
    }

    fn resolve_store_field_via_path(&self) -> Option<CallableDef<'db>> {
        let mut path = self.resolve_core_path(&["core", "ptr", "store_field"]);
        if path.is_none() {
            path = self.resolve_core_path(&["core", "store_field"]);
        }
        let PathRes::Func(func_ty) = path? else {
            return None;
        };
        let base = func_ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
            Some(*func_def)
        } else {
            None
        }
    }

    fn resolve_alloc_via_path(&self) -> Option<CallableDef<'db>> {
        let mut path = self.resolve_core_path(&["core", "mem", "alloc"]);
        if path.is_none() {
            path = self.resolve_core_path(&["core", "alloc"]);
        }
        let PathRes::Func(func_ty) = path? else {
            return None;
        };
        let base = func_ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Func(func_def)) = base.data(self.db) {
            Some(*func_def)
        } else {
            None
        }
    }

    fn find_local_get_field(&self) -> Option<CallableDef<'db>> {
        let top_mod = self.body.top_mod(self.db);
        for &func in top_mod.all_funcs(self.db) {
            let name = func.name(self.db).to_opt()?;
            if name.data(self.db) == "get_field"
                && let Some(def) = func.as_callable(self.db)
            {
                return Some(def);
            }
        }
        None
    }

    fn find_local_store_field(&self) -> Option<CallableDef<'db>> {
        let top_mod = self.body.top_mod(self.db);
        for &func in top_mod.all_funcs(self.db) {
            let name = func.name(self.db).to_opt()?;
            if name.data(self.db) == "store_field"
                && let Some(def) = func.as_callable(self.db)
            {
                return Some(def);
            }
        }
        None
    }

    fn find_local_alloc(&self) -> Option<CallableDef<'db>> {
        let top_mod = self.body.top_mod(self.db);
        for &func in top_mod.all_funcs(self.db) {
            let name = func.name(self.db).to_opt()?;
            if name.data(self.db) == "alloc"
                && let Some(def) = func.as_callable(self.db)
            {
                return Some(def);
            }
        }
        None
    }

    /// Returns which intrinsic operation the given function represents, if any.
    fn intrinsic_kind(&self, func_def: CallableDef<'db>) -> Option<IntrinsicOp> {
        if func_def.ingot(self.db).kind(self.db) != IngotKind::Core {
            return None;
        }
        let name = func_def.name(self.db)?;
        match name.data(self.db).as_str() {
            "mload" => Some(IntrinsicOp::Mload),
            "mstore" => Some(IntrinsicOp::Mstore),
            "mstore8" => Some(IntrinsicOp::Mstore8),
            "sload" => Some(IntrinsicOp::Sload),
            "sstore" => Some(IntrinsicOp::Sstore),
            _ => None,
        }
    }

    /// Emits a synthetic `u256` literal value.
    fn synthetic_u256(&mut self, value: BigUint) -> ValueId {
        let ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(value)),
        })
    }

    /// Emits a synthetic `AddressSpace::Memory` literal.
    fn synthetic_address_space_memory(&mut self) -> Option<ValueId> {
        let ty = self.address_space_ty()?;
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Int(BigUint::from(0u8))),
        }))
    }

    /// Caches the `AddressSpace` type so synthetic values can reference it.
    fn address_space_ty(&mut self) -> Option<TyId<'db>> {
        if let Some(ty) = self.address_space_ty {
            return Some(ty);
        }
        let mut path = self.resolve_core_path(&["core", "ptr", "AddressSpace"]);
        if path.is_none() {
            path = self.resolve_core_path(&["core", "AddressSpace"]);
        }
        let PathRes::Ty(ty) = path? else {
            return None;
        };
        self.address_space_ty = Some(ty);
        Some(ty)
    }

    /// Resolves a fully-qualified core path like `["core","ptr","foo"]`.
    fn resolve_core_path(&self, segments: &[&str]) -> Option<PathRes<'db>> {
        let mut iter = segments.iter();
        let first = *iter.next()?;
        let mut path = PathId::from_ident(self.db, self.make_ident(first));
        for segment in iter {
            path = path.push_ident(self.db, self.make_ident(segment));
        }
        resolve_path(
            self.db,
            path,
            self.body.scope(),
            PredicateListId::empty_list(self.db),
            true,
        )
        .ok()
    }

    fn make_ident(&self, segment: &str) -> IdentId<'db> {
        if segment == "core" {
            IdentId::make_core(self.db)
        } else {
            IdentId::new(self.db, segment.to_string())
        }
    }

    /// Lower a statement, returning the successor block and (optional) produced value.
    fn lower_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let Partial::Present(stmt) = stmt_id.data(self.db, self.body) else {
            return (Some(block), None);
        };
        match stmt {
            Stmt::Let(pat, ty, value) => {
                let (next_block, value_id) = if let Some(expr) = value {
                    let (next_block, val) = self.lower_expr_in(block, *expr);
                    (next_block, Some(val))
                } else {
                    (Some(block), None)
                };
                if let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::Let {
                            stmt: stmt_id,
                            pat: *pat,
                            ty: *ty,
                            value: value_id,
                        },
                    );
                }
                (next_block, None)
            }
            Stmt::For(_, _, _) => {
                panic!("for loops are not supported in MIR lowering yet");
            }
            Stmt::While(cond, body_expr) => self.lower_while(block, *cond, *body_expr),
            Stmt::Continue => {
                let scope = self.loop_stack.last().expect("continue outside of loop");
                self.set_terminator(
                    block,
                    Terminator::Goto {
                        target: scope.continue_target,
                    },
                );
                (None, None)
            }
            Stmt::Break => {
                let scope = self.loop_stack.last().expect("break outside of loop");
                self.set_terminator(
                    block,
                    Terminator::Goto {
                        target: scope.break_target,
                    },
                );
                (None, None)
            }
            Stmt::Return(value) => {
                let (next_block, ret_value) = if let Some(expr) = value {
                    let (next_block, val) = self.lower_expr_in(block, *expr);
                    (next_block, Some(val))
                } else {
                    (Some(block), None)
                };
                if let Some(curr_block) = next_block {
                    self.set_terminator(curr_block, Terminator::Return(ret_value));
                }
                (None, None)
            }
            Stmt::Expr(expr) => self.lower_expr_stmt(block, stmt_id, *expr),
        }
    }

    /// Lower a `while` loop statement and wire up its control-flow edges.
    fn lower_while(
        &mut self,
        block: BasicBlockId,
        cond_expr: ExprId,
        body_expr: ExprId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        let cond_entry = self.alloc_block();
        let body_block = self.alloc_block();
        let exit_block = self.alloc_block();

        self.set_terminator(block, Terminator::Goto { target: cond_entry });

        let (cond_header_opt, cond_val) = self.lower_expr_in(cond_entry, cond_expr);
        let Some(cond_header) = cond_header_opt else {
            return (None, None);
        };

        self.loop_stack.push(LoopScope {
            continue_target: cond_entry,
            break_target: exit_block,
        });

        let body_end = self.lower_expr_in(body_block, body_expr).0;

        self.loop_stack.pop();

        let mut backedge = None;
        if let Some(body_end_block) = body_end {
            self.set_terminator(body_end_block, Terminator::Goto { target: cond_entry });
            backedge = Some(body_end_block);
        }

        self.set_terminator(
            cond_header,
            Terminator::Branch {
                cond: cond_val,
                then_bb: body_block,
                else_bb: exit_block,
            },
        );

        self.mir_body.loop_headers.insert(
            cond_entry,
            LoopInfo {
                body: body_block,
                exit: exit_block,
                backedge,
            },
        );

        (Some(exit_block), None)
    }

    /// Lower an `if` expression used in statement position.
    fn lower_if_expr(
        &mut self,
        block: BasicBlockId,
        if_expr: ExprId,
        cond: ExprId,
        then_expr: ExprId,
        else_expr: Option<ExprId>,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        if !self.is_unit_ty(self.typed_body.expr_ty(self.db, if_expr)) {
            let value = self.ensure_value(if_expr);
            return (Some(block), Some(value));
        }

        let (cond_block_opt, cond_val) = self.lower_expr_in(block, cond);
        let cond_block = match cond_block_opt {
            Some(block) => block,
            None => return (None, None),
        };

        let then_block = self.alloc_block();
        let merge_block = self.alloc_block();
        let else_block = if else_expr.is_some() {
            self.alloc_block()
        } else {
            merge_block
        };

        self.set_terminator(
            cond_block,
            Terminator::Branch {
                cond: cond_val,
                then_bb: then_block,
                else_bb: else_block,
            },
        );

        let then_end = self.lower_expr_in(then_block, then_expr).0;
        if let Some(end_block) = then_end {
            self.set_terminator(
                end_block,
                Terminator::Goto {
                    target: merge_block,
                },
            );
        }

        if let Some(else_expr) = else_expr {
            let else_end = self.lower_expr_in(else_block, else_expr).0;
            if let Some(end_block) = else_end {
                self.set_terminator(
                    end_block,
                    Terminator::Goto {
                        target: merge_block,
                    },
                );
            }
        }

        (Some(merge_block), None)
    }

    /// Returns whether the given type represents the unit value.
    fn is_unit_ty(&self, ty: TyId<'db>) -> bool {
        ty.is_tuple(self.db) && ty.field_count(self.db) == 0
    }

    /// Lower an expression statement, producing its continuation block/value.
    fn lower_expr_stmt(
        &mut self,
        block: BasicBlockId,
        stmt_id: StmtId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, Option<ValueId>) {
        if let Some((next_block, value_id)) = self.try_lower_intrinsic_stmt(block, expr) {
            return (next_block, Some(value_id));
        }
        let exprs = self.body.exprs(self.db);
        let Partial::Present(expr_data) = &exprs[expr] else {
            return (Some(block), None);
        };

        match expr_data {
            Expr::Assign(target, value) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::Assign {
                            stmt: stmt_id,
                            target: *target,
                            value: value_id,
                        },
                    );
                }
                (next_block, None)
            }
            Expr::If(cond, then_expr, else_expr) => {
                let (next_block, value_id) =
                    self.lower_if_expr(block, expr, *cond, *then_expr, *else_expr);
                if let (Some(curr_block), Some(value)) = (next_block, value_id) {
                    self.push_inst(
                        curr_block,
                        MirInst::Eval {
                            stmt: stmt_id,
                            value,
                        },
                    );
                }
                (next_block, value_id)
            }
            Expr::AugAssign(target, value, op) => {
                let (next_block, value_id) = self.lower_expr_in(block, *value);
                if let Some(curr_block) = next_block {
                    self.push_inst(
                        curr_block,
                        MirInst::AugAssign {
                            stmt: stmt_id,
                            target: *target,
                            value: value_id,
                            op: *op,
                        },
                    );
                }
                (next_block, None)
            }
            Expr::Block(stmts) => {
                let next_block = self.lower_block(block, expr, stmts);
                let value_id = self.ensure_value(expr);
                (next_block, Some(value_id))
            }
            Expr::Match(scrutinee, arms) => {
                if let Partial::Present(arms) = arms {
                    // Expression-position match: re-use the same lowering so we can produce a value.
                    if let Some(patterns) = self.match_arm_patterns(arms) {
                        let (next_block, value_id) =
                            self.lower_match_expr(block, expr, *scrutinee, arms, &patterns);
                        return (next_block, Some(value_id));
                    }
                }
                let value_id = self.ensure_value(expr);
                self.push_inst(
                    block,
                    MirInst::Eval {
                        stmt: stmt_id,
                        value: value_id,
                    },
                );
                (Some(block), Some(value_id))
            }
            _ => {
                let value_id = self.ensure_value(expr);
                self.push_inst(
                    block,
                    MirInst::Eval {
                        stmt: stmt_id,
                        value: value_id,
                    },
                );
                (Some(block), Some(value_id))
            }
        }
    }
}
