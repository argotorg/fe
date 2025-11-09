use std::{error::Error, fmt};

use hir::hir_def::{
    Body, Expr, ExprId, Func, LitKind, MatchArm, Partial, Pat, PatId, Stmt, StmtId, TopLevelMod,
};
use hir_analysis::{
    HirAnalysisDb,
    ty::{
        ty_check::{TypedBody, check_func_body},
        ty_def::{PrimTy, TyBase, TyData, TyId},
    },
};

use crate::ir::{
    BasicBlock, BasicBlockId, CallOrigin, LoopInfo, MatchArmLowering, MatchArmPattern,
    MatchLoweringInfo, MirBody, MirFunction, MirInst, MirModule, SwitchOrigin, SwitchTarget,
    SwitchValue, Terminator, ValueData, ValueId, ValueOrigin,
};
use rustc_hash::FxHashSet;

#[derive(Debug)]
pub enum MirLowerError {
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

pub type MirLowerResult<T> = Result<T, MirLowerError>;

pub fn lower_module<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> MirLowerResult<MirModule<'db>> {
    let mut module = MirModule::new(top_mod);

    for &func in top_mod.all_funcs(db) {
        let (_diags, typed_body) = check_func_body(db, func);
        let lowered = lower_function(db, func, typed_body.clone())?;
        module.functions.push(lowered);
    }

    Ok(module)
}

fn lower_function<'db>(
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
    let ret_val = builder.ensure_value(body.expr(db));
    if let Some(block) = fallthrough {
        builder.set_terminator(block, Terminator::Return(Some(ret_val)));
    }
    let mir_body = builder.finish();

    Ok(MirFunction {
        func,
        body: mir_body,
        typed_body,
    })
}

struct MirBuilder<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    mir_body: MirBody<'db>,
    loop_stack: Vec<LoopScope>,
}

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
            loop_stack: Vec::new(),
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
                        let literal_bits = literal.bits() as u64;
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

    /// Returns true if the pattern is a wildcard (`_`).
    fn is_wildcard_pat(&self, pat: PatId) -> bool {
        matches!(
            pat.data(self.db, self.body),
            Partial::Present(Pat::WildCard)
        )
    }

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

    /// Lower an expression inside a concrete block, returning the exit block and value.
    fn lower_expr_in(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> (Option<BasicBlockId>, ValueId) {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Block(stmts)) => {
                let next_block = self.lower_block(block, expr, stmts);
                let val = self.ensure_value(expr);
                (next_block, val)
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
                let Some(callable) = self.typed_body.callable_expr(expr) else {
                    return None;
                };
                let mut args = Vec::with_capacity(call_args.len());
                for arg in call_args.iter() {
                    args.push(self.ensure_value(arg.expr));
                }
                (args, callable)
            }
            Partial::Present(Expr::MethodCall(receiver, _, _, call_args)) => {
                let Some(callable) = self.typed_body.callable_expr(expr) else {
                    return None;
                };
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
        Some(self.mir_body.alloc_value(ValueData {
            ty,
            origin: ValueOrigin::Call(CallOrigin {
                expr,
                callable: callable.clone(),
                args,
            }),
        }))
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
                            ty: ty.clone(),
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
