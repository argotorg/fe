//! Intrinsic lowering for MIR: recognizes core intrinsic calls, statement intrinsics, and
//! code-region resolution.

use super::*;

impl<'db, 'a> MirBuilder<'db, 'a> {
    /// Attempts to lower a statement-only intrinsic call (`mstore`, `codecopy`, etc.).
    ///
    /// # Parameters
    /// - `block`: Current basic block.
    /// - `expr`: Expression id representing the intrinsic call.
    ///
    /// # Returns
    /// The successor block and produced value, or `None` if not an intrinsic stmt.
    pub(super) fn try_lower_intrinsic_stmt(
        &mut self,
        block: BasicBlockId,
        expr: ExprId,
    ) -> Option<(Option<BasicBlockId>, ValueId)> {
        let (op, args) = self.intrinsic_stmt_args(expr)?;
        let value_id = self.ensure_value(expr);
        if matches!(op, IntrinsicOp::ReturnData | IntrinsicOp::Revert) {
            debug_assert!(
                args.len() == 2,
                "terminating intrinsics should have exactly two arguments"
            );
            let offset = args[0];
            let size = args[1];
            let term = match op {
                IntrinsicOp::ReturnData => Terminator::ReturnData { offset, size },
                IntrinsicOp::Revert => Terminator::Revert { offset, size },
                _ => unreachable!(),
            };
            self.set_terminator(block, term);
            return Some((None, value_id));
        }
        self.push_inst(block, MirInst::IntrinsicStmt { expr, op, args });
        Some((Some(block), value_id))
    }

    /// Collects the intrinsic opcode and lowered arguments for a statement-only intrinsic.
    ///
    /// # Parameters
    /// - `expr`: Intrinsic call expression id.
    ///
    /// # Returns
    /// The intrinsic opcode and its argument `ValueId`s, or `None` if not applicable.
    pub(super) fn intrinsic_stmt_args(
        &mut self,
        expr: ExprId,
    ) -> Option<(IntrinsicOp, Vec<ValueId>)> {
        let callable = self.typed_body.callable_expr(expr)?;
        let op = self.intrinsic_kind(callable.callable_def)?;
        if op.returns_value() {
            return None;
        }

        let (args, _) = self.collect_call_args(expr)?;
        Some((op, args))
    }

    /// Maps a callable definition to a known intrinsic opcode.
    ///
    /// # Parameters
    /// - `func_def`: Callable definition to inspect.
    ///
    /// # Returns
    /// Matching `IntrinsicOp` if the callable is a core intrinsic.
    pub(super) fn intrinsic_kind(&self, func_def: CallableDef<'db>) -> Option<IntrinsicOp> {
        if func_def.ingot(self.db).kind(self.db) != IngotKind::Core {
            return None;
        }
        let name = func_def.name(self.db)?;
        match name.data(self.db).as_str() {
            "mload" => Some(IntrinsicOp::Mload),
            "calldataload" => Some(IntrinsicOp::Calldataload),
            "calldatacopy" => Some(IntrinsicOp::Calldatacopy),
            "calldatasize" => Some(IntrinsicOp::Calldatasize),
            "returndatacopy" => Some(IntrinsicOp::Returndatacopy),
            "returndatasize" => Some(IntrinsicOp::Returndatasize),
            "addr_of" => Some(IntrinsicOp::AddrOf),
            "mstore" => Some(IntrinsicOp::Mstore),
            "mstore8" => Some(IntrinsicOp::Mstore8),
            "sload" => Some(IntrinsicOp::Sload),
            "sstore" => Some(IntrinsicOp::Sstore),
            "return_data" => Some(IntrinsicOp::ReturnData),
            "revert" => Some(IntrinsicOp::Revert),
            "codecopy" => Some(IntrinsicOp::Codecopy),
            "codesize" => Some(IntrinsicOp::Codesize),
            "code_region_offset" => Some(IntrinsicOp::CodeRegionOffset),
            "code_region_len" => Some(IntrinsicOp::CodeRegionLen),
            "keccak" => Some(IntrinsicOp::Keccak),
            "caller" => Some(IntrinsicOp::Caller),
            "stor_at" => Some(IntrinsicOp::StorAt),
            "at_offset" => Some(IntrinsicOp::StorAt),
            _ => None,
        }
    }

    /// Resolves the `code_region` target represented by an intrinsic argument path.
    ///
    /// # Parameters
    /// - `expr`: Path expression referencing a function.
    ///
    /// # Returns
    /// A `CodeRegionRoot` describing the referenced function, or `None` on failure.
    pub(super) fn code_region_target(&self, expr: ExprId) -> Option<CodeRegionRoot<'db>> {
        let expr_data = &self.body.exprs(self.db)[expr].borrowed().to_opt()?;
        let Expr::Path(path) = expr_data else {
            return None;
        };
        let path = path.to_opt()?;

        let func_ty = match resolve_path(
            self.db,
            path,
            self.body.scope(),
            PredicateListId::empty_list(self.db),
            true,
        ) {
            Ok(PathRes::Func(func_ty)) => func_ty,
            _ => return None,
        };

        let (base, args) = func_ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = base.data(self.db) else {
            return None;
        };
        let _ = extract_contract_function(self.db, *func)?;
        Some(CodeRegionRoot {
            func: *func,
            generic_args: args.to_vec(),
            symbol: None,
        })
    }
}
