//! Intrinsic lowering for MIR: recognizes core intrinsic calls, statement intrinsics, and
//! code-region resolution.

use super::*;

impl<'db, 'a> MirBuilder<'db, 'a> {
    pub(super) fn callable_def_for_call_expr(&self, expr: ExprId) -> Option<CallableDef<'db>> {
        Some(self.typed_body.callable_expr(expr)?.callable_def)
    }

    /// Attempts to lower a statement-only intrinsic call (`mstore`, `codecopy`, etc.).
    ///
    /// # Parameters
    /// - `expr`: Expression id representing the intrinsic call.
    ///
    /// # Returns
    /// The produced value, or `None` if not an intrinsic stmt.
    pub(super) fn try_lower_intrinsic_stmt(&mut self, expr: ExprId) -> Option<ValueId> {
        if self.current_block().is_none() {
            return Some(self.ensure_value(expr));
        }
        let (op, args) = self.intrinsic_stmt_args(expr)?;
        let value_id = self.ensure_value(expr);
        if matches!(op, IntrinsicOp::ReturnData | IntrinsicOp::Revert) {
            debug_assert!(
                args.len() == 2,
                "terminating intrinsics should have exactly two arguments"
            );
            self.set_current_terminator(Terminator::TerminatingCall(
                crate::ir::TerminatingCall::Intrinsic { op, args },
            ));
            return Some(value_id);
        }
        self.push_inst_here(MirInst::Assign {
            stmt: None,
            dest: None,
            rvalue: crate::ir::Rvalue::Intrinsic { op, args },
        });
        Some(value_id)
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
        let callable_def = self.callable_def_for_call_expr(expr)?;
        let op = self.intrinsic_kind(callable_def)?;
        if op.returns_value() {
            return None;
        }

        let (mut args, _) = self.collect_call_args(expr)?;
        let is_method_call = matches!(
            expr.data(self.db, self.body),
            Partial::Present(Expr::MethodCall(..))
        );
        if is_method_call && !args.is_empty() {
            args.remove(0);
        }
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
        match func_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return None,
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
            "keccak" | "keccak256" => Some(IntrinsicOp::Keccak),
            "caller" => Some(IntrinsicOp::Caller),
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
        let ty = self.typed_body.expr_ty(self.db, expr);
        let (base, args) = ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = base.data(self.db) else {
            return None;
        };
        let _ = extract_contract_function(self.db, *func)?;
        let generic_args = args.to_vec();
        debug_assert!(
            generic_args
                .iter()
                .all(|ty| !matches!(ty.data(self.db), TyData::TyVar(_))),
            "code_region target generic args should never contain TyVar; this should be canonicalized during typing"
        );
        Some(CodeRegionRoot {
            origin: crate::ir::MirFunctionOrigin::Hir(*func),
            generic_args,
            symbol: None,
        })
    }

    /// Resolves the `code_region` target represented by a function-item type.
    ///
    /// This is used when contract code regions are passed through locals/params (e.g. `fn f<F>(x: F)`),
    /// where the value has no runtime representation but the *type* still uniquely identifies the
    /// referenced contract entrypoint.
    pub(super) fn code_region_target_from_ty(&self, ty: TyId<'db>) -> Option<CodeRegionRoot<'db>> {
        let (base, args) = ty.decompose_ty_app(self.db);
        let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = base.data(self.db) else {
            return None;
        };
        let _ = extract_contract_function(self.db, *func)?;
        Some(CodeRegionRoot {
            origin: crate::ir::MirFunctionOrigin::Hir(*func),
            generic_args: args.to_vec(),
            symbol: None,
        })
    }
}
