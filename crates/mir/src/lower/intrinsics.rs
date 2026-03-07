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
            self.set_current_terminator(Terminator::TerminatingCall {
                source: crate::ir::SourceInfoId::SYNTHETIC,
                call: crate::ir::TerminatingCall::Intrinsic { op, args },
            });
            return Some(value_id);
        }
        self.push_inst_here(MirInst::Assign {
            source: crate::ir::SourceInfoId::SYNTHETIC,
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
        let CallableDef::Func(func) = func_def else {
            return None;
        };
        if func.body(self.db).is_some() {
            return None;
        }
        let name = func.name(self.db).to_opt()?;
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
            "alloc" => Some(IntrinsicOp::Alloc),
            "sload" => Some(IntrinsicOp::Sload),
            "sstore" => Some(IntrinsicOp::Sstore),
            "return_data" => Some(IntrinsicOp::ReturnData),
            "revert" => Some(IntrinsicOp::Revert),
            "codecopy" => Some(IntrinsicOp::Codecopy),
            "codesize" => Some(IntrinsicOp::Codesize),
            "code_region_offset" => Some(IntrinsicOp::CodeRegionOffset),
            "code_region_len" => Some(IntrinsicOp::CodeRegionLen),
            "keccak" | "keccak256" => Some(IntrinsicOp::Keccak),
            "addmod" => Some(IntrinsicOp::Addmod),
            "mulmod" => Some(IntrinsicOp::Mulmod),
            "caller" => Some(IntrinsicOp::Caller),
            _ => None,
        }
    }

    /// Returns `true` if the callable definition refers to the `__bitcast` intrinsic.
    ///
    /// This is the generic bitcast intrinsic `__bitcast<From, To>(value: From) -> To` in core
    /// that reinterprets bits between primitive integer types without runtime cost.
    pub(super) fn is_cast_intrinsic(&self, func_def: CallableDef<'db>) -> bool {
        match func_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return false,
        }

        let CallableDef::Func(func) = func_def else {
            return false;
        };
        if func.body(self.db).is_some() {
            return false;
        }

        let Some(name) = func_def.name(self.db) else {
            return false;
        };
        name.data(self.db).as_str() == "__bitcast"
    }

    pub(super) fn checked_intrinsic_kind(
        &self,
        func_def: CallableDef<'db>,
        ty: TyId<'db>,
    ) -> Option<crate::ir::CheckedIntrinsic<'db>> {
        match func_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return None,
        }

        let CallableDef::Func(func) = func_def else {
            return None;
        };
        if func.body(self.db).is_some() {
            return None;
        }

        let name = func_def.name(self.db)?;
        let inner_ty = ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(ty);
        let op = match name.data(self.db).as_str() {
            "__checked_add" => crate::ir::CheckedArithmeticOp::Add,
            "__checked_sub" => crate::ir::CheckedArithmeticOp::Sub,
            "__checked_mul" => crate::ir::CheckedArithmeticOp::Mul,
            "__checked_neg" => crate::ir::CheckedArithmeticOp::Neg,
            _ => return None,
        };

        self.checked_intrinsic_ty_suffix(inner_ty)?;

        Some(crate::ir::CheckedIntrinsic { op, ty: inner_ty })
    }

    pub(super) fn builtin_terminator_kind(
        &self,
        func_def: CallableDef<'db>,
    ) -> Option<crate::ir::BuiltinTerminatorKind> {
        match func_def.ingot(self.db).kind(self.db) {
            IngotKind::Core => {}
            _ => return None,
        }

        let CallableDef::Func(func) = func_def else {
            return None;
        };
        if func.body(self.db).is_some() {
            return None;
        }

        let name = func_def.name(self.db)?;
        match name.data(self.db).as_str() {
            "panic" | "todo" => Some(crate::ir::BuiltinTerminatorKind::Abort),
            _ => None,
        }
    }

    fn checked_intrinsic_ty_suffix(&self, ty: TyId<'db>) -> Option<&'static str> {
        let base_ty = ty.base_ty(self.db);
        let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(self.db) else {
            return None;
        };
        match prim {
            PrimTy::U8 => Some("u8"),
            PrimTy::U16 => Some("u16"),
            PrimTy::U32 => Some("u32"),
            PrimTy::U64 => Some("u64"),
            PrimTy::U128 => Some("u128"),
            PrimTy::U256 => Some("u256"),
            PrimTy::Usize => Some("usize"),
            PrimTy::I8 => Some("i8"),
            PrimTy::I16 => Some("i16"),
            PrimTy::I32 => Some("i32"),
            PrimTy::I64 => Some("i64"),
            PrimTy::I128 => Some("i128"),
            PrimTy::I256 => Some("i256"),
            PrimTy::Isize => Some("isize"),
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
        let ty = self
            .typed_body
            .expr_ty(self.db, expr)
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or_else(|| self.typed_body.expr_ty(self.db, expr));
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
        let ty = ty
            .as_capability(self.db)
            .map(|(_, inner)| inner)
            .unwrap_or(ty);
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
