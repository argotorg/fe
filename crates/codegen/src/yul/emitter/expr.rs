//! Expression and value lowering helpers shared across the Yul emitter.

use common::ingot::IngotKind;
use hir::analysis::ty::simplified_pattern::ConstructorKind;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
use hir::hir_def::{
    CallableDef,
    expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
};
use hir::projection::{IndexSource, Projection};
use hir::span::LazySpan;
use mir::{
    CallOrigin, LocalId, ValueId, ValueOrigin,
    ir::{FieldPtrOrigin, MirFunctionOrigin, Place, SyntheticValue},
    layout,
    repr::{PlaceState, ResolvedPlace, ResolvedPlaceSegment},
};
use num_bigint::BigUint;

use crate::yul::state::BlockState;

use super::{
    YulError,
    function::FunctionEmitter,
    util::{function_name, is_std_evm_ops, prefix_yul_name},
};

impl<'db> FunctionEmitter<'db> {
    fn hidden_runtime_param_local(&self, local: LocalId) -> bool {
        self.mir_func
            .body
            .param_locals
            .iter()
            .position(|&param_local| param_local == local)
            .is_some_and(|idx| !self.mir_func.runtime_abi.value_param_visible(idx))
            || self
                .mir_func
                .body
                .effect_param_locals
                .iter()
                .position(|&effect_local| effect_local == local)
                .is_some_and(|idx| !self.mir_func.runtime_abi.effect_param_visible(idx))
    }

    /// Attempts to lower a call to a core numeric intrinsic directly to inline Yul.
    ///
    /// This is an optimization that avoids generating separate Yul functions for
    /// primitive arithmetic intrinsics like `__add_u8`, `__sub_i32`, `__mul_u256`, etc.
    /// Instead, it recognizes the naming pattern `__<op>_<type>` and emits the
    /// corresponding Yul opcode inline with appropriate bit masking.
    ///
    /// For types smaller than 256 bits, proper masking is applied:
    /// - **Unsigned types**: Results are masked with `and(result, mask)` to truncate
    ///   overflow bits (e.g., `u8` uses mask `0xff`).
    /// - **Signed types**: Results use `signextend(byte, and(value, mask))` to
    ///   correctly propagate the sign bit.
    /// - **256-bit types** (`u256`, `i256`): No masking needed.
    ///
    /// # Parameters
    /// - `call`: The call origin containing the target function and arguments.
    /// - `state`: Current block state for lowering argument values.
    ///
    /// # Returns
    /// - `Ok(Some(yul))`: The intrinsic was recognized and lowered to inline Yul.
    /// - `Ok(None)`: The call is not a recognized core numeric intrinsic; the caller
    ///   should fall back to normal function call emission.
    /// - `Err(...)`: An error occurred during lowering.
    fn try_lower_core_numeric_intrinsic_call(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<Option<String>, YulError> {
        let Some(target) = call.hir_target.as_ref() else {
            return Ok(None);
        };
        let CallableDef::Func(func) = target.callable_def else {
            return Ok(None);
        };
        if func.body(self.db).is_some() {
            return Ok(None);
        }

        match target.callable_def.ingot(self.db).kind(self.db) {
            IngotKind::Core | IngotKind::Std => {}
            _ => return Ok(None),
        }

        let Some(name) = target.callable_def.name(self.db) else {
            return Ok(None);
        };
        let name = name.data(self.db).as_str();
        if name == "__bitcast" || !name.starts_with("__") {
            return Ok(None);
        }

        let Some((op, suffix)) = name[2..].rsplit_once('_') else {
            return Ok(None);
        };

        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        if !call.effect_args.is_empty() {
            return Err(YulError::Unsupported(format!(
                "core numeric intrinsic `{name}` unexpectedly has effect args"
            )));
        }

        #[derive(Clone, Copy, Debug)]
        enum IntPrim {
            Unsigned {
                mask: Option<&'static str>,
            },
            Signed {
                mask: Option<&'static str>,
                signextend_byte: Option<u8>,
            },
        }

        fn int_prim_from_suffix(suffix: &str) -> Option<IntPrim> {
            Some(match suffix {
                "u8" => IntPrim::Unsigned { mask: Some("0xff") },
                "u16" => IntPrim::Unsigned {
                    mask: Some("0xffff"),
                },
                "u32" => IntPrim::Unsigned {
                    mask: Some("0xffffffff"),
                },
                "u64" => IntPrim::Unsigned {
                    mask: Some("0xffffffffffffffff"),
                },
                "u128" => IntPrim::Unsigned {
                    mask: Some("0xffffffffffffffffffffffffffffffff"),
                },
                "u256" | "usize" => IntPrim::Unsigned { mask: None },
                "i8" => IntPrim::Signed {
                    mask: Some("0xff"),
                    signextend_byte: Some(0),
                },
                "i16" => IntPrim::Signed {
                    mask: Some("0xffff"),
                    signextend_byte: Some(1),
                },
                "i32" => IntPrim::Signed {
                    mask: Some("0xffffffff"),
                    signextend_byte: Some(3),
                },
                "i64" => IntPrim::Signed {
                    mask: Some("0xffffffffffffffff"),
                    signextend_byte: Some(7),
                },
                "i128" => IntPrim::Signed {
                    mask: Some("0xffffffffffffffffffffffffffffffff"),
                    signextend_byte: Some(15),
                },
                "i256" | "isize" => IntPrim::Signed {
                    mask: None,
                    signextend_byte: None,
                },
                _ => return None,
            })
        }

        fn trunc_bits(value: &str, prim: IntPrim) -> String {
            match prim {
                IntPrim::Unsigned { mask: Some(mask) }
                | IntPrim::Signed {
                    mask: Some(mask), ..
                } => {
                    format!("and({value}, {mask})")
                }
                IntPrim::Unsigned { mask: None } | IntPrim::Signed { mask: None, .. } => {
                    value.to_string()
                }
            }
        }

        fn canonical_unsigned(value: &str, mask: Option<&'static str>) -> String {
            match mask {
                Some(mask) => format!("and({value}, {mask})"),
                None => value.to_string(),
            }
        }

        fn canonical_signed(
            value: &str,
            mask: Option<&'static str>,
            signextend_byte: Option<u8>,
        ) -> String {
            match (mask, signextend_byte) {
                (Some(mask), Some(byte)) => format!("signextend({byte}, and({value}, {mask}))"),
                _ => value.to_string(),
            }
        }

        let lowered = if suffix == "bool" {
            let normalize_bool = |value: &str| format!("iszero(iszero({value}))");

            match (op, lowered_args.as_slice()) {
                ("not", [arg]) => Some(format!("iszero({})", normalize_bool(arg))),
                ("bitand", [lhs, rhs]) => Some(format!(
                    "and({}, {})",
                    normalize_bool(lhs),
                    normalize_bool(rhs)
                )),
                ("bitor", [lhs, rhs]) => Some(format!(
                    "or({}, {})",
                    normalize_bool(lhs),
                    normalize_bool(rhs)
                )),
                ("bitxor", [lhs, rhs]) => Some(format!(
                    "xor({}, {})",
                    normalize_bool(lhs),
                    normalize_bool(rhs)
                )),
                ("eq", [lhs, rhs]) => Some(format!(
                    "eq({}, {})",
                    normalize_bool(lhs),
                    normalize_bool(rhs)
                )),
                ("ne", [lhs, rhs]) => Some(format!(
                    "iszero(eq({}, {}))",
                    normalize_bool(lhs),
                    normalize_bool(rhs)
                )),
                _ => None,
            }
        } else if let Some(int_prim) = int_prim_from_suffix(suffix) {
            let (mask, signextend_byte, signed) = match int_prim {
                IntPrim::Unsigned { mask } => (mask, None, false),
                IntPrim::Signed {
                    mask,
                    signextend_byte,
                } => (mask, signextend_byte, true),
            };

            let arg_unsigned = |value: &str| canonical_unsigned(value, mask);
            let arg_signed = |value: &str| canonical_signed(value, mask, signextend_byte);
            let result_unsigned = |value: String| canonical_unsigned(&value, mask);
            let result_signed = |value: String| canonical_signed(&value, mask, signextend_byte);

            match (op, lowered_args.as_slice()) {
                ("add", [lhs, rhs]) => Some(if signed {
                    result_signed(format!("add({}, {})", arg_signed(lhs), arg_signed(rhs)))
                } else {
                    result_unsigned(format!("add({}, {})", arg_unsigned(lhs), arg_unsigned(rhs)))
                }),
                ("sub", [lhs, rhs]) => Some(if signed {
                    result_signed(format!("sub({}, {})", arg_signed(lhs), arg_signed(rhs)))
                } else {
                    result_unsigned(format!("sub({}, {})", arg_unsigned(lhs), arg_unsigned(rhs)))
                }),
                ("mul", [lhs, rhs]) => Some(if signed {
                    result_signed(format!("mul({}, {})", arg_signed(lhs), arg_signed(rhs)))
                } else {
                    result_unsigned(format!("mul({}, {})", arg_unsigned(lhs), arg_unsigned(rhs)))
                }),
                ("div", [lhs, rhs]) => Some(if signed {
                    result_signed(format!("sdiv({}, {})", arg_signed(lhs), arg_signed(rhs)))
                } else {
                    result_unsigned(format!("div({}, {})", arg_unsigned(lhs), arg_unsigned(rhs)))
                }),
                ("rem", [lhs, rhs]) => Some(if signed {
                    result_signed(format!("smod({}, {})", arg_signed(lhs), arg_signed(rhs)))
                } else {
                    result_unsigned(format!("mod({}, {})", arg_unsigned(lhs), arg_unsigned(rhs)))
                }),
                ("pow", [lhs, rhs]) => {
                    let base_bits = trunc_bits(lhs, int_prim);
                    let exp_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("exp({base_bits}, {exp_bits})"))
                    } else {
                        result_unsigned(format!("exp({base_bits}, {exp_bits})"))
                    })
                }
                ("shl", [lhs, rhs]) => {
                    let value_bits = trunc_bits(lhs, int_prim);
                    let shift_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("shl({shift_bits}, {value_bits})"))
                    } else {
                        result_unsigned(format!("shl({shift_bits}, {value_bits})"))
                    })
                }
                ("shr", [lhs, rhs]) => {
                    let shift_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("sar({shift_bits}, {})", arg_signed(lhs)))
                    } else {
                        result_unsigned(format!("shr({shift_bits}, {})", arg_unsigned(lhs)))
                    })
                }
                ("bitand", [lhs, rhs]) => {
                    let lhs_bits = trunc_bits(lhs, int_prim);
                    let rhs_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("and({lhs_bits}, {rhs_bits})"))
                    } else {
                        result_unsigned(format!("and({lhs_bits}, {rhs_bits})"))
                    })
                }
                ("bitor", [lhs, rhs]) => {
                    let lhs_bits = trunc_bits(lhs, int_prim);
                    let rhs_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("or({lhs_bits}, {rhs_bits})"))
                    } else {
                        result_unsigned(format!("or({lhs_bits}, {rhs_bits})"))
                    })
                }
                ("bitxor", [lhs, rhs]) => {
                    let lhs_bits = trunc_bits(lhs, int_prim);
                    let rhs_bits = trunc_bits(rhs, int_prim);
                    Some(if signed {
                        result_signed(format!("xor({lhs_bits}, {rhs_bits})"))
                    } else {
                        result_unsigned(format!("xor({lhs_bits}, {rhs_bits})"))
                    })
                }
                ("bitnot", [arg]) => {
                    let arg_bits = trunc_bits(arg, int_prim);
                    Some(if signed {
                        result_signed(format!("not({arg_bits})"))
                    } else {
                        result_unsigned(format!("not({arg_bits})"))
                    })
                }
                ("neg", [arg]) => Some(result_signed(format!("sub(0, {})", arg_signed(arg)))),
                ("eq", [lhs, rhs]) => {
                    let lhs_bits = trunc_bits(lhs, int_prim);
                    let rhs_bits = trunc_bits(rhs, int_prim);
                    Some(format!("eq({lhs_bits}, {rhs_bits})"))
                }
                ("ne", [lhs, rhs]) => {
                    let lhs_bits = trunc_bits(lhs, int_prim);
                    let rhs_bits = trunc_bits(rhs, int_prim);
                    Some(format!("iszero(eq({lhs_bits}, {rhs_bits}))"))
                }
                ("lt", [lhs, rhs]) => Some(if signed {
                    format!("slt({}, {})", arg_signed(lhs), arg_signed(rhs))
                } else {
                    format!("lt({}, {})", arg_unsigned(lhs), arg_unsigned(rhs))
                }),
                ("le", [lhs, rhs]) => Some(if signed {
                    format!("iszero(sgt({}, {}))", arg_signed(lhs), arg_signed(rhs))
                } else {
                    format!("iszero(gt({}, {}))", arg_unsigned(lhs), arg_unsigned(rhs))
                }),
                ("gt", [lhs, rhs]) => Some(if signed {
                    format!("sgt({}, {})", arg_signed(lhs), arg_signed(rhs))
                } else {
                    format!("gt({}, {})", arg_unsigned(lhs), arg_unsigned(rhs))
                }),
                ("ge", [lhs, rhs]) => Some(if signed {
                    format!("iszero(slt({}, {}))", arg_signed(lhs), arg_signed(rhs))
                } else {
                    format!("iszero(lt({}, {}))", arg_unsigned(lhs), arg_unsigned(rhs))
                }),
                _ => None,
            }
        } else {
            None
        };

        Ok(lowered)
    }
    pub(super) fn core_lib(&self) -> mir::CoreLib<'db> {
        let scope = match self.mir_func.origin {
            MirFunctionOrigin::Hir(func) => func.scope(),
            MirFunctionOrigin::Synthetic(synth) => synth.contract().scope(),
        };
        mir::CoreLib::new(self.db, scope)
    }

    fn format_hir_expr_context(&self, expr: hir::hir_def::ExprId) -> String {
        let Some(body) = (match self.mir_func.origin {
            MirFunctionOrigin::Hir(func) => func.body(self.db),
            MirFunctionOrigin::Synthetic(_) => None,
        }) else {
            return format!(
                "func={} expr={expr:?} (missing HIR body)",
                self.mir_func.symbol_name
            );
        };

        let span = expr.span(body).resolve(self.db);
        let span_context = if let Some(span) = span {
            let path = span
                .file
                .path(self.db)
                .as_ref()
                .map(|p| p.to_string())
                .unwrap_or_else(|| "<unknown file>".into());
            let start: usize = u32::from(span.range.start()) as usize;
            let text = span.file.text(self.db);
            let (mut line, mut col) = (1usize, 1usize);
            for byte in text.as_bytes().iter().take(start) {
                if *byte == b'\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }
            format!("{path}:{line}:{col}")
        } else {
            "<no span>".into()
        };

        let expr_data = match expr.data(self.db, body) {
            hir::hir_def::Partial::Present(expr_data) => match expr_data {
                hir::hir_def::Expr::Path(path) => path
                    .to_opt()
                    .map(|path| format!("Path({})", path.pretty_print(self.db)))
                    .unwrap_or_else(|| "Path(<absent>)".into()),
                hir::hir_def::Expr::Call(callee, args) => {
                    let callee_data = match callee.data(self.db, body) {
                        hir::hir_def::Partial::Present(hir::hir_def::Expr::Path(path)) => path
                            .to_opt()
                            .map(|path| format!("Path({})", path.pretty_print(self.db)))
                            .unwrap_or_else(|| "Path(<absent>)".into()),
                        hir::hir_def::Partial::Present(other) => format!("{other:?}"),
                        hir::hir_def::Partial::Absent => "<absent>".into(),
                    };
                    format!("Call({callee:?} {callee_data}, {args:?})")
                }
                hir::hir_def::Expr::MethodCall(receiver, method, _, args) => {
                    let method_name = method
                        .to_opt()
                        .map(|id| id.data(self.db).to_string())
                        .unwrap_or_else(|| "<absent>".into());
                    format!("MethodCall({receiver:?}, {method_name}, {args:?})")
                }
                other => format!("{other:?}"),
            },
            hir::hir_def::Partial::Absent => "<absent>".into(),
        };

        format!(
            "func={} expr={expr:?} at {}: {}",
            self.mir_func.symbol_name, span_context, expr_data
        )
    }

    /// Lowers a MIR `ValueId` into a Yul expression string.
    ///
    /// * `value_id` - Identifier selecting the MIR value.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns the Yul expression referencing the value or an error if unsupported.
    pub(super) fn lower_value(
        &self,
        value_id: ValueId,
        state: &BlockState,
    ) -> Result<String, YulError> {
        // Check if this value was already bound to a temp in the current scope
        if let Some(temp) = state.value_temp(value_id.index()) {
            return Ok(temp.clone());
        }
        let value = self.mir_func.body.value(value_id);
        match &value.origin {
            ValueOrigin::Expr(expr) => unreachable!(
                "unlowered HIR expression reached codegen (MIR lowering should have failed earlier): {}",
                self.format_hir_expr_context(*expr)
            ),
            ValueOrigin::ControlFlowResult { expr } => unreachable!(
                "control-flow result value reached codegen without binding (MIR lowering should have inserted/used a temp): {}",
                self.format_hir_expr_context(*expr)
            ),
            ValueOrigin::Unit => Ok("0".into()),
            ValueOrigin::Unary { op, inner } => {
                let value = self.lower_value(*inner, state)?;
                match op {
                    UnOp::Minus => Ok(format!("sub(0, {value})")),
                    UnOp::Not => Ok(format!("iszero({value})")),
                    UnOp::Plus => Ok(value),
                    UnOp::BitNot => Ok(format!("not({value})")),
                    UnOp::Mut => todo!(),
                    UnOp::Ref => todo!(),
                }
            }
            ValueOrigin::Binary { op, lhs, rhs } => {
                let left = self.lower_value(*lhs, state)?;
                let right = self.lower_value(*rhs, state)?;
                match op {
                    BinOp::Arith(op) => match op {
                        ArithBinOp::Add => Ok(format!("add({left}, {right})")),
                        ArithBinOp::Sub => Ok(format!("sub({left}, {right})")),
                        ArithBinOp::Mul => Ok(format!("mul({left}, {right})")),
                        ArithBinOp::Div => Ok(format!("div({left}, {right})")),
                        ArithBinOp::Rem => Ok(format!("mod({left}, {right})")),
                        ArithBinOp::Pow => Ok(format!("exp({left}, {right})")),
                        ArithBinOp::LShift => Ok(format!("shl({right}, {left})")),
                        ArithBinOp::RShift => Ok(format!("shr({right}, {left})")),
                        ArithBinOp::BitAnd => Ok(format!("and({left}, {right})")),
                        ArithBinOp::BitOr => Ok(format!("or({left}, {right})")),
                        ArithBinOp::BitXor => Ok(format!("xor({left}, {right})")),
                        // Range should be lowered to Range type construction before codegen
                        ArithBinOp::Range => {
                            todo!(
                                "Range operator should be handled during type checking/MIR lowering"
                            )
                        }
                    },
                    BinOp::Comp(op) => {
                        let expr = match op {
                            CompBinOp::Eq => format!("eq({left}, {right})"),
                            CompBinOp::NotEq => format!("iszero(eq({left}, {right}))"),
                            CompBinOp::Lt => format!("lt({left}, {right})"),
                            CompBinOp::LtEq => format!("iszero(gt({left}, {right}))"),
                            CompBinOp::Gt => format!("gt({left}, {right})"),
                            CompBinOp::GtEq => format!("iszero(lt({left}, {right}))"),
                        };
                        Ok(expr)
                    }
                    BinOp::Logical(op) => {
                        let func = match op {
                            LogicalBinOp::And => "and",
                            LogicalBinOp::Or => "or",
                        };
                        Ok(format!("{func}({left}, {right})"))
                    }
                    BinOp::Index => Err(YulError::Unsupported(
                        "index expressions should be lowered to places before codegen".into(),
                    )),
                }
            }
            ValueOrigin::Local(local) => {
                if let Some(name) = state.resolve_local(*local) {
                    return Ok(name);
                }
                if self.hidden_runtime_param_local(*local) {
                    return Ok("0".into());
                }

                let local_data = self.mir_func.body.local(*local);
                let is_param = self.mir_func.body.param_locals.contains(local);
                let is_effect = self.mir_func.body.effect_param_locals.contains(local);

                Err(YulError::Unsupported(format!(
                    "unbound MIR local reached codegen (func={}, local=l{} `{}`, ty={}, param={is_param}, effect={is_effect})",
                    self.mir_func.symbol_name,
                    local.index(),
                    local_data.name,
                    local_data.ty.pretty_print(self.db),
                )))
            }
            ValueOrigin::PlaceRoot(local) => {
                if let Some(name) = state.resolve_local(*local) {
                    return Ok(name);
                }
                if self.hidden_runtime_param_local(*local) {
                    return Ok("0".into());
                }
                Err(YulError::Unsupported(format!(
                    "unbound MIR place-root local reached codegen (func={}, local=l{} `{}`, ty={})",
                    self.mir_func.symbol_name,
                    local.index(),
                    self.mir_func.body.local(*local).name,
                    self.mir_func.body.local(*local).ty.pretty_print(self.db),
                )))
            }
            ValueOrigin::FuncItem(_) => {
                debug_assert!(
                    layout::is_zero_sized_ty_in(self.db, &self.layout, value.ty),
                    "function item values should be zero-sized (ty={})",
                    value.ty.pretty_print(self.db)
                );
                Ok("0".into())
            }
            ValueOrigin::Synthetic(synth) => self.lower_synthetic_value(synth, value.ty),
            ValueOrigin::FieldPtr(field_ptr) => self.lower_field_ptr(field_ptr, state),
            ValueOrigin::PlaceRef(place) => {
                if !value.repr.is_ref() {
                    if self.place_yields_location_value(
                        place,
                        value.ty,
                        self.mir_func.body.value_pointer_info(value_id),
                    )? {
                        return self.lower_place_address(place, state);
                    }
                    let loaded_ty = if value.repr.address_space().is_none() {
                        value
                            .ty
                            .as_capability(self.db)
                            .map(|(_, inner_ty)| inner_ty)
                            .unwrap_or(value.ty)
                    } else {
                        value.ty
                    };
                    return self.lower_place_load(place, loaded_ty, state);
                }
                self.lower_place_ref(place, state)
            }
            ValueOrigin::MoveOut { place } => {
                if value.repr.is_ref() {
                    self.lower_place_ref(place, state)
                } else {
                    if self.place_yields_location_value(
                        place,
                        value.ty,
                        self.mir_func.body.value_pointer_info(value_id),
                    )? {
                        return self.lower_place_address(place, state);
                    }
                    let loaded_ty = if value.repr.address_space().is_none() {
                        value
                            .ty
                            .as_capability(self.db)
                            .map(|(_, inner_ty)| inner_ty)
                            .unwrap_or(value.ty)
                    } else {
                        value.ty
                    };
                    self.lower_place_load(place, loaded_ty, state)
                }
            }
            ValueOrigin::ConstRegion(id) => {
                let label = self.const_region_labels.get(id).unwrap();
                Ok(format!("dataoffset(\"{label}\")"))
            }
            ValueOrigin::TransparentCast { value } => self.lower_value(*value, state),
        }
    }

    /// Lowers a MIR call into a Yul function invocation.
    ///
    /// * `call` - Call origin describing the callee and arguments.
    /// * `state` - Binding state used to lower argument expressions.
    ///
    /// Returns the Yul invocation string for the call.
    pub(super) fn lower_call_value(
        &self,
        call: &CallOrigin<'_>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if let Some(target) = call.hir_target.as_ref()
            && matches!(
                target.callable_def.ingot(self.db).kind(self.db),
                IngotKind::Core
            )
            && target.callable_def.name(self.db).is_some_and(|name| {
                matches!(name.data(self.db).as_str(), "__as_bytes" | "__keccak256")
            })
        {
            return Err(YulError::Unsupported(
                "core::keccak requires a compile-time constant value".into(),
            ));
        }

        if call
            .hir_target
            .as_ref()
            .and_then(|target| target.callable_def.name(self.db))
            .is_some_and(|name| name.data(self.db) == "contract_field_slot")
        {
            return Err(YulError::Unsupported(
                "`contract_field_slot` must be constant-folded before codegen".into(),
            ));
        }

        if let Some(intrinsic) = self.try_lower_core_numeric_intrinsic_call(call, state)? {
            return Ok(intrinsic);
        }

        let is_evm_op = match call.hir_target.as_ref() {
            Some(target) => {
                matches!(
                    target.callable_def,
                    CallableDef::Func(func) if is_std_evm_ops(self.db, func)
                )
            }
            None => false,
        };
        let callee = if let Some(name) = &call.resolved_name {
            name.clone()
        } else {
            let Some(target) = call.hir_target.as_ref() else {
                return Err(YulError::Unsupported(
                    "call is missing a resolved symbol name".into(),
                ));
            };
            match target.callable_def {
                CallableDef::Func(func) => function_name(self.db, func),
                CallableDef::VariantCtor(_) => {
                    return Err(YulError::Unsupported(
                        "callable without hir function definition is not supported yet".into(),
                    ));
                }
            }
        };
        let callee = if is_evm_op {
            callee
        } else {
            prefix_yul_name(&callee)
        };
        let mut lowered_args = Vec::with_capacity(call.args.len());
        for &arg in &call.args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        for &arg in &call.effect_args {
            lowered_args.push(self.lower_value(arg, state)?);
        }
        if lowered_args.is_empty() {
            Ok(format!("{callee}()"))
        } else {
            Ok(format!("{callee}({})", lowered_args.join(", ")))
        }
    }

    /// Lowers special MIR synthetic values such as constants into Yul expressions.
    ///
    /// * `value` - Synthetic value emitted during MIR construction.
    ///
    /// Returns the literal Yul expression for the synthetic value.
    fn lower_synthetic_value(
        &self,
        value: &SyntheticValue,
        ty: TyId<'db>,
    ) -> Result<String, YulError> {
        match value {
            SyntheticValue::Int(int) => {
                let ty = ty
                    .as_capability(self.db)
                    .map(|(_, inner)| inner)
                    .unwrap_or(ty);
                let TyData::TyBase(TyBase::Prim(prim)) = ty.base_ty(self.db).data(self.db) else {
                    return Ok(int.to_string());
                };
                let maybe_signed_subword = match prim {
                    PrimTy::I8 => Some((BigUint::from(0xffu16), BigUint::from(0x80u16), 0u8)),
                    PrimTy::I16 => Some((BigUint::from(0xffffu32), BigUint::from(0x8000u32), 1)),
                    PrimTy::I32 => Some((
                        BigUint::from(0xffff_ffffu64),
                        BigUint::from(0x8000_0000u64),
                        3,
                    )),
                    PrimTy::I64 => Some((BigUint::from(u64::MAX), BigUint::from(1u128 << 63), 7)),
                    PrimTy::I128 => {
                        Some((BigUint::from(u128::MAX), BigUint::from(1u128 << 127), 15))
                    }
                    _ => None,
                };
                if let Some((mask, sign_bit, byte)) = maybe_signed_subword {
                    let masked = int & mask;
                    if (&masked & sign_bit) != BigUint::from(0u8) {
                        return Ok(format!("signextend({byte}, {})", masked));
                    }
                    return Ok(masked.to_string());
                }
                Ok(int.to_string())
            }
            SyntheticValue::Bool(flag) => Ok(if *flag { "1" } else { "0" }.into()),
            SyntheticValue::Bytes(bytes) => Ok(format!("0x{}", hex::encode(bytes))),
        }
    }

    /// Lowers a FieldPtr (pointer arithmetic for nested struct access) into a Yul add expression.
    ///
    /// * `field_ptr` - The FieldPtrOrigin containing base pointer and offset.
    /// * `state` - Current bindings for previously-evaluated expressions.
    ///
    /// Returns a Yul expression representing `base + offset`.
    fn lower_field_ptr(
        &self,
        field_ptr: &FieldPtrOrigin,
        state: &BlockState,
    ) -> Result<String, YulError> {
        let base = self.lower_value(field_ptr.base, state)?;
        if field_ptr.offset_bytes == 0 {
            Ok(base)
        } else {
            let offset = match field_ptr.addr_space {
                mir::ir::AddressSpaceKind::Memory
                | mir::ir::AddressSpaceKind::Calldata
                | mir::ir::AddressSpaceKind::Code => field_ptr.offset_bytes,
                mir::ir::AddressSpaceKind::Storage
                | mir::ir::AddressSpaceKind::TransientStorage => field_ptr.offset_bytes / 32,
            };
            Ok(format!("add({}, {})", base, offset))
        }
    }

    /// Lowers a PlaceLoad (load value from a place with projection path).
    ///
    /// Walks the projection path to compute the byte offset from the base,
    /// then emits a load instruction based on the address space, applying
    /// the appropriate type conversion (masking, sign extension, etc.).
    pub(super) fn lower_place_load(
        &self,
        place: &Place<'db>,
        loaded_ty: TyId<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        if layout::ty_size_bytes_in(self.db, &self.layout, loaded_ty).is_some_and(|size| size == 0)
        {
            return Ok("0".into());
        }

        let packed = self.is_packed_scalar_array_access(place, loaded_ty)?;
        let (addr, place_state) = self.lower_place_terminal(place, state)?;
        let raw_load = match place_state
            .location_address_space()
            .ok_or_else(|| YulError::Unsupported(format!("place is not a location: {place:?}")))?
        {
            mir::ir::AddressSpaceKind::Memory => {
                if packed {
                    format!("byte(0, mload({addr}))")
                } else {
                    format!("mload({addr})")
                }
            }
            mir::ir::AddressSpaceKind::Calldata => format!("calldataload({addr})"),
            mir::ir::AddressSpaceKind::Storage => format!("sload({addr})"),
            mir::ir::AddressSpaceKind::TransientStorage => format!("tload({addr})"),
            mir::ir::AddressSpaceKind::Code => {
                return Err(YulError::Unsupported(
                    "cannot lower code space load into a pure expression. intercept in emit_load_inst".into(),
                ));
            }
        };

        // Apply type-specific conversion (std::evm::word::WordRepr::from_word equivalent)
        Ok(self.apply_from_word_conversion(&raw_load, loaded_ty))
    }

    pub(super) fn is_packed_scalar_array_access(
        &self,
        place: &Place<'db>,
        scalar_ty: TyId<'db>,
    ) -> Result<bool, YulError> {
        let space = self.mir_func.body.place_address_space(place);
        if !matches!(
            space,
            mir::ir::AddressSpaceKind::Memory | mir::ir::AddressSpaceKind::Code
        ) {
            return Ok(false);
        }

        let scalar_ty = mir::repr::word_conversion_leaf_ty(self.db, scalar_ty);
        if !layout::is_packed_memory_array_elem_ty(self.db, scalar_ty) {
            return Ok(false);
        }

        let base_value = self.mir_func.body.value(place.base);
        let mut current_ty = base_value.ty;
        let Some(last_idx) = place.projection.len().checked_sub(1) else {
            return Ok(false);
        };

        for (idx, proj) in place.projection.iter().enumerate() {
            match proj {
                Projection::Field(field_idx) => {
                    let field_types = current_ty.field_types(self.db);
                    current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                        YulError::Unsupported(format!(
                            "packed check: field {} out of bounds (have {} fields)",
                            field_idx,
                            field_types.len()
                        ))
                    })?;
                }
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                    let field_types = ctor.field_types(self.db);
                    current_ty = *field_types.get(*field_idx).ok_or_else(|| {
                        YulError::Unsupported(format!(
                            "packed check: variant field {} out of bounds (have {} fields)",
                            field_idx,
                            field_types.len()
                        ))
                    })?;
                }
                Projection::Discriminant => {
                    current_ty = TyId::new(self.db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
                }
                Projection::Index(_) => {
                    let elem_ty = layout::array_elem_ty(self.db, current_ty).ok_or_else(|| {
                        YulError::Unsupported(
                            "packed check: array index on non-array type".to_string(),
                        )
                    })?;
                    if idx == last_idx && layout::is_packed_memory_array_elem_ty(self.db, elem_ty) {
                        return Ok(true);
                    }
                    current_ty = elem_ty;
                }
                Projection::Deref => return Ok(false),
            }
        }

        Ok(false)
    }

    /// Applies the `WordRepr::from_word` conversion for a given type.
    ///
    /// This mirrors the stdlib word-conversion semantics defined in:
    /// - `ingots/std/src/evm/word.fe` (`WordRepr` trait)
    ///
    /// Conversion rules:
    /// - bool: word != 0
    /// - u8/u16/u32/u64/u128: mask to appropriate width
    /// - u256: identity
    /// - i8/i16/i32/i64/i128/i256: sign extension
    ///
    /// NOTE: This is a single source of truth for codegen. If the stdlib word
    /// conversion semantics change, this function must be updated to match.
    pub(super) fn apply_from_word_conversion(&self, raw_load: &str, ty: TyId<'db>) -> String {
        let ty = mir::repr::word_conversion_leaf_ty(self.db, ty);
        let base_ty = ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(self.db) {
            match prim {
                PrimTy::Bool => {
                    // bool: iszero(eq(word, 0)) which is equivalent to word != 0
                    format!("iszero(eq({raw_load}, 0))")
                }
                PrimTy::U8 => format!("and({raw_load}, 0xff)"),
                PrimTy::U16 => format!("and({raw_load}, 0xffff)"),
                PrimTy::U32 => format!("and({raw_load}, 0xffffffff)"),
                PrimTy::U64 => format!("and({raw_load}, 0xffffffffffffffff)"),
                PrimTy::U128 => {
                    format!("and({raw_load}, 0xffffffffffffffffffffffffffffffff)")
                }
                PrimTy::U256 | PrimTy::Usize => {
                    // No conversion needed for full-width unsigned
                    raw_load.to_string()
                }
                PrimTy::I8 => {
                    // Sign extension for i8
                    format!("signextend(0, and({raw_load}, 0xff))")
                }
                PrimTy::I16 => {
                    format!("signextend(1, and({raw_load}, 0xffff))")
                }
                PrimTy::I32 => {
                    format!("signextend(3, and({raw_load}, 0xffffffff))")
                }
                PrimTy::I64 => {
                    format!("signextend(7, and({raw_load}, 0xffffffffffffffff))")
                }
                PrimTy::I128 => {
                    format!("signextend(15, and({raw_load}, 0xffffffffffffffffffffffffffffffff))")
                }
                PrimTy::I256 | PrimTy::Isize => {
                    // Full-width signed doesn't need masking, sign is already there
                    raw_load.to_string()
                }
                // Aggregate/pointer-like types - no conversion
                PrimTy::String
                | PrimTy::Array
                | PrimTy::Tuple(_)
                | PrimTy::Ptr
                | PrimTy::View
                | PrimTy::BorrowMut
                | PrimTy::BorrowRef => raw_load.to_string(),
            }
        } else {
            // Non-primitive types (aggregates, etc.) - no conversion
            raw_load.to_string()
        }
    }

    /// Applies the `WordRepr::to_word` conversion for a given type.
    pub(super) fn apply_to_word_conversion(&self, raw_value: &str, ty: TyId<'db>) -> String {
        let ty = mir::repr::word_conversion_leaf_ty(self.db, ty);
        let base_ty = ty.base_ty(self.db);
        if let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(self.db) {
            match prim {
                PrimTy::Bool => format!("iszero(iszero({raw_value}))"),
                PrimTy::U8 => format!("and({raw_value}, 0xff)"),
                PrimTy::U16 => format!("and({raw_value}, 0xffff)"),
                PrimTy::U32 => format!("and({raw_value}, 0xffffffff)"),
                PrimTy::U64 => format!("and({raw_value}, 0xffffffffffffffff)"),
                PrimTy::U128 => {
                    format!("and({raw_value}, 0xffffffffffffffffffffffffffffffff)")
                }
                PrimTy::U256 | PrimTy::Usize => raw_value.to_string(),
                PrimTy::I8
                | PrimTy::I16
                | PrimTy::I32
                | PrimTy::I64
                | PrimTy::I128
                | PrimTy::I256
                | PrimTy::Isize => raw_value.to_string(),
                PrimTy::String
                | PrimTy::Array
                | PrimTy::Tuple(_)
                | PrimTy::Ptr
                | PrimTy::View
                | PrimTy::BorrowMut
                | PrimTy::BorrowRef => raw_value.to_string(),
            }
        } else {
            raw_value.to_string()
        }
    }

    /// Lowers a PlaceRef (reference to a place with projection path).
    ///
    /// Walks the projection path to compute the byte offset from the base,
    /// returning the pointer without loading.
    pub(super) fn lower_place_ref(
        &self,
        place: &Place<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        self.lower_place_address(place, state)
    }

    pub(super) fn lower_place_space(
        &self,
        place: &Place<'db>,
    ) -> Result<mir::ir::AddressSpaceKind, YulError> {
        let resolved = self.resolve_place(place)?;
        resolved
            .final_state()
            .location_address_space()
            .ok_or_else(|| YulError::Unsupported(format!("place is not a location: {place:?}")))
    }

    fn resolve_place(&self, place: &Place<'db>) -> Result<ResolvedPlace<'db>, YulError> {
        let core = self.core_lib();
        mir::repr::resolve_place(
            self.db,
            &core,
            &self.mir_func.body.values,
            &self.mir_func.body.locals,
            place,
        )
        .ok_or_else(|| {
            let base = self.mir_func.body.value(place.base);
            YulError::Unsupported(format!(
                "failed to resolve MIR place {place:?} (base ty={}, repr={:?}, origin={:?}, pointer_info={:?})",
                base.ty.pretty_print(self.db),
                base.repr,
                base.origin,
                base.pointer_info,
            ))
        })
    }

    fn place_yields_location_value(
        &self,
        place: &Place<'db>,
        value_ty: TyId<'db>,
        pointer_info: Option<mir::ir::PointerInfo<'db>>,
    ) -> Result<bool, YulError> {
        mir::repr::place_yields_location_value(
            self.db,
            &self.core_lib(),
            &self.mir_func.body.values,
            &self.mir_func.body.locals,
            place,
            value_ty,
            pointer_info,
        )
        .ok_or_else(|| YulError::Unsupported(format!("failed to resolve MIR place {place:?}")))
    }

    fn lower_place_segment_terminal(
        &self,
        segment_base_expr: String,
        segment: &ResolvedPlaceSegment<'db>,
        state: &BlockState,
    ) -> Result<(String, PlaceState<'db>), YulError> {
        let address_space = segment
            .base
            .location_address_space()
            .ok_or_else(|| YulError::Unsupported("place segment is not a location".to_string()))?;
        if segment.projections.is_empty() {
            return Ok((segment_base_expr, segment.terminal_state()));
        }

        let mut base_expr = segment_base_expr;
        let mut total_offset: usize = 0;
        let is_slot_addressed = matches!(
            address_space,
            mir::ir::AddressSpaceKind::Storage | mir::ir::AddressSpaceKind::TransientStorage
        );

        for step in &segment.projections {
            match &step.projection {
                Projection::Field(field_idx) => {
                    if mir::repr::transparent_field0_inner_ty(self.db, step.owner.ty, *field_idx)
                        == Some(step.result.ty)
                    {
                        continue;
                    }
                    total_offset += if is_slot_addressed {
                        layout::field_offset_slots(self.db, step.owner.ty, *field_idx)
                    } else {
                        layout::field_offset_memory_in(
                            self.db,
                            &self.layout,
                            step.owner.ty,
                            *field_idx,
                        )
                    };
                }
                Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                } => {
                    if is_slot_addressed {
                        total_offset += 1;
                        total_offset += layout::variant_field_offset_slots(
                            self.db, *enum_ty, *variant, *field_idx,
                        );
                    } else {
                        total_offset += self.layout.discriminant_size_bytes;
                        total_offset += layout::variant_field_offset_memory_in(
                            self.db,
                            &self.layout,
                            *enum_ty,
                            *variant,
                            *field_idx,
                        );
                    }
                }
                Projection::Discriminant => {}
                Projection::Index(idx_source) => {
                    let stride = if is_slot_addressed {
                        layout::array_elem_stride_slots(self.db, step.owner.ty)
                    } else {
                        layout::array_elem_stride_memory_in(self.db, &self.layout, step.owner.ty)
                    }
                    .ok_or_else(|| {
                        YulError::Unsupported(
                            "place projection: array index access on non-array type".to_string(),
                        )
                    })?;

                    match idx_source {
                        IndexSource::Constant(idx) => {
                            total_offset += idx * stride;
                        }
                        IndexSource::Dynamic(value_id) => {
                            if total_offset != 0 {
                                base_expr = format!("add({base_expr}, {total_offset})");
                                total_offset = 0;
                            }
                            let idx_expr = self.lower_value(*value_id, state)?;
                            let offset_expr = if stride == 1 {
                                idx_expr
                            } else {
                                format!("mul({idx_expr}, {stride})")
                            };
                            base_expr = format!("add({base_expr}, {offset_expr})");
                        }
                    }
                }
                Projection::Deref => {
                    return Err(YulError::Unsupported(
                        "place projection: deref reached segment walker".to_string(),
                    ));
                }
            }
        }

        if total_offset != 0 {
            base_expr = format!("add({base_expr}, {total_offset})");
        }

        Ok((base_expr, segment.terminal_state()))
    }

    fn lower_place_terminal(
        &self,
        place: &Place<'db>,
        state: &BlockState,
    ) -> Result<(String, PlaceState<'db>), YulError> {
        let resolved = self.resolve_place(place)?;
        let root_expr = if let ValueOrigin::Local(local) =
            &self.mir_func.body.value(place.base).origin
            && let Some(spill) = self.mir_func.body.spill_slots.get(local)
        {
            state.resolve_local(*spill).ok_or_else(|| {
                let local_data = self.mir_func.body.local(*spill);
                YulError::Unsupported(format!(
                    "unbound MIR spill slot local reached codegen (func={}, local=l{} `{}`, ty={})",
                    self.mir_func.symbol_name,
                    spill.index(),
                    local_data.name,
                    local_data.ty.pretty_print(self.db),
                ))
            })?
        } else {
            self.lower_value(place.base, state)?
        };
        let mut current_terminal: Option<(String, PlaceState<'db>)> = None;

        for (idx, segment) in resolved.segments.iter().enumerate() {
            let segment_base_expr = match (idx == 0, segment.start_kind, current_terminal.take()) {
                (true, None, None)
                | (true, Some(mir::repr::DerefStepKind::ReuseLocation), None)
                | (true, Some(mir::repr::DerefStepKind::UseBaseValue), None) => root_expr.clone(),
                (true, Some(mir::repr::DerefStepKind::LoadLocationValue), None) => self
                    .apply_from_word_conversion(
                        &Self::yul_load(
                            segment.before.location_address_space().ok_or_else(|| {
                                YulError::Unsupported(
                                    "place terminal is not a location".to_string(),
                                )
                            })?,
                            &root_expr,
                        ),
                        segment.before.ty,
                    ),
                (false, Some(mir::repr::DerefStepKind::ReuseLocation), Some((addr, _))) => addr,
                (
                    false,
                    Some(mir::repr::DerefStepKind::LoadLocationValue),
                    Some((addr, terminal_state)),
                ) => self.apply_from_word_conversion(
                    &Self::yul_load(
                        terminal_state.location_address_space().ok_or_else(|| {
                            YulError::Unsupported("place terminal is not a location".to_string())
                        })?,
                        &addr,
                    ),
                    segment.before.ty,
                ),
                (false, None, Some(_))
                | (false, Some(mir::repr::DerefStepKind::UseBaseValue), Some(_))
                | (true, None, Some(_))
                | (true, Some(_), Some(_))
                | (false, _, None) => {
                    return Err(YulError::Unsupported(format!(
                        "invalid resolved place segment sequence: {place:?}"
                    )));
                }
            };
            current_terminal =
                Some(self.lower_place_segment_terminal(segment_base_expr, segment, state)?);
        }

        current_terminal.ok_or_else(|| {
            YulError::Unsupported(format!("resolved place produced no segments: {place:?}"))
        })
    }

    /// Computes the address for a place by walking the projection path.
    ///
    /// Returns a Yul expression representing the memory/storage address.
    /// For memory, computes byte offsets. For storage, computes slot offsets.
    pub(super) fn lower_place_address(
        &self,
        place: &Place<'db>,
        state: &BlockState,
    ) -> Result<String, YulError> {
        self.lower_place_terminal(place, state)
            .map(|(addr, _)| addr)
    }
}
