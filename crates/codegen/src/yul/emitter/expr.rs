use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use mir::{RefKind, RuntimeClass};

use crate::yul::{
    doc::YulDoc,
    errors::YulError,
    legalize::{
        YBuiltin, YExpr, YLocalId, YulAddressSpace, YulPlace, YulPlaceElem, YulPlaceRoot,
        YulValueClass, yul_space_for_deref_carrier,
    },
};

use super::function::{FunctionEmitter, RenderedValue};

const PANIC_SELECTOR_WORD: &str =
    "0x4e487b7100000000000000000000000000000000000000000000000000000000";
const PANIC_OVERFLOW: u64 = 0x11;
const PANIC_DIVISION_BY_ZERO: u64 = 0x12;

fn panic_revert_docs(code: u64) -> Vec<YulDoc> {
    vec![
        YulDoc::line(format!("mstore(0, {PANIC_SELECTOR_WORD})")),
        YulDoc::line(format!("mstore(4, 0x{code:x})")),
        YulDoc::line("revert(0, 36)"),
    ]
}

impl<'a, 'db> FunctionEmitter<'a, 'db> {
    pub(super) fn render_builtin_stmt(
        &mut self,
        builtin: &YBuiltin<'db>,
    ) -> Result<Vec<YulDoc>, YulError> {
        Ok(match builtin {
            YBuiltin::Mstore { addr, value } => vec![YulDoc::line(format!(
                "mstore({}, {})",
                self.scalar_word_expr(*addr)?,
                self.scalar_word_expr(*value)?
            ))],
            YBuiltin::Mstore8 { addr, value } => vec![YulDoc::line(format!(
                "mstore8({}, {})",
                self.scalar_word_expr(*addr)?,
                self.scalar_word_expr(*value)?
            ))],
            YBuiltin::Mcopy { dst, src, len } => vec![YulDoc::line(format!(
                "mcopy({}, {}, {})",
                self.scalar_word_expr(*dst)?,
                self.scalar_word_expr(*src)?,
                self.scalar_word_expr(*len)?
            ))],
            YBuiltin::Sstore { slot, value } => vec![YulDoc::line(format!(
                "sstore({}, {})",
                self.scalar_word_expr(*slot)?,
                self.scalar_word_expr(*value)?
            ))],
            YBuiltin::ReturnDataCopy { dst, offset, len } => vec![YulDoc::line(format!(
                "returndatacopy({}, {}, {})",
                self.scalar_word_expr(*dst)?,
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?
            ))],
            YBuiltin::CallDataCopy { dst, offset, len } => vec![YulDoc::line(format!(
                "calldatacopy({}, {}, {})",
                self.scalar_word_expr(*dst)?,
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?
            ))],
            YBuiltin::CodeCopy { dst, offset, len } => vec![YulDoc::line(format!(
                "datacopy({}, {}, {})",
                self.scalar_word_expr(*dst)?,
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?
            ))],
            YBuiltin::Log0 { offset, len } => vec![YulDoc::line(format!(
                "log0({}, {})",
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?
            ))],
            YBuiltin::Log1 {
                offset,
                len,
                topic0,
            } => vec![YulDoc::line(format!(
                "log1({}, {}, {})",
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
                self.scalar_word_expr(*topic0)?
            ))],
            YBuiltin::Log2 {
                offset,
                len,
                topic0,
                topic1,
            } => vec![YulDoc::line(format!(
                "log2({}, {}, {}, {})",
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
                self.scalar_word_expr(*topic0)?,
                self.scalar_word_expr(*topic1)?
            ))],
            YBuiltin::Log3 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
            } => vec![YulDoc::line(format!(
                "log3({}, {}, {}, {}, {})",
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
                self.scalar_word_expr(*topic0)?,
                self.scalar_word_expr(*topic1)?,
                self.scalar_word_expr(*topic2)?
            ))],
            YBuiltin::Log4 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
                topic3,
            } => vec![YulDoc::line(format!(
                "log4({}, {}, {}, {}, {}, {})",
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
                self.scalar_word_expr(*topic0)?,
                self.scalar_word_expr(*topic1)?,
                self.scalar_word_expr(*topic2)?,
                self.scalar_word_expr(*topic3)?
            ))],
            _ => {
                return Err(YulError::InvalidYulPackage(format!(
                    "expression builtin `{builtin:?}` used as a statement"
                )));
            }
        })
    }

    pub(super) fn render_expr(
        &mut self,
        expr: &YExpr<'db>,
        expected: Option<&YulValueClass<'db>>,
    ) -> Result<RenderedValue<'db>, YulError> {
        match expr {
            YExpr::Use(local) => self.local_value(*local),
            YExpr::ConstWord(value) => Ok(RenderedValue {
                setup: Vec::new(),
                value: self.const_scalar_expr(value),
                class: expected.cloned().unwrap_or_else(|| {
                    YulValueClass::Word(mir::ScalarClass {
                        repr: self.scalar_repr_for_const(value),
                        role: mir::ScalarRole::Plain,
                    })
                }),
            }),
            YExpr::Placeholder { class } => Ok(RenderedValue {
                setup: Vec::new(),
                value: Self::zero_for_class(class),
                class: class.clone(),
            }),
            YExpr::Builtin(builtin) => self.render_builtin_expr(builtin, expected),
            YExpr::Unary { op, value } => {
                let value = self.local_value(*value)?;
                let class = expected.cloned().unwrap_or_else(|| value.class.clone());
                self.render_unary_value(*op, value, class)
            }
            YExpr::Binary { op, lhs, rhs } => {
                let lhs = self.local_value(*lhs)?;
                let rhs = self.local_value(*rhs)?;
                let class = expected.cloned().unwrap_or_else(|| lhs.class.clone());
                self.render_binary_value(*op, lhs, rhs, class)
            }
            YExpr::Cast { value, to } => {
                let value = self.local_value(*value)?;
                let class = YulValueClass::Word(to.clone());
                Ok(RenderedValue {
                    setup: value.setup,
                    value: self.cast_word_expr(&value.value, to),
                    class,
                })
            }
            YExpr::ConstRef { region, layout } => Ok(RenderedValue {
                setup: Vec::new(),
                value: format!("dataoffset(\"{}\")", self.index.const_label(*region)?),
                class: YulValueClass::CodePtr { layout: *layout },
            }),
            YExpr::AllocObject { layout, .. } => {
                let ptr_class = YulValueClass::MemoryPtr { layout: *layout };
                let (setup, value) =
                    self.alloc_temp_memory(&self.class_size_bytes(&ptr_class)?.to_string());
                Ok(RenderedValue {
                    setup,
                    value,
                    class: ptr_class,
                })
            }
            YExpr::MaterializeToObject { src, layout } => {
                let src = self.local_value(*src)?;
                if matches!(src.class, YulValueClass::MemoryPtr { .. }) {
                    return Ok(RenderedValue {
                        setup: src.setup,
                        value: src.value,
                        class: YulValueClass::MemoryPtr { layout: *layout },
                    });
                }
                let temp = self.state.alloc_temp();
                let mut setup = src.setup.clone();
                let ptr_class = YulValueClass::MemoryPtr { layout: *layout };
                setup.extend(self.alloc_memory_slot(&temp, &ptr_class)?);
                setup.extend(self.copy_into_addr(
                    ptr_class.clone(),
                    YulAddressSpace::Memory,
                    temp.clone(),
                    src,
                )?);
                Ok(RenderedValue {
                    setup,
                    value: temp,
                    class: ptr_class,
                })
            }
            YExpr::MaterializePlaceToObject { place, layout } => {
                let (mut setup, addr, space) = self.address_of_place(place)?;
                let temp = self.state.alloc_temp();
                let ptr_class = YulValueClass::MemoryPtr { layout: *layout };
                let src_class = self.materialized_place_source_class(place, space, *layout)?;
                setup.extend(self.alloc_memory_slot(&temp, &ptr_class)?);
                setup.extend(self.copy_into_addr(
                    ptr_class.clone(),
                    YulAddressSpace::Memory,
                    temp.clone(),
                    RenderedValue {
                        setup: Vec::new(),
                        value: addr,
                        class: src_class,
                    },
                )?);
                Ok(RenderedValue {
                    setup,
                    value: temp,
                    class: ptr_class,
                })
            }
            YExpr::ProviderFromRaw { raw, class } => {
                let raw = self.local_value(*raw)?;
                Ok(RenderedValue {
                    setup: raw.setup,
                    value: raw.value,
                    class: class.clone(),
                })
            }
            YExpr::WordToRawAddr { value, class } => {
                let value = self.local_value(*value)?;
                Ok(RenderedValue {
                    setup: value.setup,
                    value: value.value,
                    class: class.clone(),
                })
            }
            YExpr::ProviderToRaw { value } => {
                let value = self.local_value(*value)?;
                Ok(RenderedValue {
                    setup: value.setup,
                    value: value.value,
                    class: YulValueClass::Word(mir::ScalarClass {
                        repr: mir::ScalarRepr::Int {
                            bits: 256,
                            signed: false,
                        },
                        role: mir::ScalarRole::Plain,
                    }),
                })
            }
            YExpr::AddrOf { place } => {
                let (setup, value, _) = self.address_of_place(place)?;
                Ok(RenderedValue {
                    setup,
                    value,
                    class: expected
                        .cloned()
                        .unwrap_or_else(|| place.result_class.clone()),
                })
            }
            YExpr::Load { place } => self.load_from_place(place),
            YExpr::Call { callee, args } => {
                let mut setup = Vec::new();
                let args = args
                    .iter()
                    .map(|arg| self.local_value(*arg))
                    .collect::<Result<Vec<_>, _>>()?;
                for arg in &args {
                    setup.extend(arg.setup.clone());
                }
                let rendered_args = args
                    .into_iter()
                    .map(|arg| arg.value)
                    .collect::<Vec<_>>()
                    .join(", ");
                let callee_plan = self.index.function(*callee)?;
                let class = expected
                    .cloned()
                    .or_else(|| callee_plan.ret.clone())
                    .ok_or_else(|| {
                        YulError::InvalidYulPackage(format!(
                            "value context requires a return value from `{}`",
                            callee_plan.symbol
                        ))
                    })?;
                Ok(RenderedValue {
                    setup,
                    value: format!(
                        "{}({rendered_args})",
                        super::util::prefix_yul_name(&callee_plan.symbol)
                    ),
                    class,
                })
            }
            YExpr::EnumMake {
                layout,
                variant,
                fields,
            } => {
                let temp = self.state.alloc_temp();
                let ptr_class = expected
                    .cloned()
                    .unwrap_or(YulValueClass::MemoryPtr { layout: *layout });
                let mut setup = self.alloc_memory_slot(&temp, &ptr_class)?;
                setup.extend(self.write_enum_variant(
                    &temp,
                    YulAddressSpace::Memory,
                    *layout,
                    *variant,
                    fields,
                )?);
                Ok(RenderedValue {
                    setup,
                    value: temp,
                    class: ptr_class,
                })
            }
            YExpr::EnumTagOfValue { value } => self.enum_tag_of_value(*value),
            YExpr::EnumIsVariant { value, variant } => {
                let tag = self.enum_tag_of_value(*value)?;
                let cmp = format!("eq({}, {})", tag.value, variant.index);
                let class = YulValueClass::Word(mir::ScalarClass {
                    repr: mir::ScalarRepr::Bool,
                    role: mir::ScalarRole::Plain,
                });
                Ok(RenderedValue {
                    setup: tag.setup,
                    value: cmp,
                    class,
                })
            }
            YExpr::EnumExtract {
                value,
                variant,
                field,
            } => {
                let base = self.local_value(*value)?;
                let layout = self.class_layout(&base.class)?;
                let ptr_class = base.class.clone();
                let mut setup = base.setup;
                let temp = self.state.alloc_temp();
                let offset = self.variant_field_offset_bytes(layout, *variant, *field);
                let addr = format!("add({}, {offset})", base.value);
                let field_class = expected
                    .cloned()
                    .unwrap_or_else(|| panic!("enum extract requires destination class"));
                match &field_class {
                    YulValueClass::Word(_) => {
                        setup.extend(self.read_scalar_from_addr(
                            &field_class,
                            Self::root_space_for_class(&ptr_class)?,
                            addr.clone(),
                            &temp,
                        )?);
                        Ok(RenderedValue {
                            setup,
                            value: temp,
                            class: field_class,
                        })
                    }
                    _ => Ok(RenderedValue {
                        setup,
                        value: addr,
                        class: field_class,
                    }),
                }
            }
            YExpr::EnumGetTag { root } => self.enum_tag_of_value(*root),
            YExpr::EnumAssertVariantRef { root, variant } => {
                let tag = self.enum_tag_of_value(*root)?;
                let mut setup = tag.setup;
                setup.push(YulDoc::block(
                    format!("if iszero(eq({}, {})) ", tag.value, variant.index),
                    vec![YulDoc::line("invalid()")],
                ));
                let root = self.local_value(*root)?;
                setup.extend(root.setup);
                Ok(RenderedValue {
                    setup,
                    value: root.value,
                    class: root.class,
                })
            }
        }
    }

    fn materialized_place_source_class(
        &self,
        place: &YulPlace<'db>,
        space: YulAddressSpace,
        layout: mir::LayoutId<'db>,
    ) -> Result<YulValueClass<'db>, YulError> {
        let RuntimeClass::AggregateValue {
            layout: source_layout,
        } = place.runtime_result_class
        else {
            return Err(YulError::InvalidYulPackage(
                "materialize-place-to-object source is not aggregate-backed".to_string(),
            ));
        };
        if source_layout != layout {
            return Err(YulError::InvalidYulPackage(format!(
                "materialize-place-to-object source layout `{source_layout:?}` does not match destination `{layout:?}`"
            )));
        }
        Ok(match space {
            YulAddressSpace::Memory => YulValueClass::MemoryPtr { layout },
            YulAddressSpace::Storage => YulValueClass::StoragePtr { layout },
            YulAddressSpace::Transient => YulValueClass::TransientPtr { layout },
            YulAddressSpace::Calldata => YulValueClass::CalldataPtr { layout },
            YulAddressSpace::Code => YulValueClass::CodePtr { layout },
        })
    }

    fn render_builtin_expr(
        &mut self,
        builtin: &YBuiltin<'db>,
        expected: Option<&YulValueClass<'db>>,
    ) -> Result<RenderedValue<'db>, YulError> {
        Ok(match builtin {
            YBuiltin::Mload { addr } => RenderedValue {
                setup: Vec::new(),
                value: format!("mload({})", self.scalar_word_expr(*addr)?),
                class: expected
                    .cloned()
                    .unwrap_or_else(|| self.local_class(*addr).expect("mload addr class")),
            },
            YBuiltin::Msize => RenderedValue {
                setup: Vec::new(),
                value: "msize()".to_string(),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::Sload { slot } => RenderedValue {
                setup: Vec::new(),
                value: format!("sload({})", self.scalar_word_expr(*slot)?),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::CallValue => RenderedValue {
                setup: Vec::new(),
                value: "callvalue()".to_string(),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::ReturnDataSize => RenderedValue {
                setup: Vec::new(),
                value: "returndatasize()".to_string(),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::CallDataSize => RenderedValue {
                setup: Vec::new(),
                value: "calldatasize()".to_string(),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::CallDataLoad { offset } => RenderedValue {
                setup: Vec::new(),
                value: format!("calldataload({})", self.scalar_word_expr(*offset)?),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::CodeSize => RenderedValue {
                setup: Vec::new(),
                value: "codesize()".to_string(),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::Keccak256 { offset, len } => RenderedValue {
                setup: Vec::new(),
                value: format!(
                    "keccak256({}, {})",
                    self.scalar_word_expr(*offset)?,
                    self.scalar_word_expr(*len)?
                ),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::AddMod { lhs, rhs, modulus } => RenderedValue {
                setup: Vec::new(),
                value: format!(
                    "addmod({}, {}, {})",
                    self.scalar_word_expr(*lhs)?,
                    self.scalar_word_expr(*rhs)?,
                    self.scalar_word_expr(*modulus)?
                ),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::MulMod { lhs, rhs, modulus } => RenderedValue {
                setup: Vec::new(),
                value: format!(
                    "mulmod({}, {}, {})",
                    self.scalar_word_expr(*lhs)?,
                    self.scalar_word_expr(*rhs)?,
                    self.scalar_word_expr(*modulus)?
                ),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::SignExtend { byte, value } => RenderedValue {
                setup: Vec::new(),
                value: format!(
                    "signextend({}, {})",
                    self.scalar_word_expr(*byte)?,
                    self.scalar_word_expr(*value)?
                ),
                class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
            },
            YBuiltin::IntrinsicArith {
                op,
                checked,
                lhs,
                rhs,
                class,
            } => self.render_intrinsic_arith(*op, *checked, *lhs, *rhs, class.clone())?,
            YBuiltin::Saturating {
                op,
                lhs,
                rhs,
                class,
            } => self.render_saturating_builtin(*op, *lhs, *rhs, class.clone())?,
            YBuiltin::Address => self.word_builtin("address()"),
            YBuiltin::Caller => self.word_builtin("caller()"),
            YBuiltin::Origin => self.word_builtin("origin()"),
            YBuiltin::GasPrice => self.word_builtin("gasprice()"),
            YBuiltin::CoinBase => self.word_builtin("coinbase()"),
            YBuiltin::Timestamp => self.word_builtin("timestamp()"),
            YBuiltin::Number => self.word_builtin("number()"),
            YBuiltin::PrevRandao => self.word_builtin("prevrandao()"),
            YBuiltin::GasLimit => self.word_builtin("gaslimit()"),
            YBuiltin::ChainId => self.word_builtin("chainid()"),
            YBuiltin::BaseFee => self.word_builtin("basefee()"),
            YBuiltin::SelfBalance => self.word_builtin("selfbalance()"),
            YBuiltin::BlockHash { block } => {
                self.word_builtin(&format!("blockhash({})", self.scalar_word_expr(*block)?))
            }
            YBuiltin::Gas => self.word_builtin("gas()"),
            YBuiltin::CurrentCodeRegionLen => {
                self.word_builtin(&format!("datasize(\"{}\")", self.section_label))
            }
            YBuiltin::CodeRegionOffset { region } => self.word_builtin(&format!(
                "dataoffset(\"{}\")",
                self.index.code_region_label(*region)?
            )),
            YBuiltin::CodeRegionLen { region } => self.word_builtin(&format!(
                "datasize(\"{}\")",
                self.index.code_region_label(*region)?
            )),
            YBuiltin::Malloc { size } => {
                let (setup, value) = self.alloc_temp_memory(&self.scalar_word_expr(*size)?);
                RenderedValue {
                    setup,
                    value,
                    class: expected.cloned().unwrap_or_else(|| self.word_u256_class()),
                }
            }
            YBuiltin::Call {
                gas,
                addr,
                value,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => self.word_builtin(&format!(
                "call({}, {}, {}, {}, {}, {}, {})",
                self.scalar_word_expr(*gas)?,
                self.scalar_word_expr(*addr)?,
                self.scalar_word_expr(*value)?,
                self.scalar_word_expr(*args_offset)?,
                self.scalar_word_expr(*args_len)?,
                self.scalar_word_expr(*ret_offset)?,
                self.scalar_word_expr(*ret_len)?,
            )),
            YBuiltin::StaticCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => self.word_builtin(&format!(
                "staticcall({}, {}, {}, {}, {}, {})",
                self.scalar_word_expr(*gas)?,
                self.scalar_word_expr(*addr)?,
                self.scalar_word_expr(*args_offset)?,
                self.scalar_word_expr(*args_len)?,
                self.scalar_word_expr(*ret_offset)?,
                self.scalar_word_expr(*ret_len)?,
            )),
            YBuiltin::DelegateCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => self.word_builtin(&format!(
                "delegatecall({}, {}, {}, {}, {}, {})",
                self.scalar_word_expr(*gas)?,
                self.scalar_word_expr(*addr)?,
                self.scalar_word_expr(*args_offset)?,
                self.scalar_word_expr(*args_len)?,
                self.scalar_word_expr(*ret_offset)?,
                self.scalar_word_expr(*ret_len)?,
            )),
            YBuiltin::Create { value, offset, len } => self.word_builtin(&format!(
                "create({}, {}, {})",
                self.scalar_word_expr(*value)?,
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
            )),
            YBuiltin::Create2 {
                value,
                offset,
                len,
                salt,
            } => self.word_builtin(&format!(
                "create2({}, {}, {}, {})",
                self.scalar_word_expr(*value)?,
                self.scalar_word_expr(*offset)?,
                self.scalar_word_expr(*len)?,
                self.scalar_word_expr(*salt)?,
            )),
            YBuiltin::CallDataSelector => self.word_builtin("shr(224, calldataload(0))"),
            YBuiltin::MakeContractFieldRef { slot, class, .. } => RenderedValue {
                setup: Vec::new(),
                value: slot.to_string(),
                class: class.clone(),
            },
            YBuiltin::Mstore { .. }
            | YBuiltin::Mstore8 { .. }
            | YBuiltin::Mcopy { .. }
            | YBuiltin::Sstore { .. }
            | YBuiltin::ReturnDataCopy { .. }
            | YBuiltin::CallDataCopy { .. }
            | YBuiltin::CodeCopy { .. }
            | YBuiltin::Log0 { .. }
            | YBuiltin::Log1 { .. }
            | YBuiltin::Log2 { .. }
            | YBuiltin::Log3 { .. }
            | YBuiltin::Log4 { .. } => {
                return Err(YulError::InvalidYulPackage(format!(
                    "statement-only builtin `{builtin:?}` used as an expression"
                )));
            }
        })
    }

    fn word_builtin(&self, value: &str) -> RenderedValue<'db> {
        RenderedValue {
            setup: Vec::new(),
            value: value.to_string(),
            class: self.word_u256_class(),
        }
    }

    fn load_from_place(&mut self, place: &YulPlace<'db>) -> Result<RenderedValue<'db>, YulError> {
        if let Some(local) = self.direct_word_slot_local(place) {
            return self.local_value(local);
        }
        let (mut setup, addr, space) = self.address_of_place(place)?;
        if matches!(place.result_class, YulValueClass::Word(_)) {
            let temp = self.state.alloc_temp();
            setup.extend(if place.packed_byte_access {
                self.read_packed_byte_scalar_from_addr(&place.result_class, space, addr, &temp)?
            } else {
                self.read_scalar_from_addr(&place.result_class, space, addr, &temp)?
            });
            return Ok(RenderedValue {
                setup,
                value: temp,
                class: place.result_class.clone(),
            });
        }
        match place.storage_kind {
            crate::yul::legalize::YulStorageKind::Cell
                if self.place_load_uses_place_addr(place) =>
            {
                Ok(RenderedValue {
                    setup,
                    value: addr,
                    class: place.result_class.clone(),
                })
            }
            crate::yul::legalize::YulStorageKind::Cell => {
                let temp = self.state.alloc_temp();
                setup.extend(self.read_transport_word_from_addr(space, addr, &temp)?);
                Ok(RenderedValue {
                    setup,
                    value: temp,
                    class: place.result_class.clone(),
                })
            }
            crate::yul::legalize::YulStorageKind::Bytes => Ok(RenderedValue {
                setup,
                value: addr,
                class: place.result_class.clone(),
            }),
        }
    }

    fn place_load_uses_place_addr(&self, place: &YulPlace<'db>) -> bool {
        place.path.is_empty()
            && matches!(
                &place.runtime_result_class,
                RuntimeClass::Ref {
                    kind: RefKind::Object | RefKind::Const,
                    pointee,
                    ..
                } if pointee.aggregate_layout().is_some()
            )
    }

    pub(super) fn address_of_place(
        &mut self,
        place: &YulPlace<'db>,
    ) -> Result<(Vec<YulDoc>, String, YulAddressSpace), YulError> {
        let (setup, mut addr, space, mut layout) = match &place.root {
            YulPlaceRoot::Slot(local) => {
                let setup = self.ensure_root_slot(*local)?;
                let root = self.root_slot_name(*local)?.to_string();
                let class = match &self.local(*local)?.root {
                    crate::yul::legalize::YulLocalRoot::MemorySlot { class } => class,
                    _ => unreachable!(),
                };
                (
                    setup,
                    root,
                    YulAddressSpace::Memory,
                    match class {
                        mir::RuntimeClass::AggregateValue { layout } => Some(*layout),
                        mir::RuntimeClass::Ref { pointee, .. } => pointee.aggregate_layout(),
                        mir::RuntimeClass::Scalar(_) | mir::RuntimeClass::RawAddr { .. } => None,
                    },
                )
            }
            YulPlaceRoot::Ptr {
                local,
                space,
                class,
            } => {
                let value = self.local_value(*local)?;
                (
                    value.setup,
                    value.value,
                    *space,
                    self.class_layout(class).ok(),
                )
            }
        };
        let mut setup = setup;
        let mut space = space;

        for elem in place.path.iter() {
            match elem {
                YulPlaceElem::Field { field, class } => {
                    let current_layout = layout.ok_or_else(|| {
                        YulError::Layout("field projection requires a layout".to_string())
                    })?;
                    let offset = self.field_offset_bytes(current_layout, *field);
                    addr = self.project_addr(space, addr, offset);
                    layout = self.class_layout(class).ok();
                }
                YulPlaceElem::Index { index, class } => {
                    let current_layout = layout.ok_or_else(|| {
                        YulError::Layout("index projection requires an array layout".to_string())
                    })?;
                    let stride = self.index_stride_bytes(current_layout);
                    let index_expr = match index {
                        hir::projection::IndexSource::Constant(idx) => idx.to_string(),
                        hir::projection::IndexSource::Dynamic(local) => {
                            self.scalar_word_expr(*local)?
                        }
                    };
                    addr =
                        self.project_addr_expr(space, addr, format!("mul({index_expr}, {stride})"));
                    layout = self.class_layout(class).ok();
                }
                YulPlaceElem::VariantField {
                    variant,
                    field,
                    class,
                } => {
                    let current_layout = layout.ok_or_else(|| {
                        YulError::Layout("variant projection requires an enum layout".to_string())
                    })?;
                    let offset = self.variant_field_offset_bytes(current_layout, *variant, *field);
                    addr = self.project_addr(space, addr, offset);
                    layout = self.class_layout(class).ok();
                }
                YulPlaceElem::Deref {
                    carrier_class,
                    class,
                } => {
                    let temp = self.state.alloc_temp();
                    setup.extend(self.read_transport_word_from_addr(space, addr, &temp)?);
                    addr = temp;
                    space = yul_space_for_deref_carrier(carrier_class).ok_or_else(|| {
                        YulError::InvalidYulPackage(format!(
                            "cannot follow non-transport runtime class `{carrier_class:?}`"
                        ))
                    })?;
                    layout = self.class_layout(class).ok();
                }
            }
        }

        Ok((setup, addr, space))
    }

    fn project_addr(&self, space: YulAddressSpace, base: String, offset: usize) -> String {
        if offset == 0 {
            base
        } else {
            self.project_addr_expr(space, base, offset.to_string())
        }
    }

    fn project_addr_expr(&self, space: YulAddressSpace, base: String, offset: String) -> String {
        match space {
            YulAddressSpace::Memory | YulAddressSpace::Code | YulAddressSpace::Calldata => {
                format!("add({base}, {offset})")
            }
            YulAddressSpace::Storage | YulAddressSpace::Transient => {
                let word_offset = format!(
                    "div(add({offset}, {}), {})",
                    self.index.package_layout().word_size_bytes - 1,
                    self.index.package_layout().word_size_bytes
                );
                format!("add({base}, {word_offset})")
            }
        }
    }

    pub(super) fn read_scalar_from_addr(
        &mut self,
        class: &YulValueClass<'db>,
        space: YulAddressSpace,
        addr: String,
        dst: &str,
    ) -> Result<Vec<YulDoc>, YulError> {
        let YulValueClass::Word(word_class) = class else {
            return Err(YulError::InvalidYulPackage(format!(
                "attempted scalar load with non-word class `{class:?}`"
            )));
        };
        let value = match space {
            YulAddressSpace::Memory => format!("mload({addr})"),
            YulAddressSpace::Storage => format!("sload({addr})"),
            YulAddressSpace::Transient => format!("tload({addr})"),
            YulAddressSpace::Calldata => format!("calldataload({addr})"),
            YulAddressSpace::Code => {
                let mut docs = self.alloc_memory_name(dst, "32");
                docs.push(YulDoc::line(format!("datacopy({dst}, {addr}, 32)")));
                docs.push(YulDoc::line(format!("{dst} := mload({dst})")));
                return Ok(docs);
            }
        };
        Ok(vec![YulDoc::line(format!(
            "let {dst} := {}",
            self.canonicalize_scalar_expr(value, word_class)
        ))])
    }

    pub(super) fn read_packed_byte_scalar_from_addr(
        &mut self,
        class: &YulValueClass<'db>,
        space: YulAddressSpace,
        addr: String,
        dst: &str,
    ) -> Result<Vec<YulDoc>, YulError> {
        let YulValueClass::Word(word_class) = class else {
            return Err(YulError::InvalidYulPackage(format!(
                "attempted packed scalar load with non-word class `{class:?}`"
            )));
        };
        let value = match space {
            YulAddressSpace::Memory => format!("byte(0, mload({addr}))"),
            YulAddressSpace::Calldata => format!("byte(0, calldataload({addr}))"),
            YulAddressSpace::Code => {
                let mut docs = self.alloc_memory_name(dst, "32");
                docs.push(YulDoc::line(format!("datacopy({dst}, {addr}, 1)")));
                docs.push(YulDoc::line(format!(
                    "{dst} := {}",
                    self.canonicalize_scalar_expr(format!("byte(0, mload({dst}))"), word_class)
                )));
                return Ok(docs);
            }
            YulAddressSpace::Storage | YulAddressSpace::Transient => {
                return Err(YulError::Unsupported(format!(
                    "packed byte scalar load from {space:?} is not supported"
                )));
            }
        };
        Ok(vec![YulDoc::line(format!(
            "let {dst} := {}",
            self.canonicalize_scalar_expr(value, word_class)
        ))])
    }

    pub(super) fn read_transport_word_from_addr(
        &mut self,
        space: YulAddressSpace,
        addr: String,
        dst: &str,
    ) -> Result<Vec<YulDoc>, YulError> {
        let value = match space {
            YulAddressSpace::Memory => format!("mload({addr})"),
            YulAddressSpace::Storage => format!("sload({addr})"),
            YulAddressSpace::Transient => format!("tload({addr})"),
            YulAddressSpace::Calldata => format!("calldataload({addr})"),
            YulAddressSpace::Code => {
                let mut docs = self.alloc_memory_name(dst, "32");
                docs.push(YulDoc::line(format!("datacopy({dst}, {addr}, 32)")));
                docs.push(YulDoc::line(format!("{dst} := mload({dst})")));
                return Ok(docs);
            }
        };
        Ok(vec![YulDoc::line(format!("let {dst} := {value}"))])
    }

    fn enum_tag_of_value(&mut self, value: YLocalId) -> Result<RenderedValue<'db>, YulError> {
        let value = self.local_value(value)?;
        let layout = self.class_layout(&value.class)?;
        let tag_class = self.enum_tag_class(layout)?;
        let mut setup = value.setup;
        let temp = self.state.alloc_temp();
        let tag = YulValueClass::Word(tag_class.clone());
        let space = Self::root_space_for_class(&value.class)?;
        setup.extend(match space {
            YulAddressSpace::Memory | YulAddressSpace::Code | YulAddressSpace::Calldata => {
                self.read_packed_byte_scalar_from_addr(&tag, space, value.value, &temp)?
            }
            YulAddressSpace::Storage | YulAddressSpace::Transient => {
                self.read_scalar_from_addr(&tag, space, value.value, &temp)?
            }
        });
        Ok(RenderedValue {
            setup,
            value: temp,
            class: YulValueClass::Word(tag_class),
        })
    }

    pub(super) fn enum_tag_class(
        &self,
        layout: mir::LayoutId<'db>,
    ) -> Result<mir::ScalarClass<'db>, YulError> {
        let mir::Layout::Enum(data) = layout.data(self.db) else {
            return Err(YulError::Layout(format!(
                "enum tag requested for non-enum layout `{layout:?}`"
            )));
        };
        Ok(data.tag)
    }

    fn render_unary_expr(
        &self,
        op: UnOp,
        value: &str,
        class: &YulValueClass<'db>,
    ) -> Result<String, YulError> {
        let YulValueClass::Word(word) = class else {
            return Err(YulError::Unsupported(format!(
                "unary op `{op:?}` requires a word destination"
            )));
        };
        let raw = match op {
            UnOp::Plus => value.to_string(),
            UnOp::Minus => format!("sub(0, {value})"),
            UnOp::Not => format!("iszero({value})"),
            UnOp::BitNot => format!("not({value})"),
            UnOp::Mut | UnOp::Ref => {
                return Err(YulError::Unsupported(format!(
                    "unary op `{op:?}` is not supported in Yul emission"
                )));
            }
        };
        Ok(self.canonicalize_scalar_expr(raw, word))
    }

    fn render_unary_value(
        &mut self,
        op: UnOp,
        value: RenderedValue<'db>,
        class: YulValueClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        if !matches!(op, UnOp::Minus) {
            return Ok(RenderedValue {
                setup: value.setup,
                value: self.render_unary_expr(op, &value.value, &class)?,
                class,
            });
        }
        let YulValueClass::Word(word) = &class else {
            return Err(YulError::Unsupported(format!(
                "unary op `{op:?}` requires a word destination"
            )));
        };
        let mut setup = value.setup;
        let temp = self.state.alloc_temp();
        let raw = self.canonicalize_scalar_expr(format!("sub(0, {})", value.value), word);
        setup.push(YulDoc::line(format!("let {temp} := {raw}")));
        if self.scalar_is_signed(word) {
            setup.push(YulDoc::block(
                format!(
                    "if eq({}, {}) ",
                    value.value,
                    self.signed_min_literal(word)?
                ),
                panic_revert_docs(PANIC_OVERFLOW),
            ));
        }
        Ok(RenderedValue {
            setup,
            value: temp,
            class,
        })
    }

    fn render_binary_expr(
        &self,
        op: BinOp,
        lhs: &str,
        rhs: &str,
        class: &YulValueClass<'db>,
    ) -> Result<String, YulError> {
        let YulValueClass::Word(word) = class else {
            return Err(YulError::Unsupported(format!(
                "binary op `{op:?}` requires a word destination"
            )));
        };
        let raw = match op {
            BinOp::Arith(ArithBinOp::Add) => format!("add({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::Sub) => format!("sub({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::Mul) => format!("mul({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::Div) => {
                if self.scalar_is_signed(word) {
                    format!("sdiv({lhs}, {rhs})")
                } else {
                    format!("div({lhs}, {rhs})")
                }
            }
            BinOp::Arith(ArithBinOp::Rem) => {
                if self.scalar_is_signed(word) {
                    format!("smod({lhs}, {rhs})")
                } else {
                    format!("mod({lhs}, {rhs})")
                }
            }
            BinOp::Arith(ArithBinOp::Pow) => format!("exp({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::LShift) => format!("shl({rhs}, {lhs})"),
            BinOp::Arith(ArithBinOp::RShift) => {
                if self.scalar_is_signed(word) {
                    format!("sar({rhs}, {lhs})")
                } else {
                    format!("shr({rhs}, {lhs})")
                }
            }
            BinOp::Arith(ArithBinOp::BitAnd) => format!("and({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::BitOr) => format!("or({lhs}, {rhs})"),
            BinOp::Arith(ArithBinOp::BitXor) => format!("xor({lhs}, {rhs})"),
            BinOp::Comp(CompBinOp::Eq) => format!("eq({lhs}, {rhs})"),
            BinOp::Comp(CompBinOp::NotEq) => format!("iszero(eq({lhs}, {rhs}))"),
            BinOp::Comp(CompBinOp::Lt) => {
                if self.scalar_is_signed(word) {
                    format!("slt({lhs}, {rhs})")
                } else {
                    format!("lt({lhs}, {rhs})")
                }
            }
            BinOp::Comp(CompBinOp::LtEq) => {
                if self.scalar_is_signed(word) {
                    format!("iszero(sgt({lhs}, {rhs}))")
                } else {
                    format!("iszero(gt({lhs}, {rhs}))")
                }
            }
            BinOp::Comp(CompBinOp::Gt) => {
                if self.scalar_is_signed(word) {
                    format!("sgt({lhs}, {rhs})")
                } else {
                    format!("gt({lhs}, {rhs})")
                }
            }
            BinOp::Comp(CompBinOp::GtEq) => {
                if self.scalar_is_signed(word) {
                    format!("iszero(slt({lhs}, {rhs}))")
                } else {
                    format!("iszero(lt({lhs}, {rhs}))")
                }
            }
            BinOp::Logical(LogicalBinOp::And) => {
                format!("and(iszero(iszero({lhs})), iszero(iszero({rhs})))")
            }
            BinOp::Logical(LogicalBinOp::Or) => {
                format!("or(iszero(iszero({lhs})), iszero(iszero({rhs})))")
            }
            BinOp::Arith(ArithBinOp::Range) | BinOp::Index => {
                return Err(YulError::Unsupported(format!(
                    "binary op `{op:?}` is not supported in Yul emission"
                )));
            }
        };
        Ok(self.canonicalize_scalar_expr(raw, word))
    }

    fn render_binary_value(
        &mut self,
        op: BinOp,
        lhs: RenderedValue<'db>,
        rhs: RenderedValue<'db>,
        class: YulValueClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        match op {
            BinOp::Arith(
                ArithBinOp::Add
                | ArithBinOp::Sub
                | ArithBinOp::Mul
                | ArithBinOp::Div
                | ArithBinOp::Rem
                | ArithBinOp::Pow,
            ) => self.render_checked_binary_value(op, lhs, rhs, class),
            _ => {
                let mut setup = lhs.setup;
                setup.extend(rhs.setup);
                Ok(RenderedValue {
                    setup,
                    value: self.render_binary_expr(op, &lhs.value, &rhs.value, &class)?,
                    class,
                })
            }
        }
    }

    fn render_checked_binary_value(
        &mut self,
        op: BinOp,
        lhs: RenderedValue<'db>,
        rhs: RenderedValue<'db>,
        class: YulValueClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        let YulValueClass::Word(word) = &class else {
            return Err(YulError::Unsupported(format!(
                "checked binary op `{op:?}` requires a word destination"
            )));
        };
        if matches!(op, BinOp::Arith(ArithBinOp::Pow)) {
            return self.render_checked_pow(lhs, rhs, class);
        }
        let mut setup = lhs.setup;
        setup.extend(rhs.setup);
        let temp = self.state.alloc_temp();
        let raw = self.canonicalize_scalar_expr(
            match op {
                BinOp::Arith(ArithBinOp::Add) => format!("add({}, {})", lhs.value, rhs.value),
                BinOp::Arith(ArithBinOp::Sub) => format!("sub({}, {})", lhs.value, rhs.value),
                BinOp::Arith(ArithBinOp::Mul) => format!("mul({}, {})", lhs.value, rhs.value),
                BinOp::Arith(ArithBinOp::Div) => {
                    if self.scalar_is_signed(word) {
                        format!("sdiv({}, {})", lhs.value, rhs.value)
                    } else {
                        format!("div({}, {})", lhs.value, rhs.value)
                    }
                }
                BinOp::Arith(ArithBinOp::Rem) => {
                    if self.scalar_is_signed(word) {
                        format!("smod({}, {})", lhs.value, rhs.value)
                    } else {
                        format!("mod({}, {})", lhs.value, rhs.value)
                    }
                }
                _ => unreachable!(),
            },
            word,
        );
        setup.push(YulDoc::line(format!("let {temp} := {raw}")));
        if matches!(op, BinOp::Arith(ArithBinOp::Div | ArithBinOp::Rem)) {
            setup.push(YulDoc::block(
                format!("if iszero({}) ", rhs.value),
                panic_revert_docs(PANIC_DIVISION_BY_ZERO),
            ));
            if matches!(op, BinOp::Arith(ArithBinOp::Div)) && self.scalar_is_signed(word) {
                setup.push(YulDoc::block(
                    format!(
                        "if and(eq({}, {}), eq({}, {})) ",
                        lhs.value,
                        self.signed_min_literal(word)?,
                        rhs.value,
                        self.signed_neg_one_literal(word)?,
                    ),
                    panic_revert_docs(PANIC_OVERFLOW),
                ));
            }
        } else {
            setup.push(YulDoc::block(
                format!(
                    "if {} ",
                    self.checked_overflow_cond(op, &lhs.value, &rhs.value, &temp, word)?
                ),
                panic_revert_docs(PANIC_OVERFLOW),
            ));
        }
        Ok(RenderedValue {
            setup,
            value: temp,
            class,
        })
    }

    fn render_checked_pow(
        &mut self,
        lhs: RenderedValue<'db>,
        rhs: RenderedValue<'db>,
        class: YulValueClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        let YulValueClass::Word(word) = &class else {
            return Err(YulError::Unsupported(
                "checked pow requires a word destination".to_string(),
            ));
        };
        let mut setup = lhs.setup;
        setup.extend(rhs.setup);
        let result = self.state.alloc_temp();
        let idx = self.state.alloc_temp();
        setup.push(YulDoc::line(format!(
            "let {result} := {}",
            self.canonicalize_scalar_expr("1".to_string(), word)
        )));
        if self.scalar_is_signed(word) {
            setup.push(YulDoc::block(
                format!("if slt({}, 0) ", rhs.value),
                vec![YulDoc::line("revert(0, 0)")],
            ));
        }
        let raw = self.state.alloc_temp();
        let overflow = self.checked_overflow_cond(
            BinOp::Arith(ArithBinOp::Mul),
            &result,
            &lhs.value,
            &raw,
            word,
        )?;
        let body = vec![
            YulDoc::line(format!(
                "let {raw} := {}",
                self.canonicalize_scalar_expr(format!("mul({result}, {})", lhs.value), word)
            )),
            YulDoc::block(format!("if {overflow} "), panic_revert_docs(PANIC_OVERFLOW)),
            YulDoc::line(format!("{result} := {raw}")),
        ];
        setup.push(YulDoc::block(
            format!(
                "for {{ let {idx} := 0 }} lt({idx}, {}) {{ {idx} := add({idx}, 1) }} ",
                rhs.value
            ),
            body,
        ));
        Ok(RenderedValue {
            setup,
            value: result,
            class,
        })
    }

    fn render_intrinsic_arith(
        &mut self,
        op: mir::IntrinsicArithBinOp,
        checked: bool,
        lhs: YLocalId,
        rhs: YLocalId,
        class: mir::ScalarClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        if checked {
            let lhs = self.local_value(lhs)?;
            let rhs = self.local_value(rhs)?;
            return self.render_checked_binary_value(
                BinOp::Arith(match op {
                    mir::IntrinsicArithBinOp::Add => ArithBinOp::Add,
                    mir::IntrinsicArithBinOp::Sub => ArithBinOp::Sub,
                    mir::IntrinsicArithBinOp::Mul => ArithBinOp::Mul,
                    mir::IntrinsicArithBinOp::Div => ArithBinOp::Div,
                    mir::IntrinsicArithBinOp::Rem => ArithBinOp::Rem,
                    mir::IntrinsicArithBinOp::Pow => ArithBinOp::Pow,
                }),
                lhs,
                rhs,
                YulValueClass::Word(class),
            );
        }
        let lhs = self.local_value(lhs)?;
        let rhs = self.local_value(rhs)?;
        let mut setup = lhs.setup;
        setup.extend(rhs.setup);
        let value = match op {
            mir::IntrinsicArithBinOp::Add => format!("add({}, {})", lhs.value, rhs.value),
            mir::IntrinsicArithBinOp::Sub => format!("sub({}, {})", lhs.value, rhs.value),
            mir::IntrinsicArithBinOp::Mul => format!("mul({}, {})", lhs.value, rhs.value),
            mir::IntrinsicArithBinOp::Div => {
                if self.scalar_is_signed(&class) {
                    format!("sdiv({}, {})", lhs.value, rhs.value)
                } else {
                    format!("div({}, {})", lhs.value, rhs.value)
                }
            }
            mir::IntrinsicArithBinOp::Rem => {
                if self.scalar_is_signed(&class) {
                    format!("smod({}, {})", lhs.value, rhs.value)
                } else {
                    format!("mod({}, {})", lhs.value, rhs.value)
                }
            }
            mir::IntrinsicArithBinOp::Pow => format!("exp({}, {})", lhs.value, rhs.value),
        };
        Ok(RenderedValue {
            setup,
            value: self.canonicalize_scalar_expr(value, &class),
            class: YulValueClass::Word(class),
        })
    }

    fn render_saturating_builtin(
        &mut self,
        op: mir::SaturatingBinOp,
        lhs: YLocalId,
        rhs: YLocalId,
        class: mir::ScalarClass<'db>,
    ) -> Result<RenderedValue<'db>, YulError> {
        let lhs = self.local_value(lhs)?;
        let rhs = self.local_value(rhs)?;
        let mut setup = lhs.setup;
        setup.extend(rhs.setup);
        let temp = self.state.alloc_temp();
        let raw = self.canonicalize_scalar_expr(
            match op {
                mir::SaturatingBinOp::Add => format!("add({}, {})", lhs.value, rhs.value),
                mir::SaturatingBinOp::Sub => format!("sub({}, {})", lhs.value, rhs.value),
                mir::SaturatingBinOp::Mul => format!("mul({}, {})", lhs.value, rhs.value),
            },
            &class,
        );
        setup.push(YulDoc::line(format!("let {temp} := {raw}")));
        let overflow = self.checked_overflow_cond(
            match op {
                mir::SaturatingBinOp::Add => BinOp::Arith(ArithBinOp::Add),
                mir::SaturatingBinOp::Sub => BinOp::Arith(ArithBinOp::Sub),
                mir::SaturatingBinOp::Mul => BinOp::Arith(ArithBinOp::Mul),
            },
            &lhs.value,
            &rhs.value,
            &temp,
            &class,
        )?;
        if self.scalar_is_signed(&class) {
            let (when_true, when_false) = match op {
                mir::SaturatingBinOp::Add | mir::SaturatingBinOp::Sub => (
                    self.signed_max_literal(&class)?,
                    self.signed_min_literal(&class)?,
                ),
                mir::SaturatingBinOp::Mul => (
                    self.signed_max_literal(&class)?,
                    self.signed_min_literal(&class)?,
                ),
            };
            let sign_cond = match op {
                mir::SaturatingBinOp::Add | mir::SaturatingBinOp::Sub => {
                    format!("iszero(slt({}, 0))", lhs.value)
                }
                mir::SaturatingBinOp::Mul => {
                    format!("eq(slt({}, 0), slt({}, 0))", lhs.value, rhs.value)
                }
            };
            setup.push(YulDoc::block(
                format!("if {overflow} "),
                vec![
                    YulDoc::block(
                        format!("if {sign_cond} "),
                        vec![YulDoc::line(format!("{temp} := {when_true}"))],
                    ),
                    YulDoc::block(
                        format!("if iszero({sign_cond}) "),
                        vec![YulDoc::line(format!("{temp} := {when_false}"))],
                    ),
                ],
            ));
        } else {
            let replacement = match op {
                mir::SaturatingBinOp::Add | mir::SaturatingBinOp::Mul => {
                    self.unsigned_max_literal(&class)?
                }
                mir::SaturatingBinOp::Sub => "0".to_string(),
            };
            setup.push(YulDoc::block(
                format!("if {overflow} "),
                vec![YulDoc::line(format!("{temp} := {replacement}"))],
            ));
        }
        Ok(RenderedValue {
            setup,
            value: temp,
            class: YulValueClass::Word(class),
        })
    }

    fn checked_overflow_cond(
        &self,
        op: BinOp,
        lhs: &str,
        rhs: &str,
        raw: &str,
        word: &mir::ScalarClass<'db>,
    ) -> Result<String, YulError> {
        let signed = self.scalar_is_signed(word);
        Ok(match op {
            BinOp::Arith(ArithBinOp::Add) if !signed => format!("lt({raw}, {lhs})"),
            BinOp::Arith(ArithBinOp::Add) => format!(
                "and(eq(slt({lhs}, 0), slt({rhs}, 0)), iszero(eq(slt({raw}, 0), slt({lhs}, 0))))"
            ),
            BinOp::Arith(ArithBinOp::Sub) if !signed => format!("gt({rhs}, {lhs})"),
            BinOp::Arith(ArithBinOp::Sub) => format!(
                "and(iszero(eq(slt({lhs}, 0), slt({rhs}, 0))), iszero(eq(slt({raw}, 0), slt({lhs}, 0))))"
            ),
            BinOp::Arith(ArithBinOp::Mul) if !signed => {
                format!("and(iszero(iszero({lhs})), iszero(eq(div({raw}, {lhs}), {rhs})))")
            }
            BinOp::Arith(ArithBinOp::Mul) => {
                let min = self.signed_min_literal(word)?;
                let neg_one = self.signed_neg_one_literal(word)?;
                format!(
                    "or(or(and(eq({lhs}, {min}), eq({rhs}, {neg_one})), and(eq({rhs}, {min}), eq({lhs}, {neg_one}))), and(and(iszero(iszero({lhs})), iszero(iszero({rhs}))), iszero(eq(sdiv({raw}, {lhs}), {rhs}))))"
                )
            }
            BinOp::Arith(ArithBinOp::Div) if !signed => format!("iszero({rhs})"),
            BinOp::Arith(ArithBinOp::Div) => format!(
                "or(iszero({rhs}), and(eq({lhs}, {}), eq({rhs}, {})))",
                self.signed_min_literal(word)?,
                self.signed_neg_one_literal(word)?,
            ),
            BinOp::Arith(ArithBinOp::Rem) => "iszero(".to_string() + rhs + ")",
            BinOp::Arith(ArithBinOp::Pow) => {
                return Err(YulError::Unsupported(
                    "pow overflow should be handled by checked pow lowering".to_string(),
                ));
            }
            _ => {
                return Err(YulError::Unsupported(format!(
                    "overflow check requested for non-checked op `{op:?}`"
                )));
            }
        })
    }

    fn scalar_is_signed(&self, class: &mir::ScalarClass<'db>) -> bool {
        matches!(class.repr, mir::ScalarRepr::Int { signed: true, .. })
    }

    fn unsigned_max_literal(&self, class: &mir::ScalarClass<'db>) -> Result<String, YulError> {
        match class.repr {
            mir::ScalarRepr::Bool => Ok("1".to_string()),
            mir::ScalarRepr::Int {
                bits,
                signed: false,
            }
            | mir::ScalarRepr::Address { bits } => {
                let bytes = bits.div_ceil(8) as usize;
                Ok(format!("0x{}", "ff".repeat(bytes)))
            }
            mir::ScalarRepr::FixedBytes { len } => Ok(format!("0x{}", "ff".repeat(len as usize))),
            mir::ScalarRepr::Int { signed: true, .. } => Err(YulError::Unsupported(
                "unsigned max literal requested for signed scalar".to_string(),
            )),
        }
    }

    fn signed_min_literal(&self, class: &mir::ScalarClass<'db>) -> Result<String, YulError> {
        let mir::ScalarRepr::Int { bits, signed: true } = class.repr else {
            return Err(YulError::Unsupported(
                "signed min literal requires a signed integer scalar".to_string(),
            ));
        };
        let bytes = bits.div_ceil(8) as usize;
        if bits == 256 {
            return Ok(format!("0x80{}", "00".repeat(31)));
        }
        Ok(format!(
            "0x{}80{}",
            "ff".repeat(32 - bytes),
            "00".repeat(bytes - 1)
        ))
    }

    fn signed_max_literal(&self, class: &mir::ScalarClass<'db>) -> Result<String, YulError> {
        let mir::ScalarRepr::Int { bits, signed: true } = class.repr else {
            return Err(YulError::Unsupported(
                "signed max literal requires a signed integer scalar".to_string(),
            ));
        };
        let bytes = bits.div_ceil(8) as usize;
        Ok(format!("0x7f{}", "ff".repeat(bytes - 1)))
    }

    fn signed_neg_one_literal(&self, class: &mir::ScalarClass<'db>) -> Result<String, YulError> {
        let mir::ScalarRepr::Int { signed: true, .. } = class.repr else {
            return Err(YulError::Unsupported(
                "signed -1 literal requires a signed integer scalar".to_string(),
            ));
        };
        Ok(format!("0x{}", "ff".repeat(32)))
    }

    fn cast_word_expr(&self, value: &str, to: &mir::ScalarClass<'db>) -> String {
        self.canonicalize_scalar_expr(value.to_string(), to)
    }

    pub(super) fn canonicalize_scalar_expr(
        &self,
        value: String,
        class: &mir::ScalarClass<'db>,
    ) -> String {
        match class.repr {
            mir::ScalarRepr::Bool => format!("iszero(iszero({value}))"),
            mir::ScalarRepr::Int { bits, signed } => {
                if bits == 256 {
                    value
                } else if signed {
                    format!(
                        "signextend({}, and({value}, {}))",
                        bits.div_ceil(8) - 1,
                        Self::raw_word_mask(class).unwrap_or_else(|| {
                            "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                                .to_string()
                        })
                    )
                } else if let Some(mask) = Self::raw_word_mask(class) {
                    format!("and({value}, {mask})")
                } else {
                    value
                }
            }
            mir::ScalarRepr::FixedBytes { .. } | mir::ScalarRepr::Address { .. } => {
                if let Some(mask) = Self::raw_word_mask(class) {
                    format!("and({value}, {mask})")
                } else {
                    value
                }
            }
        }
    }

    pub(super) fn const_scalar_expr(&self, value: &mir::ConstScalar) -> String {
        match value {
            mir::ConstScalar::Bool(flag) => u8::from(*flag).to_string(),
            mir::ConstScalar::Int { words, .. } => {
                if words.is_empty() {
                    "0".to_string()
                } else {
                    format!("0x{}", hex::encode(words))
                }
            }
            mir::ConstScalar::FixedBytes(bytes) => format!("0x{}", hex::encode(bytes)),
            mir::ConstScalar::Address { bytes, .. } => format!("0x{}", hex::encode(bytes)),
        }
    }

    fn scalar_repr_for_const(&self, value: &mir::ConstScalar) -> mir::ScalarRepr {
        match value {
            mir::ConstScalar::Bool(_) => mir::ScalarRepr::Bool,
            mir::ConstScalar::Int { bits, signed, .. } => mir::ScalarRepr::Int {
                bits: *bits,
                signed: *signed,
            },
            mir::ConstScalar::FixedBytes(bytes) => mir::ScalarRepr::FixedBytes {
                len: bytes.len() as u16,
            },
            mir::ConstScalar::Address { bits, .. } => mir::ScalarRepr::Address { bits: *bits },
        }
    }

    fn word_u256_class(&self) -> YulValueClass<'db> {
        YulValueClass::Word(mir::ScalarClass {
            repr: mir::ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            role: mir::ScalarRole::Plain,
        })
    }
}
