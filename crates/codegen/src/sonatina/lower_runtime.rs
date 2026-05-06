use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use driver::DriverDataBase;
use hir::{
    analysis::{
        semantic::FieldIndex,
        ty::{
            ty_check::BodyOwner,
            ty_def::{PrimTy, TyBase, TyData, TyId},
        },
    },
    hir_def::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp},
    projection::IndexSource,
};
use mir::runtime::RefKind;
use mir::{
    AddressSpaceKind, ConstNode, ConstRegionId, ConstScalar, IntrinsicArithBinOp, Layout, LayoutId,
    RBlockId, RExpr, RLocalId, RStmt, RTerminator, ResolvedPlaceElem, ResolvedPlaceRootKind,
    RuntimeBody, RuntimeBuiltin, RuntimeClass, RuntimeFunction, RuntimeInlineHint, RuntimeInstance,
    RuntimeLinkage, RuntimeLocalRoot, RuntimePackage, RuntimePlace, SaturatingBinOp, ScalarClass,
    ScalarRepr, VariantId, instance::RuntimeInstanceSource, resolve_runtime_place,
};
use rustc_hash::FxHashMap;
use smallvec1::{SmallVec, smallvec};
use sonatina_ir::{
    BlockId, GlobalVariableData, GlobalVariableRef, I256, Immediate, Linkage, Module, Signature,
    Type, Value, ValueId,
    builder::{FunctionBuilder, ModuleBuilder, ObjectBuilder, Variable},
    func_cursor::InstInserter,
    inst::{
        arith::{Add, Mul, Neg, Sar, Shl, Shr, Sub},
        cast::{Bitcast, IntToPtr, PtrToInt, Sext, Trunc, Zext},
        cmp::{Eq, Gt, IsZero, Lt, Ne, Slt},
        control_flow::{Br, BrTable, Call, Jump, Phi, Return, Unreachable},
        data::{
            Alloca, ConstIndex, ConstLoad, ConstProj, ConstRef, EnumAssertVariant,
            EnumAssertVariantRef, EnumExtract, EnumGetTag, EnumIsVariant, EnumMake, EnumProj,
            EnumSetTag, EnumTag, EnumWriteVariant, Mload, Mstore, ObjAlloc, ObjIndex, ObjInitConst,
            ObjLoad, ObjProj, ObjStore, SymAddr, SymSize, SymbolRef,
        },
        evm::{
            EvmAddMod, EvmAddress, EvmBaseFee, EvmBlockHash, EvmCall, EvmCallValue,
            EvmCalldataCopy, EvmCalldataLoad, EvmCalldataSize, EvmCaller, EvmChainId, EvmCodeCopy,
            EvmCodeSize, EvmCoinBase, EvmCreate, EvmCreate2, EvmDelegateCall, EvmExp, EvmGas,
            EvmGasLimit, EvmInvalid, EvmKeccak256, EvmLog0, EvmLog1, EvmLog2, EvmLog3, EvmLog4,
            EvmMalloc, EvmMcopy, EvmMsize, EvmMstore8, EvmMulMod, EvmNumber, EvmOrigin,
            EvmPrevRandao, EvmReturn, EvmReturnDataCopy, EvmReturnDataSize, EvmRevert, EvmSdiv,
            EvmSelfBalance, EvmSelfDestruct, EvmSignExtend, EvmSload, EvmSmod, EvmSstore,
            EvmStaticCall, EvmStop, EvmTimestamp, EvmTload, EvmTstore, EvmUdiv, EvmUmod,
            inst_set::EvmInstSet,
        },
        logic::{And, Not, Or, Xor},
    },
    isa::Isa,
    module::FuncRef,
    object::EmbedSymbol,
    types::{CompoundType, EnumReprHint, EnumVariantRef, VariantData},
};

use super::{LowerError, create_module_ctx};
use crate::{
    TargetDataLayout,
    function_symbols::{FunctionSymbolInput, FunctionSymbolStyle, assign_function_symbols},
};

const PANIC_OVERFLOW: u64 = 0x11;
const PANIC_DIVISION_BY_ZERO: u64 = 0x12;

pub(super) fn compile_runtime_package_sonatina(
    db: &DriverDataBase,
    package: &RuntimePackage<'_>,
    layout: TargetDataLayout,
) -> Result<Module, LowerError> {
    let _ = layout;
    let builder = ModuleBuilder::new(create_module_ctx());
    let isa = super::create_evm_isa();
    let mut lowerer = ModuleLowerer::new(db, builder, &isa, package);
    lowerer.declare_functions()?;
    lowerer.lower_const_regions()?;
    lowerer.lower_bodies()?;
    lowerer.declare_objects()?;
    Ok(lowerer.finish())
}

struct ModuleLowerer<'db, 'a> {
    db: &'db DriverDataBase,
    builder: ModuleBuilder,
    isa: &'a sonatina_ir::isa::evm::Evm,
    package: &'a RuntimePackage<'db>,
    func_map: FxHashMap<mir::RuntimeInstance<'db>, FuncRef>,
    func_symbols: FxHashMap<mir::RuntimeInstance<'db>, String>,
    section_membership: FxHashMap<mir::RuntimeInstance<'db>, Vec<mir::RuntimeSectionRef<'db>>>,
    type_cache: FxHashMap<LayoutId<'db>, Type>,
    layout_names: FxHashMap<LayoutId<'db>, String>,
    const_globals: FxHashMap<ConstRegionId<'db>, GlobalVariableRef>,
    const_names: FxHashMap<ConstRegionId<'db>, String>,
}

impl<'db, 'a> ModuleLowerer<'db, 'a> {
    fn new(
        db: &'db DriverDataBase,
        builder: ModuleBuilder,
        isa: &'a sonatina_ir::isa::evm::Evm,
        package: &'a RuntimePackage<'db>,
    ) -> Self {
        Self {
            db,
            builder,
            isa,
            package,
            func_map: FxHashMap::default(),
            func_symbols: assign_sonatina_function_symbols(db, package),
            section_membership: compute_section_membership(db, package),
            type_cache: FxHashMap::default(),
            layout_names: FxHashMap::default(),
            const_globals: FxHashMap::default(),
            const_names: FxHashMap::default(),
        }
    }

    fn finish(self) -> Module {
        self.builder.build()
    }

    fn inst_set(&self) -> &'static EvmInstSet {
        self.isa.inst_set()
    }

    fn function_symbol(&self, instance: RuntimeInstance<'db>) -> String {
        self.func_symbols
            .get(&instance)
            .cloned()
            .or_else(|| {
                self.package
                    .functions(self.db)
                    .into_iter()
                    .find(|function| function.instance(self.db) == instance)
                    .map(|function| function.symbol(self.db).clone())
            })
            .unwrap_or_else(|| format!("{:?}", instance.key(self.db)))
    }

    fn sections_for_function(
        &self,
        instance: RuntimeInstance<'db>,
    ) -> &[mir::RuntimeSectionRef<'db>] {
        self.section_membership
            .get(&instance)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    fn declare_functions(&mut self) -> Result<(), LowerError> {
        for function in self.package.functions(self.db) {
            if runtime_intrinsic(self.db, function.instance(self.db)).is_some() {
                continue;
            }
            let signature = self.lower_signature(function)?;
            let func_ref = self.builder.declare_function(signature).map_err(|err| {
                LowerError::Internal(format!("failed to declare function: {err}"))
            })?;
            self.apply_inline_hint(func_ref, function.inline_hint(self.db));
            self.func_map.insert(function.instance(self.db), func_ref);
        }
        Ok(())
    }

    fn lower_signature(&mut self, function: RuntimeFunction<'db>) -> Result<Signature, LowerError> {
        let body = function.instance(self.db).body(self.db);
        let args = body
            .signature
            .params
            .iter()
            .map(|param| self.ty_for_class(&param.class))
            .collect::<Result<Vec<_>, _>>()?;
        let ret = body
            .signature
            .ret
            .as_ref()
            .map(|class| self.ty_for_class(class))
            .transpose()?;
        let symbol = self.function_symbol(function.instance(self.db));
        Ok(match ret {
            Some(ret) => Signature::new_single(
                &symbol,
                linkage_for_runtime(function.linkage(self.db)),
                &args,
                ret,
            ),
            None => Signature::new_unit(
                &symbol,
                linkage_for_runtime(function.linkage(self.db)),
                &args,
            ),
        })
    }

    fn apply_inline_hint(&self, func_ref: FuncRef, hint: RuntimeInlineHint) {
        let hint = match hint {
            RuntimeInlineHint::Auto => sonatina_ir::InlineHint::Auto,
            RuntimeInlineHint::Hint => sonatina_ir::InlineHint::Inline,
            RuntimeInlineHint::Always => sonatina_ir::InlineHint::Always,
            RuntimeInlineHint::Never => sonatina_ir::InlineHint::Never,
        };
        self.builder.ctx.set_inline_hint(func_ref, hint);
    }

    fn lower_const_regions(&mut self) -> Result<(), LowerError> {
        for region in self.package.const_regions(self.db) {
            self.lower_const_region(region)?;
        }
        Ok(())
    }

    fn lower_const_region(
        &mut self,
        region: ConstRegionId<'db>,
    ) -> Result<GlobalVariableRef, LowerError> {
        if let Some(&existing) = self.const_globals.get(&region) {
            return Ok(existing);
        }

        let ty = self.ty_for_layout(region.layout(self.db))?;
        let init = self.gv_initializer_for_const(region.value(self.db).clone())?;
        let name = self.const_name(region);
        let gv = self.builder.declare_gv(GlobalVariableData::constant(
            name,
            ty,
            Linkage::Private,
            init,
        ));
        self.const_globals.insert(region, gv);
        Ok(gv)
    }

    fn gv_initializer_for_const(
        &mut self,
        node: ConstNode<'db>,
    ) -> Result<sonatina_ir::global_variable::GvInitializer, LowerError> {
        Ok(match node {
            ConstNode::Scalar(scalar) => sonatina_ir::global_variable::GvInitializer::make_imm(
                self.immediate_for_const(&scalar, None)?,
            ),
            ConstNode::Aggregate { layout, fields } => {
                let ty = self.ty_for_layout(layout)?;
                let compound = ty.resolve_compound(&self.builder.ctx).ok_or_else(|| {
                    LowerError::Internal(format!("const aggregate type `{ty:?}` is not compound"))
                })?;
                match compound {
                    CompoundType::Array { .. } => {
                        sonatina_ir::global_variable::GvInitializer::make_array(
                            fields
                                .iter()
                                .cloned()
                                .map(|field| self.gv_initializer_for_const(field))
                                .collect::<Result<Vec<_>, _>>()?,
                        )
                    }
                    CompoundType::Struct(_) => {
                        sonatina_ir::global_variable::GvInitializer::make_struct(
                            fields
                                .iter()
                                .cloned()
                                .map(|field| self.gv_initializer_for_const(field))
                                .collect::<Result<Vec<_>, _>>()?,
                        )
                    }
                    CompoundType::Enum(_) => {
                        return Err(LowerError::Unsupported(
                            "enum const globals are not yet supported by Sonatina object data encoding"
                                .to_string(),
                        ));
                    }
                    CompoundType::Ptr(_)
                    | CompoundType::ObjRef(_)
                    | CompoundType::ConstRef(_)
                    | CompoundType::Func { .. } => {
                        return Err(LowerError::Unsupported(
                            "reference/function const globals are not supported".to_string(),
                        ));
                    }
                }
            }
        })
    }

    fn lower_bodies(&mut self) -> Result<(), LowerError> {
        for function in self.package.functions(self.db) {
            if runtime_intrinsic(self.db, function.instance(self.db)).is_some() {
                continue;
            }
            let body = function.instance(self.db).body(self.db);
            let func_ref = self.func_ref(function.instance(self.db))?;
            let ctx = FunctionLowerer::new(self, body, func_ref)?;
            ctx.lower()?;
        }
        Ok(())
    }

    fn declare_objects(&mut self) -> Result<(), LowerError> {
        for object in self.package.objects(self.db) {
            let mut object_builder = ObjectBuilder::new(object.name(self.db).clone());
            for section in object.sections(self.db) {
                let section_builder =
                    object_builder.section(super::section_name_for_runtime(&section.name));
                section_builder.entry(self.func_ref(section.entry.instance(self.db))?);
                for region in &section.const_regions {
                    section_builder.data(self.lower_const_region(*region)?);
                }
                for embed in &section.embeds {
                    match &embed.source {
                        mir::RuntimeSectionRef::Local { object: _, section } => {
                            section_builder.embed_local(
                                super::section_name_for_runtime(section),
                                EmbedSymbol::from(embed.as_symbol.clone()),
                            );
                        }
                        mir::RuntimeSectionRef::External { object, section } => {
                            section_builder.embed_external(
                                object.name(self.db).clone(),
                                super::section_name_for_runtime(section),
                                EmbedSymbol::from(embed.as_symbol.clone()),
                            );
                        }
                    }
                }
            }
            object_builder
                .declare(&mut self.builder)
                .map_err(|err| LowerError::Internal(format!("failed to declare object: {err}")))?;
        }
        Ok(())
    }

    fn func_ref(&self, instance: mir::RuntimeInstance<'db>) -> Result<FuncRef, LowerError> {
        self.func_map.get(&instance).copied().ok_or_else(|| {
            let declared = self
                .package
                .functions(self.db)
                .iter()
                .map(|func| describe_runtime_instance(self.db, func.instance(self.db)))
                .collect::<Vec<_>>();
            LowerError::Internal(format!(
                "missing declared function for {instance:?}: {}; declared={declared:?}",
                describe_runtime_instance(self.db, instance),
            ))
        })
    }

    fn ty_for_layout(&mut self, layout: LayoutId<'db>) -> Result<Type, LowerError> {
        if let Some(&existing) = self.type_cache.get(&layout) {
            return Ok(existing);
        }

        let ty = match layout.data(self.db) {
            Layout::Struct(data) => {
                let fields = data
                    .fields
                    .iter()
                    .map(|field| self.ty_for_class(field))
                    .collect::<Result<Vec<_>, _>>()?;
                let name = self.layout_name(layout);
                self.builder.declare_struct_type(&name, &fields, false)
            }
            Layout::Array(data) => {
                let elem = self.ty_for_class(&data.elem)?;
                self.builder.declare_array_type(elem, data.len as usize)
            }
            Layout::Enum(data) => {
                let variants = data
                    .variants
                    .iter()
                    .map(|variant| {
                        Ok(VariantData {
                            name: variant.name.clone(),
                            explicit_discriminant: None,
                            fields: variant
                                .fields
                                .iter()
                                .map(|field| self.ty_for_class(field))
                                .collect::<Result<Vec<_>, LowerError>>()?,
                        })
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                let name = self.layout_name(layout);
                self.builder
                    .declare_enum_type(&name, &variants, EnumReprHint::Default)
            }
        };
        self.type_cache.insert(layout, ty);
        Ok(ty)
    }

    fn const_name(&mut self, region: ConstRegionId<'db>) -> String {
        if let Some(name) = self.const_names.get(&region) {
            return name.clone();
        }
        let name = format!("const_region_{}", self.const_names.len());
        self.const_names.insert(region, name.clone());
        name
    }

    fn layout_name(&mut self, layout: LayoutId<'db>) -> String {
        if let Some(name) = self.layout_names.get(&layout) {
            return name.clone();
        }
        let name = format!("layout_{}", self.layout_names.len());
        self.layout_names.insert(layout, name.clone());
        name
    }

    fn ty_for_class(&mut self, class: &RuntimeClass<'db>) -> Result<Type, LowerError> {
        Ok(match class {
            RuntimeClass::Scalar(scalar) => self.scalar_ty(scalar)?,
            RuntimeClass::AggregateValue { layout } => self.ty_for_layout(*layout)?,
            RuntimeClass::Ref { pointee, kind, .. } => match kind {
                RefKind::Const => {
                    let pointee_ty = self.ty_for_class(pointee)?;
                    self.builder.constref_type(pointee_ty)
                }
                RefKind::Object => {
                    let pointee_ty = self.ty_for_class(pointee)?;
                    self.builder.objref_type(pointee_ty)
                }
                RefKind::Provider {
                    space: AddressSpaceKind::Memory,
                    ..
                } => {
                    let pointee_ty = self.ty_for_class(pointee)?;
                    self.builder.objref_type(pointee_ty)
                }
                RefKind::Provider { .. } => Type::I256,
            },
            RuntimeClass::RawAddr { .. } => Type::I256,
        })
    }

    fn scalar_ty(&mut self, scalar: &ScalarClass<'db>) -> Result<Type, LowerError> {
        Ok(match scalar.role {
            mir::ScalarRole::EnumTag { enum_layout } => self.enum_tag_ty(enum_layout)?,
            _ => scalar_ty(scalar),
        })
    }

    fn enum_tag_ty(&mut self, enum_layout: LayoutId<'db>) -> Result<Type, LowerError> {
        let Type::Compound(enum_ty) = self.ty_for_layout(enum_layout)? else {
            return Err(LowerError::Internal(format!(
                "enum layout `{enum_layout:?}` should lower to a compound type"
            )));
        };
        Ok(Type::EnumTag(enum_ty))
    }

    fn immediate_for_const(
        &mut self,
        scalar: &ConstScalar,
        class: Option<&RuntimeClass<'db>>,
    ) -> Result<Immediate, LowerError> {
        if let Some(RuntimeClass::Scalar(ScalarClass {
            role: mir::ScalarRole::EnumTag { enum_layout },
            ..
        })) = class
        {
            let ConstScalar::Int { words, .. } = scalar else {
                return Err(LowerError::Internal(format!(
                    "enum tag constant should be integer-backed, found `{scalar:?}`"
                )));
            };
            let Type::EnumTag(enum_ty) = self.enum_tag_ty(*enum_layout)? else {
                unreachable!("enum tag layouts should lower to enum tag types");
            };
            return Ok(Immediate::EnumTag {
                enum_ty,
                value: bytes_to_i256(words, false),
            });
        }
        Ok(match scalar {
            ConstScalar::Bool(value) => Immediate::from(*value),
            ConstScalar::Int {
                bits,
                signed,
                words,
            } => Immediate::from_i256(bytes_to_i256(words, *signed), int_ty(*bits)),
            ConstScalar::FixedBytes(bytes) => Immediate::from_i256(
                bytes_to_i256(bytes, false),
                fixed_bytes_ty(bytes.len() as u16),
            ),
            ConstScalar::Address { bytes, .. } => {
                Immediate::from_i256(bytes_to_i256(bytes, false), Type::I256)
            }
        })
    }

    fn enum_tag_immediate(
        &mut self,
        enum_layout: LayoutId<'db>,
        value: u16,
    ) -> Result<Immediate, LowerError> {
        let Type::EnumTag(enum_ty) = self.enum_tag_ty(enum_layout)? else {
            unreachable!("enum tag layouts should lower to enum tag types");
        };
        Ok(Immediate::EnumTag {
            enum_ty,
            value: I256::from(value as u64),
        })
    }
}

const SONATINA_FUNCTION_SYMBOL_STYLE: FunctionSymbolStyle = FunctionSymbolStyle {
    segment_separator: "__",
    variant_separator: "_",
    fallback_separator: "_",
    sanitize_segment: sanitize_sonatina_ident_segment,
    namespace_key: sonatina_symbol_namespace_key,
};

fn assign_sonatina_function_symbols<'db>(
    db: &'db DriverDataBase,
    package: &RuntimePackage<'db>,
) -> FxHashMap<mir::RuntimeInstance<'db>, String> {
    let functions = package
        .functions(db)
        .into_iter()
        .filter(|function| runtime_intrinsic(db, function.instance(db)).is_none())
        .collect::<Vec<_>>();
    let inputs = functions
        .iter()
        .map(|function| FunctionSymbolInput {
            owner: function.owner(db).clone(),
            fallback_symbol: function.symbol(db).clone(),
            variant_suffix: String::new(),
            disambiguator: mir::runtime_instance_symbol_key(db, function.instance(db)),
        })
        .collect::<Vec<_>>();
    functions
        .into_iter()
        .zip(assign_function_symbols(
            db,
            &inputs,
            &SONATINA_FUNCTION_SYMBOL_STYLE,
        ))
        .map(|(function, symbol)| (function.instance(db), symbol))
        .collect()
}

fn sanitize_sonatina_ident_segment(value: &str) -> String {
    let sanitized = value
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                ch
            } else {
                '_'
            }
        })
        .collect::<String>();
    if sanitized
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
    {
        sanitized
    } else {
        format!("_{sanitized}")
    }
}

fn sonatina_symbol_namespace_key(symbol: &str) -> String {
    symbol.to_string()
}

fn describe_runtime_instance<'db>(
    db: &DriverDataBase,
    instance: mir::RuntimeInstance<'db>,
) -> String {
    let key = instance.key(db);
    match key.source(db) {
        RuntimeInstanceSource::Semantic(semantic) => {
            let owner = semantic.key(db).owner(db);
            let owner_desc = match owner {
                BodyOwner::Func(func) => func
                    .name(db)
                    .to_opt()
                    .map(|name| format!("func {}", name.data(db)))
                    .unwrap_or_else(|| format!("func {func:?}")),
                BodyOwner::Const(const_) => format!("const {const_:?}"),
                BodyOwner::AnonConstBody { .. } => format!("{owner:?}"),
                BodyOwner::ContractInit { contract } => contract
                    .name(db)
                    .to_opt()
                    .map(|name| format!("contract-init {}", name.data(db)))
                    .unwrap_or_else(|| format!("{owner:?}")),
                BodyOwner::ContractRecvArm { contract, .. } => contract
                    .name(db)
                    .to_opt()
                    .map(|name| format!("contract-recv {}", name.data(db)))
                    .unwrap_or_else(|| format!("{owner:?}")),
            };
            format!("semantic owner={owner_desc} params={:?}", key.params(db))
        }
        RuntimeInstanceSource::Synthetic(spec) => {
            format!(
                "synthetic spec={:?} params={:?}",
                spec.spec(db),
                key.params(db)
            )
        }
    }
}

#[derive(Clone, Copy)]
enum RuntimeIntrinsic<'db> {
    Alloc,
    GenericSaturating {
        op: GenericSaturatingOp,
        ty: TyId<'db>,
    },
    Numeric {
        op: NumericIntrinsicOp,
        prim: PrimTy,
    },
}

#[derive(Clone, Copy)]
enum GenericSaturatingOp {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Copy)]
enum NumericIntrinsicOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BitNot,
    Not,
    Neg,
}

const INTRINSIC_SUFFIX_TYPES: &[(&str, PrimTy)] = &[
    ("_u8", PrimTy::U8),
    ("_u16", PrimTy::U16),
    ("_u32", PrimTy::U32),
    ("_u64", PrimTy::U64),
    ("_u128", PrimTy::U128),
    ("_u256", PrimTy::U256),
    ("_usize", PrimTy::Usize),
    ("_i8", PrimTy::I8),
    ("_i16", PrimTy::I16),
    ("_i32", PrimTy::I32),
    ("_i64", PrimTy::I64),
    ("_i128", PrimTy::I128),
    ("_i256", PrimTy::I256),
    ("_isize", PrimTy::Isize),
    ("_bool", PrimTy::Bool),
];

fn runtime_intrinsic<'db>(
    db: &'db DriverDataBase,
    instance: mir::RuntimeInstance<'db>,
) -> Option<RuntimeIntrinsic<'db>> {
    let semantic = instance.key(db).semantic(db)?;
    let hir::analysis::ty::ty_check::BodyOwner::Func(func) = semantic.key(db).owner(db) else {
        return None;
    };
    if func.body(db).is_some() {
        return None;
    }
    let name = func.name(db).to_opt()?.data(db);
    if name == "alloc" {
        return Some(RuntimeIntrinsic::Alloc);
    }
    if let Some(op) = match name.as_str() {
        "__saturating_add" => Some(GenericSaturatingOp::Add),
        "__saturating_sub" => Some(GenericSaturatingOp::Sub),
        "__saturating_mul" => Some(GenericSaturatingOp::Mul),
        _ => None,
    } {
        let ty = *semantic.key(db).subst(db).generic_args(db).first()?;
        return Some(RuntimeIntrinsic::GenericSaturating { op, ty });
    }
    let (op, prim) = intrinsic_name_parts(name.as_str())?;
    Some(RuntimeIntrinsic::Numeric {
        op: match op {
            "add" => NumericIntrinsicOp::Add,
            "sub" => NumericIntrinsicOp::Sub,
            "mul" => NumericIntrinsicOp::Mul,
            "div" => NumericIntrinsicOp::Div,
            "rem" => NumericIntrinsicOp::Rem,
            "pow" => NumericIntrinsicOp::Pow,
            "shl" => NumericIntrinsicOp::Shl,
            "shr" => NumericIntrinsicOp::Shr,
            "bitand" => NumericIntrinsicOp::BitAnd,
            "bitor" => NumericIntrinsicOp::BitOr,
            "bitxor" => NumericIntrinsicOp::BitXor,
            "eq" => NumericIntrinsicOp::Eq,
            "ne" => NumericIntrinsicOp::Ne,
            "lt" => NumericIntrinsicOp::Lt,
            "le" => NumericIntrinsicOp::Le,
            "gt" => NumericIntrinsicOp::Gt,
            "ge" => NumericIntrinsicOp::Ge,
            "bitnot" => NumericIntrinsicOp::BitNot,
            "not" => NumericIntrinsicOp::Not,
            "neg" => NumericIntrinsicOp::Neg,
            _ => return None,
        },
        prim,
    })
}

fn intrinsic_name_parts(callee_name: &str) -> Option<(&str, PrimTy)> {
    INTRINSIC_SUFFIX_TYPES.iter().find_map(|(suffix, prim)| {
        callee_name
            .strip_suffix(suffix)
            .and_then(|prefix| prefix.strip_prefix("__"))
            .map(|op| (op, *prim))
    })
}

#[derive(Clone, Copy)]
enum SlotRoot {
    Ptr(ValueId, Type),
    Object(ValueId, Type),
}

enum PlaceTerminal<'db> {
    Ptr {
        addr: ValueId,
        space: AddressSpaceKind,
        class: RuntimeClass<'db>,
    },
    Object {
        value: ValueId,
        class: RuntimeClass<'db>,
    },
    Const {
        value: ValueId,
        class: RuntimeClass<'db>,
    },
}

enum Lowered<T> {
    Value(T),
    Terminated,
}

#[derive(Clone)]
enum CopySource<'db> {
    Value {
        value: ValueId,
        class: RuntimeClass<'db>,
    },
    Object {
        value: ValueId,
        class: RuntimeClass<'db>,
    },
    Const {
        value: ValueId,
        class: RuntimeClass<'db>,
    },
    Ptr {
        addr: ValueId,
        space: AddressSpaceKind,
        class: RuntimeClass<'db>,
    },
}

struct FunctionLowerer<'ctx, 'db, 'a> {
    module: &'ctx mut ModuleLowerer<'db, 'a>,
    body: RuntimeBody<'db>,
    current_sections: Vec<mir::RuntimeSectionRef<'db>>,
    fb: FunctionBuilder<InstInserter>,
    prologue_block: BlockId,
    block_map: Vec<Option<BlockId>>,
    reachable_blocks: Vec<bool>,
    vars: FxHashMap<RLocalId, Variable>,
    slot_roots: FxHashMap<RLocalId, SlotRoot>,
    pending_enum_proof: Option<PendingEnumProof<'db>>,
    empty_revert_block: Option<BlockId>,
    overflow_panic_block: Option<BlockId>,
    division_by_zero_panic_block: Option<BlockId>,
}

#[derive(Clone, Copy)]
struct PendingEnumProof<'db> {
    // Sonatina proves value-enum extracts on a specific SSA value, so when the
    // runtime IR emits `EnumAssertVariant` as a standalone statement we must
    // reuse that exact materialization for the immediately following extract.
    local: RLocalId,
    variant: VariantId<'db>,
    value: ValueId,
}

impl<'ctx, 'db, 'a> FunctionLowerer<'ctx, 'db, 'a> {
    fn new(
        module: &'ctx mut ModuleLowerer<'db, 'a>,
        body: RuntimeBody<'db>,
        func_ref: FuncRef,
    ) -> Result<Self, LowerError> {
        let current_sections = module.sections_for_function(body.owner).to_vec();
        let mut fb = module.builder.func_builder::<InstInserter>(func_ref);
        let prologue_block = fb.append_block();
        let reachable_blocks = compute_reachable_blocks(&body);
        let block_map = reachable_blocks
            .iter()
            .map(|reachable| reachable.then(|| fb.append_block()))
            .collect::<Vec<_>>();
        let vars = body
            .locals
            .iter()
            .enumerate()
            .filter_map(|(idx, local)| match local.root {
                RuntimeLocalRoot::Slot(_) => None,
                RuntimeLocalRoot::None
                | RuntimeLocalRoot::Ref(_)
                | RuntimeLocalRoot::Ptr { .. } => match &local.carrier {
                    mir::RuntimeCarrier::Value(class) => Some(
                        module
                            .ty_for_class(class)
                            .map(|ty| (RLocalId::from_u32(idx as u32), fb.declare_var(ty))),
                    ),
                    mir::RuntimeCarrier::Erased => None,
                },
            })
            .collect::<Result<FxHashMap<_, _>, _>>()?;
        Ok(Self {
            module,
            body,
            current_sections,
            fb,
            prologue_block,
            block_map,
            reachable_blocks,
            vars,
            slot_roots: FxHashMap::default(),
            pending_enum_proof: None,
            empty_revert_block: None,
            overflow_panic_block: None,
            division_by_zero_panic_block: None,
        })
    }

    fn lower(mut self) -> Result<(), LowerError> {
        let entry_block = self.block_id(RBlockId::from_u32(0))?;
        self.fb.switch_to_block(self.prologue_block);
        self.initialize_locals().map_err(|err| {
            self.with_body_context(
                format!(
                    "while initializing locals for `{}`",
                    self.module.function_symbol(self.body.owner)
                ),
                None,
                None,
            )
            .wrap(err)
        })?;
        self.fb
            .insert_inst_no_result(Jump::new(self.module.inst_set(), entry_block));
        let blocks = self.body.blocks.clone();
        for (idx, block) in blocks.iter().enumerate() {
            if !self.reachable_blocks[idx] {
                continue;
            }
            self.fb
                .switch_to_block(self.block_id(RBlockId::from_u32(idx as u32))?);
            let mut terminated = false;
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                if matches!(
                    self.lower_stmt(stmt).map_err(|err| {
                        self.with_body_context(
                            format!(
                                "while lowering `{}` at bb{idx}[{stmt_idx}]",
                                self.module.function_symbol(self.body.owner)
                            ),
                            Some(RBlockId::from_u32(idx as u32)),
                            Some(stmt_idx),
                        )
                        .wrap(err)
                    })?,
                    Lowered::Terminated
                ) {
                    self.pending_enum_proof = None;
                    terminated = true;
                    break;
                }
            }
            if terminated {
                continue;
            }
            self.pending_enum_proof = None;
            self.lower_terminator(&block.terminator).map_err(|err| {
                self.with_body_context(
                    format!(
                        "while lowering `{}` terminator at bb{idx}",
                        self.module.function_symbol(self.body.owner)
                    ),
                    Some(RBlockId::from_u32(idx as u32)),
                    None,
                )
                .wrap(err)
            })?;
        }
        self.fb.seal_all();
        self.fb.finish();
        Ok(())
    }

    fn block_id(&self, block: RBlockId) -> Result<BlockId, LowerError> {
        self.block_map
            .get(block.as_u32() as usize)
            .copied()
            .flatten()
            .ok_or_else(|| {
                LowerError::Internal(format!(
                    "reachable runtime block {block:?} was not assigned a Sonatina block"
                ))
            })
    }

    fn with_body_context(
        &self,
        context: String,
        block: Option<RBlockId>,
        stmt: Option<usize>,
    ) -> LowerBodyContext<'_, 'db> {
        LowerBodyContext {
            db: self.module.db,
            body: &self.body,
            context,
            block,
            stmt,
        }
    }

    fn initialize_locals(&mut self) -> Result<(), LowerError> {
        let locals = self.body.locals.clone();
        for (idx, local) in locals.iter().enumerate() {
            let local_id = RLocalId::from_u32(idx as u32);
            match &local.root {
                RuntimeLocalRoot::None
                | RuntimeLocalRoot::Ref(_)
                | RuntimeLocalRoot::Ptr { .. } => {}
                RuntimeLocalRoot::Slot(class) => {
                    let class_ty = self.module.ty_for_class(class)?;
                    let root = match class {
                        RuntimeClass::AggregateValue { .. } => SlotRoot::Object(
                            self.fb.insert_inst(
                                ObjAlloc::new(self.module.inst_set(), class_ty),
                                self.fb.module_builder.objref_type(class_ty),
                            ),
                            class_ty,
                        ),
                        RuntimeClass::Scalar(_)
                        | RuntimeClass::Ref { .. }
                        | RuntimeClass::RawAddr { .. } => SlotRoot::Ptr(
                            {
                                let ptr_ty = self.fb.ptr_type(class_ty);
                                self.fb.insert_inst(
                                    Alloca::new(self.module.inst_set(), class_ty),
                                    ptr_ty,
                                )
                            },
                            class_ty,
                        ),
                    };
                    self.slot_roots.insert(local_id, root);
                }
            }
        }

        let params = self.body.signature.params.clone();
        for (idx, param) in params.iter().enumerate() {
            let local = param.local;
            let arg = self.body_signature_arg(idx)?;
            if self.slot_roots.contains_key(&local) {
                self.store_whole_local(local, arg)?;
            } else if let Some(&var) = self.vars.get(&local) {
                self.fb.def_var(var, arg);
            }
        }
        Ok(())
    }

    fn body_signature_arg(&self, idx: usize) -> Result<ValueId, LowerError> {
        self.fb
            .func
            .arg_values
            .get(idx)
            .copied()
            .ok_or_else(|| LowerError::Internal(format!("missing arg value {idx}")))
    }

    fn lower_stmt(&mut self, stmt: &RStmt<'db>) -> Result<Lowered<()>, LowerError> {
        match stmt {
            RStmt::Assign { dst, expr } => {
                let Lowered::Value(value) = self.lower_expr(expr, Some(*dst))? else {
                    self.pending_enum_proof = None;
                    return Ok(Lowered::Terminated);
                };
                self.assign_local(*dst, value)?;
                self.pending_enum_proof = None;
            }
            RStmt::EnumAssertVariant { value, variant } => {
                let materialized = self.local_value(*value)?;
                self.fb.insert_inst_no_result(EnumAssertVariant::new(
                    self.module.inst_set(),
                    materialized,
                    self.variant_ref(*variant)?,
                ));
                self.pending_enum_proof = Some(PendingEnumProof {
                    local: *value,
                    variant: *variant,
                    value: materialized,
                });
            }
            RStmt::Store { dst, src } => {
                let src = self.local_value(*src)?;
                if matches!(self.store_to_place(dst, src)?, Lowered::Terminated) {
                    self.pending_enum_proof = None;
                    return Ok(Lowered::Terminated);
                }
                self.pending_enum_proof = None;
            }
            RStmt::CopyInto { dst, src } => {
                if matches!(self.copy_into_place(dst, *src)?, Lowered::Terminated) {
                    self.pending_enum_proof = None;
                    return Ok(Lowered::Terminated);
                }
                self.pending_enum_proof = None;
            }
            RStmt::EnumSetTag { root, variant } => {
                let object = self.local_value(*root)?;
                self.fb.insert_inst_no_result(EnumSetTag::new(
                    self.module.inst_set(),
                    object,
                    self.variant_ref(*variant)?,
                ));
                self.pending_enum_proof = None;
            }
            RStmt::EnumWriteVariant {
                root,
                variant,
                fields,
            } => {
                let object = self.local_value(*root)?;
                let values = fields
                    .iter()
                    .map(|value| self.local_value(*value))
                    .collect::<Result<SmallVec<[ValueId; 2]>, _>>()?;
                self.fb.insert_inst_no_result(EnumWriteVariant::new(
                    self.module.inst_set(),
                    object,
                    self.variant_ref(*variant)?,
                    values,
                ));
                self.pending_enum_proof = None;
            }
        }
        Ok(Lowered::Value(()))
    }

    fn lower_expr(
        &mut self,
        expr: &RExpr<'db>,
        dst: Option<RLocalId>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        let value = match expr {
            RExpr::Use(value) => self.local_value(*value)?,
            RExpr::ConstScalar(value) => self.fb.make_imm_value(
                self.module
                    .immediate_for_const(value, dst.and_then(|dst| self.body.value_class(dst)))?,
            ),
            RExpr::Placeholder { class } => {
                zero_for_type(&mut self.fb, self.module.ty_for_class(class)?)
            }
            RExpr::Builtin(builtin) => {
                let value = self.lower_builtin(builtin)?;
                self.coerce_to_dst(value, dst)?
            }
            RExpr::Unary { op, value } => {
                let value = self.local_value(*value)?;
                let dst_class = dst
                    .and_then(|dst| self.body.value_class(dst).cloned())
                    .ok_or_else(|| {
                        LowerError::Internal("unary expr missing destination class".to_string())
                    })?;
                self.lower_unary(*op, value, &dst_class)?
            }
            RExpr::Binary { op, lhs, rhs } => {
                let lhs_class = self.body.value_class(*lhs).cloned().ok_or_else(|| {
                    LowerError::Internal("binary lhs missing runtime class".to_string())
                })?;
                let lhs = self.local_value(*lhs)?;
                let rhs = self.local_value(*rhs)?;
                self.lower_binary(*op, lhs, rhs, &lhs_class)?
            }
            RExpr::Cast { value, to } => {
                let value = self.local_value(*value)?;
                self.cast_scalar(value, scalar_ty(to))?
            }
            RExpr::ConstRef { region, .. } => {
                let gv = self.module.lower_const_region(*region)?;
                let gv_ty = gv.ty(&self.fb.module_builder.ctx);
                self.fb.insert_inst(
                    ConstRef::new(self.module.inst_set(), gv.into()),
                    self.fb.module_builder.constref_type(gv_ty),
                )
            }
            RExpr::AllocObject { layout } => {
                let layout_ty = self.module.ty_for_layout(*layout)?;
                self.fb.insert_inst(
                    ObjAlloc::new(self.module.inst_set(), layout_ty),
                    self.fb.module_builder.objref_type(layout_ty),
                )
            }
            RExpr::MaterializeToObject { src } => {
                let src_value = self.local_value(*src)?;
                let dst_local = dst.ok_or_else(|| {
                    LowerError::Internal("materialize-to-object missing destination".to_string())
                })?;
                let class = self.body.value_class(dst_local).ok_or_else(|| {
                    LowerError::Internal(
                        "materialize-to-object missing destination class".to_string(),
                    )
                })?;
                let RuntimeClass::Ref {
                    pointee,
                    kind: RefKind::Object,
                    ..
                } = class
                else {
                    return Err(LowerError::Internal(
                        "materialize-to-object destination is not an object ref".to_string(),
                    ));
                };
                let RuntimeClass::AggregateValue { layout } = **pointee else {
                    return Err(LowerError::Internal(
                        "materialize-to-object destination is not aggregate-backed".to_string(),
                    ));
                };
                let layout_ty = self.module.ty_for_layout(layout)?;
                let object = self.fb.insert_inst(
                    ObjAlloc::new(self.module.inst_set(), layout_ty),
                    self.fb.module_builder.objref_type(layout_ty),
                );
                let source = self.copy_source_for_local(*src, src_value)?;
                self.copy_source_into_object(
                    source,
                    &RuntimeClass::AggregateValue { layout },
                    object,
                )?;
                object
            }
            RExpr::MaterializePlaceToObject { place } => {
                return self.materialize_place_to_object(place, dst);
            }
            RExpr::ProviderFromRaw { raw, space, .. } => {
                let value = self.local_value(*raw)?;
                if *space == AddressSpaceKind::Memory {
                    return Err(LowerError::Unsupported(
                        "memory provider reconstruction from raw addresses is not supported"
                            .to_string(),
                    ));
                }
                value
            }
            RExpr::WordToRawAddr { value, .. } => self.local_value(*value)?,
            RExpr::ProviderToRaw { value } => self.local_value(*value)?,
            RExpr::RetagRef { value } => self.local_value(*value)?,
            RExpr::AddrOf { place } => return self.addr_of_place(place, dst),
            RExpr::Load { place } => return self.load_from_place(place),
            RExpr::Call { callee, args } => {
                if let Some(value) = self.lower_intrinsic_call(*callee, args, dst)? {
                    return Ok(Lowered::Value(value));
                }
                let callee_ref = self.module.func_ref(*callee)?;
                let args = args
                    .iter()
                    .map(|arg| self.local_value(*arg))
                    .collect::<Result<SmallVec<[ValueId; 8]>, _>>()?;
                let ret = callee.body(self.module.db).signature.ret.clone();
                match ret {
                    Some(class) => {
                        let ret_ty = self.module.ty_for_class(&class)?;
                        let value = self.fb.insert_inst(
                            Call::new(self.module.inst_set(), callee_ref, args),
                            ret_ty,
                        );
                        self.coerce_to_dst(value, dst)?
                    }
                    None => {
                        self.fb.insert_inst_no_result(Call::new(
                            self.module.inst_set(),
                            callee_ref,
                            args,
                        ));
                        zero_for_type(&mut self.fb, Type::Unit)
                    }
                }
            }
            RExpr::EnumMake {
                layout,
                variant,
                fields,
            } => {
                let values = fields
                    .iter()
                    .map(|field| self.local_value(*field))
                    .collect::<Result<SmallVec<[ValueId; 2]>, _>>()?;
                self.fb.insert_inst(
                    EnumMake::new(
                        self.module.inst_set(),
                        self.module.ty_for_layout(*layout)?,
                        self.variant_ref(*variant)?,
                        values,
                    ),
                    self.module.ty_for_layout(*layout)?,
                )
            }
            RExpr::EnumTagOfValue { value } => {
                let enum_value = self.local_value(*value)?;
                let ty = dst
                    .and_then(|dst| self.body.value_class(dst))
                    .map(|class| self.module.ty_for_class(class))
                    .transpose()?
                    .unwrap_or(Type::I256);
                self.fb
                    .insert_inst(EnumTag::new(self.module.inst_set(), enum_value), ty)
            }
            RExpr::EnumIsVariant { value, variant } => {
                let value = self.local_value(*value)?;
                let variant = self.variant_ref(*variant)?;
                self.fb.insert_inst(
                    EnumIsVariant::new(self.module.inst_set(), value, variant),
                    Type::I1,
                )
            }
            RExpr::EnumExtract {
                value,
                variant,
                field,
            } => {
                let value = self
                    .pending_enum_proof
                    .and_then(|proof| {
                        (proof.local == *value && proof.variant == *variant).then_some(proof.value)
                    })
                    .map(Ok)
                    .unwrap_or_else(|| self.local_value(*value))?;
                let variant = self.variant_ref(*variant)?;
                let field = self.index_value(field.0.into());
                let dst = dst.ok_or_else(|| {
                    LowerError::Internal("enum extract missing destination".to_string())
                })?;
                let ty = self.local_ty(dst)?;
                self.fb.insert_inst(
                    EnumExtract::new(self.module.inst_set(), value, variant, field),
                    ty,
                )
            }
            RExpr::EnumGetTag { root } => {
                let root = self.local_value(*root)?;
                let dst = dst.ok_or_else(|| {
                    LowerError::Internal("enum get-tag missing destination".to_string())
                })?;
                let ty = self.local_ty(dst)?;
                self.fb
                    .insert_inst(EnumGetTag::new(self.module.inst_set(), root), ty)
            }
            RExpr::EnumAssertVariantRef { root, variant } => {
                let root = self.local_value(*root)?;
                let variant = self.variant_ref(*variant)?;
                let dst = dst.ok_or_else(|| {
                    LowerError::Internal("enum assert missing destination".to_string())
                })?;
                let ty = self.local_ty(dst)?;
                self.fb.insert_inst(
                    EnumAssertVariantRef::new(self.module.inst_set(), root, variant),
                    ty,
                )
            }
        };
        Ok(Lowered::Value(value))
    }

    fn lower_builtin(&mut self, builtin: &RuntimeBuiltin<'db>) -> Result<ValueId, LowerError> {
        Ok(match builtin {
            RuntimeBuiltin::Mload { addr } => {
                let addr = self.local_value(*addr)?;
                self.fb.insert_inst(
                    Mload::new(self.module.inst_set(), addr, Type::I256),
                    Type::I256,
                )
            }
            RuntimeBuiltin::Mstore { addr, value } => {
                let addr = self.local_value(*addr)?;
                let value = self.local_value(*value)?;
                self.fb.insert_inst_no_result(Mstore::new(
                    self.module.inst_set(),
                    addr,
                    value,
                    Type::I256,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Mstore8 { addr, value } => {
                let addr = self.local_value(*addr)?;
                let value = self.local_value(*value)?;
                let value = self.cast_scalar(value, Type::I8)?;
                self.fb
                    .insert_inst_no_result(EvmMstore8::new(self.module.inst_set(), addr, value));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Mcopy { dst, src, len } => {
                let dst = self.local_value(*dst)?;
                let src = self.local_value(*src)?;
                let len = self.local_value(*len)?;
                self.fb
                    .insert_inst_no_result(EvmMcopy::new(self.module.inst_set(), dst, src, len));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Msize => self
                .fb
                .insert_inst(EvmMsize::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::Sload { slot } => {
                let slot = self.local_value(*slot)?;
                self.fb
                    .insert_inst(EvmSload::new(self.module.inst_set(), slot), Type::I256)
            }
            RuntimeBuiltin::Sstore { slot, value } => {
                let slot = self.local_value(*slot)?;
                let value = self.local_value(*value)?;
                self.fb
                    .insert_inst_no_result(EvmSstore::new(self.module.inst_set(), slot, value));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::CallValue => self
                .fb
                .insert_inst(EvmCallValue::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::ReturnDataSize => self
                .fb
                .insert_inst(EvmReturnDataSize::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::CallDataSize => self
                .fb
                .insert_inst(EvmCalldataSize::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::CallDataLoad { offset } => {
                let offset = self.local_value(*offset)?;
                self.fb.insert_inst(
                    EvmCalldataLoad::new(self.module.inst_set(), offset),
                    Type::I256,
                )
            }
            RuntimeBuiltin::ReturnDataCopy { dst, offset, len } => {
                let dst = self.local_value(*dst)?;
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb.insert_inst_no_result(EvmReturnDataCopy::new(
                    self.module.inst_set(),
                    dst,
                    offset,
                    len,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::CallDataCopy { dst, offset, len } => {
                let dst = self.local_value(*dst)?;
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb.insert_inst_no_result(EvmCalldataCopy::new(
                    self.module.inst_set(),
                    dst,
                    offset,
                    len,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::CodeSize => self
                .fb
                .insert_inst(EvmCodeSize::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::CodeCopy { dst, offset, len } => {
                let dst = self.local_value(*dst)?;
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb.insert_inst_no_result(EvmCodeCopy::new(
                    self.module.inst_set(),
                    dst,
                    offset,
                    len,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Keccak256 { offset, len } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb.insert_inst(
                    EvmKeccak256::new(self.module.inst_set(), offset, len),
                    Type::I256,
                )
            }
            RuntimeBuiltin::AddMod { lhs, rhs, modulus } => {
                let lhs = self.local_value(*lhs)?;
                let rhs = self.local_value(*rhs)?;
                let modulus = self.local_value(*modulus)?;
                self.fb.insert_inst(
                    EvmAddMod::new(self.module.inst_set(), lhs, rhs, modulus),
                    Type::I256,
                )
            }
            RuntimeBuiltin::MulMod { lhs, rhs, modulus } => {
                let lhs = self.local_value(*lhs)?;
                let rhs = self.local_value(*rhs)?;
                let modulus = self.local_value(*modulus)?;
                self.fb.insert_inst(
                    EvmMulMod::new(self.module.inst_set(), lhs, rhs, modulus),
                    Type::I256,
                )
            }
            RuntimeBuiltin::SignExtend { byte, value } => {
                let byte = self.local_value(*byte)?;
                let value = self.local_value(*value)?;
                self.fb.insert_inst(
                    EvmSignExtend::new(self.module.inst_set(), byte, value),
                    Type::I256,
                )
            }
            RuntimeBuiltin::IntrinsicArith {
                op,
                checked,
                lhs,
                rhs,
                class,
            } => self.lower_intrinsic_arith_builtin(*op, *checked, *lhs, *rhs, class)?,
            RuntimeBuiltin::Saturating {
                op,
                lhs,
                rhs,
                class,
            } => {
                let lhs = self.local_value(*lhs)?;
                let rhs = self.local_value(*rhs)?;
                let lhs = self.cast_scalar(lhs, scalar_ty(class))?;
                let rhs = self.cast_scalar(rhs, scalar_ty(class))?;
                let signed = matches!(class.repr, ScalarRepr::Int { signed: true, .. });
                match (op, signed) {
                    (SaturatingBinOp::Add, true) => self.fb.insert_saddsat(lhs, rhs),
                    (SaturatingBinOp::Add, false) => self.fb.insert_uaddsat(lhs, rhs),
                    (SaturatingBinOp::Sub, true) => self.fb.insert_ssubsat(lhs, rhs),
                    (SaturatingBinOp::Sub, false) => self.fb.insert_usubsat(lhs, rhs),
                    (SaturatingBinOp::Mul, true) => self.fb.insert_smulsat(lhs, rhs),
                    (SaturatingBinOp::Mul, false) => self.fb.insert_umulsat(lhs, rhs),
                }
            }
            RuntimeBuiltin::Address => self
                .fb
                .insert_inst(EvmAddress::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::Caller => self
                .fb
                .insert_inst(EvmCaller::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::Origin => self
                .fb
                .insert_inst(EvmOrigin::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::GasPrice => {
                return Err(LowerError::Unsupported(
                    "gasprice is not supported by the Sonatina backend".to_string(),
                ));
            }
            RuntimeBuiltin::CoinBase => self
                .fb
                .insert_inst(EvmCoinBase::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::Timestamp => self
                .fb
                .insert_inst(EvmTimestamp::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::Number => self
                .fb
                .insert_inst(EvmNumber::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::PrevRandao => self
                .fb
                .insert_inst(EvmPrevRandao::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::GasLimit => self
                .fb
                .insert_inst(EvmGasLimit::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::ChainId => self
                .fb
                .insert_inst(EvmChainId::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::BaseFee => self
                .fb
                .insert_inst(EvmBaseFee::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::SelfBalance => self
                .fb
                .insert_inst(EvmSelfBalance::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::BlockHash { block } => {
                let block = self.local_value(*block)?;
                self.fb
                    .insert_inst(EvmBlockHash::new(self.module.inst_set(), block), Type::I256)
            }
            RuntimeBuiltin::Gas => self
                .fb
                .insert_inst(EvmGas::new(self.module.inst_set()), Type::I256),
            RuntimeBuiltin::CurrentCodeRegionLen => self.fb.insert_inst(
                SymSize::new(self.module.inst_set(), SymbolRef::CurrentSection),
                Type::I256,
            ),
            RuntimeBuiltin::CodeRegionOffset { region } => self.fb.insert_inst(
                SymAddr::new(self.module.inst_set(), self.code_region_symbol_ref(*region)),
                Type::I256,
            ),
            RuntimeBuiltin::CodeRegionLen { region } => self.fb.insert_inst(
                SymSize::new(self.module.inst_set(), self.code_region_symbol_ref(*region)),
                Type::I256,
            ),
            RuntimeBuiltin::Malloc { size } => {
                let size = self.local_value(*size)?;
                let ptr_ty = self.fb.ptr_type(Type::I8);
                self.fb
                    .insert_inst(EvmMalloc::new(self.module.inst_set(), size), ptr_ty)
            }
            RuntimeBuiltin::Call {
                gas,
                addr,
                value,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                let gas = self.local_value(*gas)?;
                let addr = self.local_value(*addr)?;
                let value = self.local_value(*value)?;
                let args_offset = self.local_value(*args_offset)?;
                let args_len = self.local_value(*args_len)?;
                let ret_offset = self.local_value(*ret_offset)?;
                let ret_len = self.local_value(*ret_len)?;
                self.fb.insert_inst(
                    EvmCall::new(
                        self.module.inst_set(),
                        gas,
                        addr,
                        value,
                        args_offset,
                        args_len,
                        ret_offset,
                        ret_len,
                    ),
                    Type::I256,
                )
            }
            RuntimeBuiltin::StaticCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                let gas = self.local_value(*gas)?;
                let addr = self.local_value(*addr)?;
                let args_offset = self.local_value(*args_offset)?;
                let args_len = self.local_value(*args_len)?;
                let ret_offset = self.local_value(*ret_offset)?;
                let ret_len = self.local_value(*ret_len)?;
                self.fb.insert_inst(
                    EvmStaticCall::new(
                        self.module.inst_set(),
                        gas,
                        addr,
                        args_offset,
                        args_len,
                        ret_offset,
                        ret_len,
                    ),
                    Type::I256,
                )
            }
            RuntimeBuiltin::DelegateCall {
                gas,
                addr,
                args_offset,
                args_len,
                ret_offset,
                ret_len,
            } => {
                let gas = self.local_value(*gas)?;
                let addr = self.local_value(*addr)?;
                let args_offset = self.local_value(*args_offset)?;
                let args_len = self.local_value(*args_len)?;
                let ret_offset = self.local_value(*ret_offset)?;
                let ret_len = self.local_value(*ret_len)?;
                self.fb.insert_inst(
                    EvmDelegateCall::new(
                        self.module.inst_set(),
                        gas,
                        addr,
                        args_offset,
                        args_len,
                        ret_offset,
                        ret_len,
                    ),
                    Type::I256,
                )
            }
            RuntimeBuiltin::Create { value, offset, len } => {
                let value = self.local_value(*value)?;
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb.insert_inst(
                    EvmCreate::new(self.module.inst_set(), value, offset, len),
                    Type::I256,
                )
            }
            RuntimeBuiltin::Create2 {
                value,
                offset,
                len,
                salt,
            } => {
                let value = self.local_value(*value)?;
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                let salt = self.local_value(*salt)?;
                self.fb.insert_inst(
                    EvmCreate2::new(self.module.inst_set(), value, offset, len, salt),
                    Type::I256,
                )
            }
            RuntimeBuiltin::Log0 { offset, len } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb
                    .insert_inst_no_result(EvmLog0::new(self.module.inst_set(), offset, len));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Log1 {
                offset,
                len,
                topic0,
            } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                let topic0 = self.local_value(*topic0)?;
                self.fb.insert_inst_no_result(EvmLog1::new(
                    self.module.inst_set(),
                    offset,
                    len,
                    topic0,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Log2 {
                offset,
                len,
                topic0,
                topic1,
            } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                let topic0 = self.local_value(*topic0)?;
                let topic1 = self.local_value(*topic1)?;
                self.fb.insert_inst_no_result(EvmLog2::new(
                    self.module.inst_set(),
                    offset,
                    len,
                    topic0,
                    topic1,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Log3 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
            } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                let topic0 = self.local_value(*topic0)?;
                let topic1 = self.local_value(*topic1)?;
                let topic2 = self.local_value(*topic2)?;
                self.fb.insert_inst_no_result(EvmLog3::new(
                    self.module.inst_set(),
                    offset,
                    len,
                    topic0,
                    topic1,
                    topic2,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::Log4 {
                offset,
                len,
                topic0,
                topic1,
                topic2,
                topic3,
            } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                let topic0 = self.local_value(*topic0)?;
                let topic1 = self.local_value(*topic1)?;
                let topic2 = self.local_value(*topic2)?;
                let topic3 = self.local_value(*topic3)?;
                self.fb.insert_inst_no_result(EvmLog4::new(
                    self.module.inst_set(),
                    offset,
                    len,
                    topic0,
                    topic1,
                    topic2,
                    topic3,
                ));
                zero_for_type(&mut self.fb, Type::Unit)
            }
            RuntimeBuiltin::CallDataSelector => {
                let zero = self.fb.make_imm_value(I256::zero());
                let word = self.fb.insert_inst(
                    EvmCalldataLoad::new(self.module.inst_set(), zero),
                    Type::I256,
                );
                let shift = self.fb.make_imm_value(I256::from(224u64));
                self.fb
                    .insert_inst(Shr::new(self.module.inst_set(), shift, word), Type::I256)
            }
            RuntimeBuiltin::MakeContractFieldRef { slot, class, .. } => {
                if matches!(
                    class,
                    RuntimeClass::Ref {
                        kind: RefKind::Provider {
                            space: AddressSpaceKind::Memory,
                            ..
                        },
                        ..
                    }
                ) {
                    return Err(LowerError::Unsupported(
                        "memory contract field handles are not supported".to_string(),
                    ));
                }
                self.fb.make_imm_value(I256::from(*slot))
            }
        })
    }

    fn code_region_symbol_ref(&self, region: mir::RuntimeCodeRegion<'db>) -> SymbolRef {
        if !self.current_sections.is_empty()
            && self
                .module
                .package
                .code_regions(self.module.db)
                .iter()
                .find(|resolved| resolved.region(self.module.db) == region)
                .is_some_and(|resolved| {
                    self.current_sections.iter().all(|current_section| {
                        runtime_section_refs_match(
                            self.module.db,
                            &resolved.source(self.module.db),
                            current_section,
                        )
                    })
                })
        {
            SymbolRef::CurrentSection
        } else {
            SymbolRef::Embed(EmbedSymbol::from(code_region_symbol(
                self.module.db,
                self.module.package,
                region,
            )))
        }
    }

    fn lower_intrinsic_call(
        &mut self,
        callee: mir::RuntimeInstance<'db>,
        args: &[RLocalId],
        dst: Option<RLocalId>,
    ) -> Result<Option<ValueId>, LowerError> {
        let Some(intrinsic) = runtime_intrinsic(self.module.db, callee) else {
            return Ok(None);
        };
        let value = match intrinsic {
            RuntimeIntrinsic::Alloc => {
                let [size] = args else {
                    return Err(LowerError::Internal(
                        "alloc requires 1 argument".to_string(),
                    ));
                };
                let size = self.local_value(*size)?;
                let ptr_ty = self.fb.ptr_type(Type::I8);
                self.fb
                    .insert_inst(EvmMalloc::new(self.module.inst_set(), size), ptr_ty)
            }
            RuntimeIntrinsic::GenericSaturating { op, ty } => {
                let prim = intrinsic_prim_from_ty(self.module.db, ty)?;
                let op_ty = intrinsic_value_type(prim);
                let signed = prim_is_signed(prim);
                let (lhs, rhs) = intrinsic_binary_args(self, args)?;
                let lhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), lhs, prim, op_ty);
                let rhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), rhs, prim, op_ty);
                match (op, signed) {
                    (GenericSaturatingOp::Add, true) => self.fb.insert_saddsat(lhs, rhs),
                    (GenericSaturatingOp::Add, false) => self.fb.insert_uaddsat(lhs, rhs),
                    (GenericSaturatingOp::Sub, true) => self.fb.insert_ssubsat(lhs, rhs),
                    (GenericSaturatingOp::Sub, false) => self.fb.insert_usubsat(lhs, rhs),
                    (GenericSaturatingOp::Mul, true) => self.fb.insert_smulsat(lhs, rhs),
                    (GenericSaturatingOp::Mul, false) => self.fb.insert_umulsat(lhs, rhs),
                }
            }
            RuntimeIntrinsic::Numeric { op, prim } => {
                self.lower_numeric_intrinsic_call(op, prim, args)?
            }
        };
        Ok(Some(self.coerce_to_dst(value, dst)?))
    }

    fn lower_numeric_intrinsic_call(
        &mut self,
        op: NumericIntrinsicOp,
        prim: PrimTy,
        args: &[RLocalId],
    ) -> Result<ValueId, LowerError> {
        let op_ty = intrinsic_value_type(prim);
        let signed = prim_is_signed(prim);
        Ok(match op {
            NumericIntrinsicOp::Eq
            | NumericIntrinsicOp::Ne
            | NumericIntrinsicOp::Lt
            | NumericIntrinsicOp::Le
            | NumericIntrinsicOp::Gt
            | NumericIntrinsicOp::Ge => {
                let (lhs, rhs) = intrinsic_binary_args(self, args)?;
                let lhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), lhs, prim, op_ty);
                let rhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), rhs, prim, op_ty);
                match op {
                    NumericIntrinsicOp::Eq => self
                        .fb
                        .insert_inst(Eq::new(self.module.inst_set(), lhs, rhs), Type::I1),
                    NumericIntrinsicOp::Ne => {
                        let eq = self
                            .fb
                            .insert_inst(Eq::new(self.module.inst_set(), lhs, rhs), Type::I1);
                        self.fb
                            .insert_inst(IsZero::new(self.module.inst_set(), eq), Type::I1)
                    }
                    NumericIntrinsicOp::Lt => {
                        if signed {
                            self.fb
                                .insert_inst(Slt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        } else {
                            self.fb
                                .insert_inst(Lt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        }
                    }
                    NumericIntrinsicOp::Le => {
                        let gt = if signed {
                            self.fb
                                .insert_inst(Slt::new(self.module.inst_set(), rhs, lhs), Type::I1)
                        } else {
                            self.fb
                                .insert_inst(Gt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        };
                        self.fb
                            .insert_inst(IsZero::new(self.module.inst_set(), gt), Type::I1)
                    }
                    NumericIntrinsicOp::Gt => {
                        if signed {
                            self.fb
                                .insert_inst(Slt::new(self.module.inst_set(), rhs, lhs), Type::I1)
                        } else {
                            self.fb
                                .insert_inst(Gt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        }
                    }
                    NumericIntrinsicOp::Ge => {
                        let lt = if signed {
                            self.fb
                                .insert_inst(Slt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        } else {
                            self.fb
                                .insert_inst(Lt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                        };
                        self.fb
                            .insert_inst(IsZero::new(self.module.inst_set(), lt), Type::I1)
                    }
                    _ => unreachable!(),
                }
            }
            NumericIntrinsicOp::Add
            | NumericIntrinsicOp::Sub
            | NumericIntrinsicOp::Mul
            | NumericIntrinsicOp::Pow
            | NumericIntrinsicOp::Shl
            | NumericIntrinsicOp::Shr
            | NumericIntrinsicOp::BitAnd
            | NumericIntrinsicOp::BitOr
            | NumericIntrinsicOp::BitXor
            | NumericIntrinsicOp::Div
            | NumericIntrinsicOp::Rem => {
                let (lhs, rhs) = intrinsic_binary_args(self, args)?;
                let lhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), lhs, prim, op_ty);
                let rhs =
                    lower_intrinsic_operand(&mut self.fb, self.module.inst_set(), rhs, prim, op_ty);
                match op {
                    NumericIntrinsicOp::Add => self
                        .fb
                        .insert_inst(Add::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::Sub => self
                        .fb
                        .insert_inst(Sub::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::Mul => self
                        .fb
                        .insert_inst(Mul::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::Pow => self
                        .fb
                        .insert_inst(EvmExp::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::Shl => self
                        .fb
                        .insert_inst(Shl::new(self.module.inst_set(), rhs, lhs), op_ty),
                    NumericIntrinsicOp::Shr => {
                        if signed {
                            self.fb
                                .insert_inst(Sar::new(self.module.inst_set(), rhs, lhs), op_ty)
                        } else {
                            self.fb
                                .insert_inst(Shr::new(self.module.inst_set(), rhs, lhs), op_ty)
                        }
                    }
                    NumericIntrinsicOp::BitAnd => self
                        .fb
                        .insert_inst(And::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::BitOr => self
                        .fb
                        .insert_inst(Or::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::BitXor => self
                        .fb
                        .insert_inst(Xor::new(self.module.inst_set(), lhs, rhs), op_ty),
                    NumericIntrinsicOp::Div => {
                        let [raw, _overflow] = if signed {
                            self.fb.insert_evm_sdivo(lhs, rhs)
                        } else {
                            self.fb.insert_evm_udivo(lhs, rhs)
                        };
                        raw
                    }
                    NumericIntrinsicOp::Rem => {
                        let [raw, _overflow] = if signed {
                            self.fb.insert_evm_smodo(lhs, rhs)
                        } else {
                            self.fb.insert_evm_umodo(lhs, rhs)
                        };
                        raw
                    }
                    _ => unreachable!(),
                }
            }
            NumericIntrinsicOp::BitNot | NumericIntrinsicOp::Not | NumericIntrinsicOp::Neg => {
                let value = intrinsic_unary_arg(self, args)?;
                let value = lower_intrinsic_operand(
                    &mut self.fb,
                    self.module.inst_set(),
                    value,
                    prim,
                    op_ty,
                );
                match op {
                    NumericIntrinsicOp::BitNot => self
                        .fb
                        .insert_inst(Not::new(self.module.inst_set(), value), op_ty),
                    NumericIntrinsicOp::Not => self
                        .fb
                        .insert_inst(IsZero::new(self.module.inst_set(), value), Type::I1),
                    NumericIntrinsicOp::Neg => self
                        .fb
                        .insert_inst(Neg::new(self.module.inst_set(), value), op_ty),
                    _ => unreachable!(),
                }
            }
        })
    }

    fn lower_terminator(&mut self, terminator: &RTerminator<'db>) -> Result<(), LowerError> {
        match terminator {
            RTerminator::Goto(block) => {
                self.fb.insert_inst_no_result(Jump::new(
                    self.module.inst_set(),
                    self.block_id(*block)?,
                ));
            }
            RTerminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond = self.local_value(*cond)?;
                let cond = condition_to_i1(&mut self.fb, cond, self.module.inst_set());
                self.fb.insert_inst_no_result(Br::new(
                    self.module.inst_set(),
                    cond,
                    self.block_id(*then_bb)?,
                    self.block_id(*else_bb)?,
                ));
            }
            RTerminator::SwitchScalar {
                discr,
                cases,
                default,
            } => {
                let discr = self.local_value(*discr)?;
                let table = cases
                    .iter()
                    .map(|(value, block)| {
                        Ok((
                            self.fb
                                .make_imm_value(self.module.immediate_for_const(value, None)?),
                            self.block_id(*block)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                self.fb.insert_inst_no_result(BrTable::new(
                    self.module.inst_set(),
                    discr,
                    Some(self.block_id(*default)?),
                    table,
                ));
            }
            RTerminator::MatchEnumTag { cases, default, .. } => {
                let (tag, enum_layout) = match terminator {
                    RTerminator::MatchEnumTag {
                        tag, enum_layout, ..
                    } => (self.local_value(*tag)?, *enum_layout),
                    _ => unreachable!(),
                };
                let table = cases
                    .iter()
                    .map(|(variant, block)| {
                        Ok((
                            self.fb.make_imm_value(
                                self.module.enum_tag_immediate(enum_layout, variant.index)?,
                            ),
                            self.block_id(*block)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                self.fb.insert_inst_no_result(BrTable::new(
                    self.module.inst_set(),
                    tag,
                    default.map(|block| self.block_id(block)).transpose()?,
                    table,
                ));
            }
            RTerminator::TerminalCall { callee, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.local_value(*arg))
                    .collect::<Result<SmallVec<[ValueId; 8]>, _>>()?;
                self.fb.insert_inst_no_result(Call::new(
                    self.module.inst_set(),
                    self.module.func_ref(*callee)?,
                    args,
                ));
                self.fb
                    .insert_inst_no_result(Unreachable::new_unchecked(self.module.inst_set()));
            }
            RTerminator::ReturnData { offset, len } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb
                    .insert_inst_no_result(EvmReturn::new(self.module.inst_set(), offset, len));
            }
            RTerminator::Revert { offset, len } => {
                let offset = self.local_value(*offset)?;
                let len = self.local_value(*len)?;
                self.fb
                    .insert_inst_no_result(EvmRevert::new(self.module.inst_set(), offset, len));
            }
            RTerminator::SelfDestruct { beneficiary } => {
                let beneficiary = self.local_value(*beneficiary)?;
                self.fb.insert_inst_no_result(EvmSelfDestruct::new(
                    self.module.inst_set(),
                    beneficiary,
                ));
            }
            RTerminator::Trap => {
                self.fb
                    .insert_inst_no_result(EvmInvalid::new(self.module.inst_set()));
            }
            RTerminator::Return(value) => match value {
                Some(value) => {
                    let value = self.local_value(*value)?;
                    self.fb
                        .insert_inst_no_result(Return::new_single(self.module.inst_set(), value))
                }
                None => self
                    .fb
                    .insert_inst_no_result(Return::new_unit(self.module.inst_set())),
            },
            RTerminator::Stop => {
                self.fb
                    .insert_inst_no_result(EvmStop::new(self.module.inst_set()));
            }
        }
        Ok(())
    }

    fn assign_local(&mut self, local: RLocalId, value: ValueId) -> Result<(), LowerError> {
        if self.slot_roots.contains_key(&local) {
            self.store_whole_local(local, value)
        } else if let Some(&var) = self.vars.get(&local) {
            let var_ty = self
                .body
                .value_class(local)
                .map(|class| self.module.ty_for_class(class))
                .transpose()?
                .ok_or_else(|| {
                    LowerError::Internal(format!("missing runtime class for {local:?}"))
                })?;
            let value = self.coerce_value_to_ty(value, var_ty)?;
            self.fb.def_var(var, value);
            Ok(())
        } else {
            Ok(())
        }
    }

    fn store_whole_local(&mut self, local: RLocalId, value: ValueId) -> Result<(), LowerError> {
        match self.slot_roots.get(&local).copied() {
            Some(SlotRoot::Ptr(ptr, ty)) => {
                let value = self.coerce_value_to_ty(value, ty)?;
                self.fb
                    .insert_inst_no_result(Mstore::new(self.module.inst_set(), ptr, value, ty));
                Ok(())
            }
            Some(SlotRoot::Object(object, _)) => {
                let class = self.body.value_class(local).cloned().ok_or_else(|| {
                    LowerError::Internal(format!("missing runtime class for {local:?}"))
                })?;
                self.copy_source_into_object(
                    CopySource::Value {
                        value,
                        class: class.clone(),
                    },
                    &class,
                    object,
                )
            }
            None => Err(LowerError::Internal(format!(
                "missing slot root for {local:?}"
            ))),
        }
    }

    fn local_value(&mut self, local: RLocalId) -> Result<ValueId, LowerError> {
        if let Some(root) = self.slot_roots.get(&local) {
            return match root {
                SlotRoot::Ptr(ptr, ty) => Ok(self
                    .fb
                    .insert_inst(Mload::new(self.module.inst_set(), *ptr, *ty), *ty)),
                SlotRoot::Object(object, ty) => Ok(self
                    .fb
                    .insert_inst(ObjLoad::new(self.module.inst_set(), *object), *ty)),
            };
        }
        let var = self
            .vars
            .get(&local)
            .copied()
            .ok_or_else(|| LowerError::Internal(format!("missing variable for {local:?}")))?;
        Ok(self.fb.use_var(var))
    }

    fn local_ty(&mut self, local: RLocalId) -> Result<Type, LowerError> {
        let class = self
            .body
            .value_class(local)
            .ok_or_else(|| LowerError::Internal(format!("erased local {local:?} has no type")))?;
        self.module.ty_for_class(class)
    }

    fn place_terminal_for_carrier(
        &mut self,
        value: RLocalId,
        carrier_class: RuntimeClass<'db>,
        class: RuntimeClass<'db>,
        allow_value_carrier: bool,
        root_kind: &str,
    ) -> Result<PlaceTerminal<'db>, LowerError> {
        match carrier_class {
            RuntimeClass::Ref {
                kind: RefKind::Const,
                ..
            } => Ok(PlaceTerminal::Const {
                value: self.local_value(value)?,
                class,
            }),
            RuntimeClass::Ref {
                kind: RefKind::Object,
                ..
            }
            | RuntimeClass::Ref {
                kind:
                    RefKind::Provider {
                        space: AddressSpaceKind::Memory,
                        ..
                    },
                ..
            } => Ok(PlaceTerminal::Object {
                value: self.local_value(value)?,
                class,
            }),
            RuntimeClass::Ref {
                kind: RefKind::Provider { space, .. },
                ..
            } => Ok(PlaceTerminal::Ptr {
                addr: self.local_value(value)?,
                space,
                class,
            }),
            RuntimeClass::AggregateValue { .. } if allow_value_carrier => {
                Ok(PlaceTerminal::Object {
                    value: self.local_value(value)?,
                    class,
                })
            }
            RuntimeClass::RawAddr { space, .. } if allow_value_carrier => Ok(PlaceTerminal::Ptr {
                addr: self.local_value(value)?,
                space,
                class,
            }),
            RuntimeClass::Scalar(_)
            | RuntimeClass::AggregateValue { .. }
            | RuntimeClass::RawAddr { .. } => Err(LowerError::Internal(format!(
                "{root_kind} root did not lower to a supported place carrier"
            ))),
        }
    }

    fn place_terminal_from_loaded_carrier(
        &mut self,
        value: ValueId,
        carrier_class: &RuntimeClass<'db>,
    ) -> Result<PlaceTerminal<'db>, LowerError> {
        match carrier_class {
            RuntimeClass::Ref {
                kind: RefKind::Const,
                pointee,
                ..
            } => {
                let RuntimeClass::AggregateValue { .. } = &**pointee else {
                    return Err(LowerError::Internal(
                        "const carrier follow requires aggregate pointee".to_string(),
                    ));
                };
                Ok(PlaceTerminal::Const {
                    value,
                    class: (**pointee).clone(),
                })
            }
            RuntimeClass::Ref {
                kind: RefKind::Object,
                pointee,
                ..
            }
            | RuntimeClass::Ref {
                kind:
                    RefKind::Provider {
                        space: AddressSpaceKind::Memory,
                        ..
                    },
                pointee,
                ..
            } => Ok(PlaceTerminal::Object {
                value,
                class: (**pointee).clone(),
            }),
            RuntimeClass::Ref {
                kind: RefKind::Provider { space, .. },
                pointee,
                ..
            } => Ok(PlaceTerminal::Ptr {
                addr: value,
                space: *space,
                class: (**pointee).clone(),
            }),
            RuntimeClass::RawAddr {
                space,
                target: Some(layout),
            } => Ok(PlaceTerminal::Ptr {
                addr: value,
                space: *space,
                class: RuntimeClass::AggregateValue { layout: *layout },
            }),
            RuntimeClass::RawAddr { target: None, .. } => Err(LowerError::Unsupported(
                "cannot continue projection through an opaque raw-address carrier".to_string(),
            )),
            RuntimeClass::Scalar(_) | RuntimeClass::AggregateValue { .. } => {
                Err(LowerError::Internal(
                    "attempted to follow a non-carrier projected field".to_string(),
                ))
            }
        }
    }

    fn load_terminal_value(
        &mut self,
        terminal: &PlaceTerminal<'db>,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        match terminal {
            PlaceTerminal::Object { value, .. } => Ok(self.fb.insert_inst(
                ObjLoad::new(self.module.inst_set(), *value),
                self.module.ty_for_class(class)?,
            )),
            PlaceTerminal::Const { value, .. } => Ok(self.fb.insert_inst(
                ConstLoad::new(self.module.inst_set(), *value),
                self.module.ty_for_class(class)?,
            )),
            PlaceTerminal::Ptr { addr, space, .. } => self.load_from_ptr(*addr, *space, class),
        }
    }

    fn resolve_place(
        &mut self,
        place: &RuntimePlace<'db>,
    ) -> Result<Lowered<PlaceTerminal<'db>>, LowerError> {
        let program = self.module.db as &dyn mir::MirDb;
        let resolved = resolve_runtime_place(self.module.db, &program, &self.body, place)
            .map_err(|err| LowerError::Internal(format!("invalid runtime place: {err:?}")))?;
        let mut terminal = match resolved.root_kind {
            ResolvedPlaceRootKind::Slot { local, class } => {
                match self.slot_roots.get(&local).ok_or_else(|| {
                    LowerError::Internal(format!("missing slot root for {local:?}"))
                })? {
                    SlotRoot::Ptr(ptr, _) => PlaceTerminal::Ptr {
                        addr: *ptr,
                        space: AddressSpaceKind::Memory,
                        class,
                    },
                    SlotRoot::Object(value, _) => PlaceTerminal::Object {
                        value: *value,
                        class,
                    },
                }
            }
            ResolvedPlaceRootKind::Ref { value, class } => self.place_terminal_for_carrier(
                value,
                self.body
                    .value_class(value)
                    .cloned()
                    .ok_or_else(|| LowerError::Internal(format!("erased handle root {value:?}")))?,
                class,
                false,
                "ref",
            )?,
            ResolvedPlaceRootKind::Provider {
                value,
                provider_class,
                class,
                ..
            } => self.place_terminal_for_carrier(value, provider_class, class, true, "provider")?,
            ResolvedPlaceRootKind::Ptr { addr, space, class } => PlaceTerminal::Ptr {
                addr: self.local_value(addr)?,
                space,
                class,
            },
        };

        for elem in resolved.path.iter() {
            terminal = match (terminal, elem) {
                (
                    PlaceTerminal::Object { value, .. },
                    ResolvedPlaceElem::Field { field, class },
                ) => {
                    let idx = self.index_value(field.0.into());
                    PlaceTerminal::Object {
                        value: self.fb.insert_inst(
                            ObjProj::new(self.module.inst_set(), smallvec![value, idx]),
                            self.module.ty_for_object_projection(class)?,
                        ),
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Object {
                        value,
                        class: base_class,
                    },
                    ResolvedPlaceElem::Index { index, class },
                ) => {
                    let Lowered::Value(index) = self.checked_index_value(&base_class, index)?
                    else {
                        return Ok(Lowered::Terminated);
                    };
                    PlaceTerminal::Object {
                        value: self.fb.insert_inst(
                            ObjIndex::new(self.module.inst_set(), value, index),
                            self.module.ty_for_object_projection(class)?,
                        ),
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Object { value, .. },
                    ResolvedPlaceElem::VariantField {
                        variant,
                        field,
                        class,
                    },
                ) => {
                    let variant_ref = self.variant_ref(*variant)?;
                    let field = self.index_value(field.0.into());
                    let value = self.fb.insert_inst(
                        EnumAssertVariantRef::new(self.module.inst_set(), value, variant_ref),
                        self.fb.type_of(value),
                    );
                    PlaceTerminal::Object {
                        value: self.fb.insert_inst(
                            EnumProj::new(self.module.inst_set(), value, variant_ref, field),
                            self.module.ty_for_object_projection(class)?,
                        ),
                        class: class.clone(),
                    }
                }
                (PlaceTerminal::Const { value, .. }, ResolvedPlaceElem::Field { field, class }) => {
                    let idx = self.index_value(field.0.into());
                    PlaceTerminal::Const {
                        value: self.fb.insert_inst(
                            ConstProj::new(self.module.inst_set(), smallvec![value, idx]),
                            self.module.ty_for_const_projection(class)?,
                        ),
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Const {
                        value,
                        class: base_class,
                    },
                    ResolvedPlaceElem::Index { index, class },
                ) => {
                    let Lowered::Value(index) = self.checked_index_value(&base_class, index)?
                    else {
                        return Ok(Lowered::Terminated);
                    };
                    PlaceTerminal::Const {
                        value: self.fb.insert_inst(
                            ConstIndex::new(self.module.inst_set(), value, index),
                            self.module.ty_for_const_projection(class)?,
                        ),
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Ptr {
                        addr,
                        space,
                        class: base_class,
                    },
                    ResolvedPlaceElem::Field { field, class },
                ) => {
                    let offset = base_class
                        .field_offset_words(self.module.db, *field)
                        .ok_or_else(|| {
                            LowerError::Internal("field projection on non-struct class".to_string())
                        })?;
                    PlaceTerminal::Ptr {
                        addr: self.offset_address(addr, offset, space)?,
                        space,
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Ptr {
                        addr,
                        space,
                        class: base_class,
                    },
                    ResolvedPlaceElem::Index { index, class },
                ) => {
                    let span = base_class
                        .index_stride_words(self.module.db)
                        .ok_or_else(|| {
                            LowerError::Internal("index projection on non-array class".to_string())
                        })?;
                    let Lowered::Value(idx) = self.checked_index_value(&base_class, index)? else {
                        return Ok(Lowered::Terminated);
                    };
                    let scale = self.scale_for_space(space, span);
                    let scaled = if scale == 1 {
                        idx
                    } else {
                        let scale = self.index_value(scale);
                        self.fb
                            .insert_inst(Mul::new(self.module.inst_set(), idx, scale), Type::I256)
                    };
                    PlaceTerminal::Ptr {
                        addr: self.fb.insert_inst(
                            Add::new(self.module.inst_set(), addr, scaled),
                            Type::I256,
                        ),
                        space,
                        class: class.clone(),
                    }
                }
                (
                    PlaceTerminal::Ptr { addr, space, .. },
                    ResolvedPlaceElem::VariantField {
                        variant,
                        field,
                        class,
                    },
                ) => PlaceTerminal::Ptr {
                    addr: self.offset_address(
                        addr,
                        variant
                            .field_offset_words(self.module.db, *field)
                            .ok_or_else(|| {
                                LowerError::Internal("variant field layout missing".to_string())
                            })?,
                        space,
                    )?,
                    space,
                    class: class.clone(),
                },
                (terminal, ResolvedPlaceElem::Deref { carrier_class, .. }) => {
                    let value = self.load_terminal_value(&terminal, carrier_class)?;
                    self.place_terminal_from_loaded_carrier(value, carrier_class)?
                }
                (terminal, elem) => {
                    return Err(LowerError::Unsupported(format!(
                        "unsupported place projection terminal `{terminal_kind}` with `{elem:?}`",
                        terminal_kind = match terminal {
                            PlaceTerminal::Ptr { .. } => "ptr",
                            PlaceTerminal::Object { .. } => "object",
                            PlaceTerminal::Const { .. } => "const",
                        }
                    )));
                }
            };
        }
        Ok(Lowered::Value(terminal))
    }

    fn load_from_place(
        &mut self,
        place: &RuntimePlace<'db>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        let program = self.module.db as &dyn mir::MirDb;
        let class = resolve_runtime_place(self.module.db, &program, &self.body, place)
            .map_err(|err| LowerError::Internal(format!("invalid runtime place: {err:?}")))?
            .result_class;
        let Lowered::Value(terminal) = self.resolve_place(place)? else {
            return Ok(Lowered::Terminated);
        };
        Ok(Lowered::Value(match terminal {
            PlaceTerminal::Object { value, .. } => self.fb.insert_inst(
                ObjLoad::new(self.module.inst_set(), value),
                self.module.ty_for_class(&class)?,
            ),
            PlaceTerminal::Const { value, .. } => self.fb.insert_inst(
                ConstLoad::new(self.module.inst_set(), value),
                self.module.ty_for_class(&class)?,
            ),
            PlaceTerminal::Ptr { addr, space, class } => self.load_from_ptr(addr, space, &class)?,
        }))
    }

    fn materialize_place_to_object(
        &mut self,
        place: &RuntimePlace<'db>,
        dst: Option<RLocalId>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        let dst_local = dst.ok_or_else(|| {
            LowerError::Internal("materialize-place-to-object missing destination".to_string())
        })?;
        let class = self.body.value_class(dst_local).ok_or_else(|| {
            LowerError::Internal(
                "materialize-place-to-object missing destination class".to_string(),
            )
        })?;
        let RuntimeClass::Ref {
            pointee,
            kind: RefKind::Object,
            ..
        } = class
        else {
            return Err(LowerError::Internal(
                "materialize-place-to-object destination is not an object ref".to_string(),
            ));
        };
        let RuntimeClass::AggregateValue { layout } = **pointee else {
            return Err(LowerError::Internal(
                "materialize-place-to-object destination is not aggregate-backed".to_string(),
            ));
        };
        let layout_ty = self.module.ty_for_layout(layout)?;
        let object = self.fb.insert_inst(
            ObjAlloc::new(self.module.inst_set(), layout_ty),
            self.fb.module_builder.objref_type(layout_ty),
        );
        let Lowered::Value(terminal) = self.resolve_place(place)? else {
            return Ok(Lowered::Terminated);
        };
        let source = self.copy_source_for_terminal(terminal);
        self.copy_source_into_object(source, &RuntimeClass::AggregateValue { layout }, object)?;
        Ok(Lowered::Value(object))
    }

    fn copy_source_for_local(
        &self,
        local: RLocalId,
        value: ValueId,
    ) -> Result<CopySource<'db>, LowerError> {
        let class = self.body.value_class(local).cloned().ok_or_else(|| {
            LowerError::Internal(format!("missing runtime class for local {local:?}"))
        })?;
        Ok(match class {
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Object,
                ..
            } => CopySource::Object {
                value,
                class: *pointee,
            },
            RuntimeClass::Ref {
                pointee,
                kind: RefKind::Const,
                ..
            } => CopySource::Const {
                value,
                class: *pointee,
            },
            RuntimeClass::RawAddr { space, .. } => CopySource::Ptr {
                addr: value,
                space,
                class,
            },
            _ => CopySource::Value { value, class },
        })
    }

    fn copy_source_for_terminal(&self, terminal: PlaceTerminal<'db>) -> CopySource<'db> {
        match terminal {
            PlaceTerminal::Object { value, class } => CopySource::Object { value, class },
            PlaceTerminal::Const { value, class } => CopySource::Const { value, class },
            PlaceTerminal::Ptr { addr, space, class } => CopySource::Ptr { addr, space, class },
        }
    }

    fn copy_source_into_object(
        &mut self,
        source: CopySource<'db>,
        class: &RuntimeClass<'db>,
        object: ValueId,
    ) -> Result<(), LowerError> {
        if let CopySource::Const {
            value,
            class: source_class,
        } = &source
            && source_class == class
            && self.fb.type_of(*value) == self.fb.type_of(object)
        {
            self.fb.insert_inst_no_result(ObjInitConst::new(
                self.module.inst_set(),
                object,
                *value,
            ));
            return Ok(());
        }

        if !matches!(class, RuntimeClass::AggregateValue { .. }) {
            let value = self.load_copy_source_leaf(&source, class)?;
            self.fb
                .insert_inst_no_result(ObjStore::new(self.module.inst_set(), object, value));
            return Ok(());
        }

        self.copy_aggregate_source_into_object(source, class, object)
    }

    fn load_copy_source_leaf(
        &mut self,
        source: &CopySource<'db>,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        Ok(match source {
            CopySource::Value {
                value,
                class: source_class,
            } => {
                if matches!(source_class, RuntimeClass::AggregateValue { .. }) {
                    return Err(LowerError::Internal(format!(
                        "leaf copy source must not stay aggregate-valued: source={source_class:?} target={class:?}",
                    )));
                }
                let ty = self.module.ty_for_class(class)?;
                self.coerce_value_to_ty(*value, ty)?
            }
            CopySource::Object {
                value,
                class: source_class,
            } => {
                if matches!(source_class, RuntimeClass::AggregateValue { .. }) {
                    return Err(LowerError::Internal(format!(
                        "leaf object copy source must not stay aggregate-valued: source={source_class:?} target={class:?}",
                    )));
                }
                self.fb.insert_inst(
                    ObjLoad::new(self.module.inst_set(), *value),
                    self.module.ty_for_class(class)?,
                )
            }
            CopySource::Const { value, .. } => self.fb.insert_inst(
                ConstLoad::new(self.module.inst_set(), *value),
                self.module.ty_for_class(class)?,
            ),
            CopySource::Ptr {
                addr,
                space,
                class: source_class,
            } => {
                if matches!(source_class, RuntimeClass::AggregateValue { .. }) {
                    return Err(LowerError::Internal(format!(
                        "leaf ptr copy source must not stay aggregate-valued: source={source_class:?} target={class:?}",
                    )));
                }
                self.load_from_ptr(*addr, *space, class)?
            }
        })
    }

    fn copy_source_class<'b>(&self, source: &'b CopySource<'db>) -> &'b RuntimeClass<'db> {
        match source {
            CopySource::Value { class, .. }
            | CopySource::Object { class, .. }
            | CopySource::Const { class, .. }
            | CopySource::Ptr { class, .. } => class,
        }
    }

    fn copy_aggregate_source_into_object(
        &mut self,
        source: CopySource<'db>,
        class: &RuntimeClass<'db>,
        object: ValueId,
    ) -> Result<(), LowerError> {
        let RuntimeClass::AggregateValue { layout } = class else {
            return Err(LowerError::Internal(
                "aggregate source copy requires aggregate class".to_string(),
            ));
        };
        match layout.data(self.module.db) {
            Layout::Struct(data) => {
                for (idx, field) in data.fields.iter().enumerate() {
                    let field_idx = self.index_value(idx as u64);
                    let field_object = self.fb.insert_inst(
                        ObjProj::new(self.module.inst_set(), smallvec![object, field_idx]),
                        self.module.ty_for_object_projection(field)?,
                    );
                    let field_source = match &source {
                        CopySource::Value { value, .. } => CopySource::Value {
                            value: self.extract_aggregate_field(*value, idx, field)?,
                            class: field.clone(),
                        },
                        CopySource::Object { value, .. } => CopySource::Object {
                            value: self.fb.insert_inst(
                                ObjProj::new(self.module.inst_set(), smallvec![*value, field_idx]),
                                self.module.ty_for_object_projection(field)?,
                            ),
                            class: field.clone(),
                        },
                        CopySource::Const { value, .. } => CopySource::Const {
                            value: self.fb.insert_inst(
                                ConstProj::new(
                                    self.module.inst_set(),
                                    smallvec![*value, field_idx],
                                ),
                                self.module.ty_for_const_projection(field)?,
                            ),
                            class: field.clone(),
                        },
                        CopySource::Ptr { addr, space, .. } => CopySource::Ptr {
                            addr: self.offset_address(
                                *addr,
                                data.field_offset_words(self.module.db, idx),
                                *space,
                            )?,
                            space: *space,
                            class: field.clone(),
                        },
                    };
                    self.copy_source_into_object(field_source, field, field_object)?;
                }
                Ok(())
            }
            Layout::Array(data) => {
                for idx in 0..data.len as usize {
                    let elem_idx = self.index_value(idx as u64);
                    let elem_object = self.fb.insert_inst(
                        ObjIndex::new(self.module.inst_set(), object, elem_idx),
                        self.module.ty_for_object_projection(&data.elem)?,
                    );
                    let elem_source = match &source {
                        CopySource::Value { value, .. } => CopySource::Value {
                            value: self.extract_aggregate_field(*value, idx, &data.elem)?,
                            class: data.elem.clone(),
                        },
                        CopySource::Object { value, .. } => CopySource::Object {
                            value: self.fb.insert_inst(
                                ObjIndex::new(self.module.inst_set(), *value, elem_idx),
                                self.module.ty_for_object_projection(&data.elem)?,
                            ),
                            class: data.elem.clone(),
                        },
                        CopySource::Const { value, .. } => CopySource::Const {
                            value: self.fb.insert_inst(
                                ConstIndex::new(self.module.inst_set(), *value, elem_idx),
                                self.module.ty_for_const_projection(&data.elem)?,
                            ),
                            class: data.elem.clone(),
                        },
                        CopySource::Ptr { addr, space, .. } => CopySource::Ptr {
                            addr: self.offset_address(
                                *addr,
                                idx as u64 * data.elem.span_words(self.module.db),
                                *space,
                            )?,
                            space: *space,
                            class: data.elem.clone(),
                        },
                    };
                    self.copy_source_into_object(elem_source, &data.elem, elem_object)?;
                }
                Ok(())
            }
            Layout::Enum(data) => {
                self.copy_enum_source_into_object(source, *layout, data.variants.as_ref(), object)
            }
        }
    }

    fn copy_enum_source_into_object(
        &mut self,
        source: CopySource<'db>,
        dst_layout: LayoutId<'db>,
        dst_variants: &[mir::runtime::EnumVariantLayout<'db>],
        object: ValueId,
    ) -> Result<(), LowerError> {
        let source_class = self.copy_source_class(&source).clone();
        let RuntimeClass::AggregateValue { layout: src_layout } = source_class else {
            return Err(LowerError::Internal(
                "enum copy source must carry an enum aggregate class".to_string(),
            ));
        };
        let Layout::Enum(src_enum) = src_layout.data(self.module.db) else {
            return Err(LowerError::Internal(
                "enum copy source layout must be an enum".to_string(),
            ));
        };
        if src_enum.variants.len() != dst_variants.len() {
            return Err(LowerError::Internal(format!(
                "enum copy source/destination variant count mismatch: src={} dst={}",
                src_enum.variants.len(),
                dst_variants.len()
            )));
        }

        let source = match source {
            CopySource::Const { value, class } => CopySource::Value {
                value: self.fb.insert_inst(
                    ConstLoad::new(self.module.inst_set(), value),
                    self.module.ty_for_class(&class)?,
                ),
                class,
            },
            CopySource::Ptr { addr, space, class } => CopySource::Value {
                value: self.load_aggregate_from_ptr(addr, space, src_layout)?,
                class,
            },
            source => source,
        };

        let tag = match &source {
            CopySource::Value { value, .. } => self.fb.insert_inst(
                EnumTag::new(self.module.inst_set(), *value),
                self.module.enum_tag_ty(src_layout)?,
            ),
            CopySource::Object { value, .. } => self.fb.insert_inst(
                EnumGetTag::new(self.module.inst_set(), *value),
                self.module.enum_tag_ty(src_layout)?,
            ),
            CopySource::Const { .. } => unreachable!("const enum sources are normalized to values"),
            CopySource::Ptr { .. } => unreachable!("ptr enum sources are normalized to values"),
        };

        let entry = self
            .fb
            .current_block()
            .expect("enum copy requires a current block");
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let mut cases = Vec::with_capacity(dst_variants.len());
        let mut blocks = Vec::with_capacity(dst_variants.len());
        for (idx, _) in dst_variants.iter().enumerate() {
            let block = self.fb.append_block();
            cases.push((
                self.fb
                    .make_imm_value(self.module.enum_tag_immediate(src_layout, idx as u16)?),
                block,
            ));
            blocks.push(block);
        }
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            tag,
            Some(invalid),
            cases,
        ));

        for (idx, block) in blocks.into_iter().enumerate() {
            self.fb.switch_to_block(block);
            self.copy_enum_variant_into_object(
                &source,
                src_layout,
                src_enum.variants[idx].fields.as_ref(),
                dst_layout,
                dst_variants[idx].fields.as_ref(),
                VariantId {
                    enum_layout: src_layout,
                    index: idx as u16,
                },
                VariantId {
                    enum_layout: dst_layout,
                    index: idx as u16,
                },
                object,
            )?;
            self.fb
                .insert_inst_no_result(Jump::new(self.module.inst_set(), done));
        }

        self.fb.switch_to_block(invalid);
        self.fb
            .insert_inst_no_result(Unreachable::new(self.module.inst_set()));

        self.fb.switch_to_block(done);
        let _ = entry;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn copy_enum_variant_into_object(
        &mut self,
        source: &CopySource<'db>,
        src_layout: LayoutId<'db>,
        src_fields: &[RuntimeClass<'db>],
        dst_layout: LayoutId<'db>,
        dst_fields: &[RuntimeClass<'db>],
        src_variant: VariantId<'db>,
        dst_variant: VariantId<'db>,
        object: ValueId,
    ) -> Result<(), LowerError> {
        if src_fields.len() != dst_fields.len() {
            return Err(LowerError::Internal(format!(
                "enum variant payload arity mismatch: src_layout={src_layout:?} dst_layout={dst_layout:?} src_variant={} dst_variant={} src_fields={} dst_fields={}",
                src_variant.index,
                dst_variant.index,
                src_fields.len(),
                dst_fields.len()
            )));
        }

        self.fb.insert_inst_no_result(EnumSetTag::new(
            self.module.inst_set(),
            object,
            self.variant_ref(dst_variant)?,
        ));
        if src_fields.is_empty() {
            return Ok(());
        }

        let asserted_object = match source {
            CopySource::Object { value, .. } => Some(self.fb.insert_inst(
                EnumAssertVariantRef::new(
                    self.module.inst_set(),
                    *value,
                    self.variant_ref(src_variant)?,
                ),
                self.fb.type_of(*value),
            )),
            CopySource::Value { value, .. } => {
                self.fb.insert_inst_no_result(EnumAssertVariant::new(
                    self.module.inst_set(),
                    *value,
                    self.variant_ref(src_variant)?,
                ));
                None
            }
            CopySource::Const { .. } => unreachable!("const enum sources are normalized to values"),
            CopySource::Ptr { .. } => unreachable!("ptr enum sources are normalized to values"),
        };

        for (idx, (src_field, dst_field)) in src_fields.iter().zip(dst_fields.iter()).enumerate() {
            let field_idx = self.index_value(idx as u64);
            let field_object = self.fb.insert_inst(
                EnumProj::new(
                    self.module.inst_set(),
                    object,
                    self.variant_ref(dst_variant)?,
                    field_idx,
                ),
                self.module.ty_for_object_projection(dst_field)?,
            );
            let field_source = match source {
                CopySource::Value { value, .. } => CopySource::Value {
                    value: self.fb.insert_inst(
                        EnumExtract::new(
                            self.module.inst_set(),
                            *value,
                            self.variant_ref(src_variant)?,
                            field_idx,
                        ),
                        self.module.ty_for_class(src_field)?,
                    ),
                    class: src_field.clone(),
                },
                CopySource::Object { .. } => CopySource::Object {
                    value: self.fb.insert_inst(
                        EnumProj::new(
                            self.module.inst_set(),
                            asserted_object.expect("object enum copy should assert variant once"),
                            self.variant_ref(src_variant)?,
                            field_idx,
                        ),
                        self.module.ty_for_object_projection(src_field)?,
                    ),
                    class: src_field.clone(),
                },
                CopySource::Const { .. } => {
                    unreachable!("const enum sources are normalized to values")
                }
                CopySource::Ptr { .. } => {
                    return Err(LowerError::Unsupported(
                        "copying enum aggregates from non-memory providers is not supported yet"
                            .to_string(),
                    ));
                }
            };
            self.copy_source_into_object(field_source, dst_field, field_object)?;
        }
        Ok(())
    }

    fn addr_of_place(
        &mut self,
        place: &RuntimePlace<'db>,
        dst: Option<RLocalId>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        let Lowered::Value(terminal) = self.resolve_place(place)? else {
            return Ok(Lowered::Terminated);
        };
        match terminal {
            PlaceTerminal::Object { value, .. } => Ok(Lowered::Value(value)),
            PlaceTerminal::Const { .. } => Err(LowerError::Unsupported(
                "borrowing const-backed places is not supported".to_string(),
            )),
            PlaceTerminal::Ptr { addr, .. } => {
                if let Some(dst) = dst
                    && matches!(
                        self.body.value_class(dst),
                        Some(RuntimeClass::Ref {
                            kind: RefKind::Provider {
                                space: AddressSpaceKind::Memory,
                                ..
                            },
                            ..
                        }) | Some(RuntimeClass::Ref {
                            kind: RefKind::Object | RefKind::Const,
                            ..
                        })
                    )
                {
                    return Err(LowerError::Unsupported(
                        "memory providers require object-backed places, not raw pointers"
                            .to_string(),
                    ));
                }
                Ok(Lowered::Value(addr))
            }
        }
    }

    fn store_to_place(
        &mut self,
        place: &RuntimePlace<'db>,
        src: ValueId,
    ) -> Result<Lowered<()>, LowerError> {
        let Lowered::Value(terminal) = self.resolve_place(place)? else {
            return Ok(Lowered::Terminated);
        };
        match terminal {
            PlaceTerminal::Ptr { addr, space, class } => {
                self.store_to_ptr(addr, space, &class, src)?;
                Ok(Lowered::Value(()))
            }
            PlaceTerminal::Object { value, class } => {
                if !matches!(
                    class,
                    RuntimeClass::Scalar(_)
                        | RuntimeClass::Ref { .. }
                        | RuntimeClass::RawAddr { .. }
                ) {
                    return Err(LowerError::Unsupported(
                        "object place store requires scalar/raw subobject".to_string(),
                    ));
                }
                self.fb
                    .insert_inst_no_result(ObjStore::new(self.module.inst_set(), value, src));
                Ok(Lowered::Value(()))
            }
            PlaceTerminal::Const { .. } => Err(LowerError::Unsupported(
                "cannot store into const-backed places".to_string(),
            )),
        }
    }

    fn copy_into_place(
        &mut self,
        place: &RuntimePlace<'db>,
        src: RLocalId,
    ) -> Result<Lowered<()>, LowerError> {
        let src_value = self.local_value(src)?;
        let program = self.module.db as &dyn mir::MirDb;
        let dst_class = resolve_runtime_place(self.module.db, &program, &self.body, place)
            .map_err(|err| LowerError::Internal(format!("invalid runtime place: {err:?}")))?
            .result_class;
        let Lowered::Value(terminal) = self.resolve_place(place)? else {
            return Ok(Lowered::Terminated);
        };
        match terminal {
            PlaceTerminal::Object { value, .. } => {
                let source = self.copy_source_for_local(src, src_value)?;
                self.copy_source_into_object(source, &dst_class, value)?;
                Ok(Lowered::Value(()))
            }
            PlaceTerminal::Const { .. } => Err(LowerError::Unsupported(
                "cannot copy into const-backed places".to_string(),
            )),
            PlaceTerminal::Ptr { addr, space, .. } => {
                self.copy_to_ptr(addr, space, &dst_class, src_value)?;
                Ok(Lowered::Value(()))
            }
        }
    }

    fn copy_to_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        class: &RuntimeClass<'db>,
        src: ValueId,
    ) -> Result<(), LowerError> {
        match class {
            RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => {
                self.store_to_ptr(addr, space, class, src)
            }
            RuntimeClass::AggregateValue { layout } => match layout.data(self.module.db) {
                Layout::Struct(data) => {
                    for (idx, field) in data.fields.iter().enumerate() {
                        let field_value = self.extract_aggregate_field(src, idx, field)?;
                        let expected_ty = self.module.ty_for_class(field)?;
                        let actual_ty = self.fb.type_of(field_value);
                        if actual_ty != expected_ty {
                            return Err(LowerError::Internal(format!(
                                "copy-to-ptr struct field type mismatch: layout={layout:?} idx={idx} class={field:?} expected_ty={expected_ty:?} actual_ty={actual_ty:?} src_ty={:?}",
                                self.fb.type_of(src)
                            )));
                        }
                        let field_addr = self.offset_address(
                            addr,
                            data.field_offset_words(self.module.db, idx),
                            space,
                        )?;
                        self.copy_to_ptr(field_addr, space, field, field_value)?;
                    }
                    Ok(())
                }
                Layout::Array(data) => {
                    for idx in 0..data.len as usize {
                        let field_value = self.extract_aggregate_field(src, idx, &data.elem)?;
                        let expected_ty = self.module.ty_for_class(&data.elem)?;
                        let actual_ty = self.fb.type_of(field_value);
                        if actual_ty != expected_ty {
                            return Err(LowerError::Internal(format!(
                                "copy-to-ptr array elem type mismatch: layout={layout:?} idx={idx} class={:?} expected_ty={expected_ty:?} actual_ty={actual_ty:?} src_ty={:?}",
                                data.elem,
                                self.fb.type_of(src)
                            )));
                        }
                        let elem_addr = self.offset_address(
                            addr,
                            idx as u64 * data.elem.span_words(self.module.db),
                            space,
                        )?;
                        self.copy_to_ptr(elem_addr, space, &data.elem, field_value)?;
                    }
                    Ok(())
                }
                Layout::Enum(data) => self.copy_enum_to_ptr(addr, space, *layout, &data, src),
            },
        }
    }

    fn copy_enum_to_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        layout: LayoutId<'db>,
        data: &mir::runtime::EnumLayout<'db>,
        src: ValueId,
    ) -> Result<(), LowerError> {
        let tag = self.fb.insert_inst(
            EnumTag::new(self.module.inst_set(), src),
            self.module.enum_tag_ty(layout)?,
        );
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let mut cases = Vec::with_capacity(data.variants.len());
        let mut blocks = Vec::with_capacity(data.variants.len());
        for (idx, _) in data.variants.iter().enumerate() {
            let block = self.fb.append_block();
            cases.push((
                self.fb
                    .make_imm_value(self.module.enum_tag_immediate(layout, idx as u16)?),
                block,
            ));
            blocks.push(block);
        }
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            tag,
            Some(invalid),
            cases,
        ));

        for (idx, block) in blocks.into_iter().enumerate() {
            self.fb.switch_to_block(block);
            let variant = VariantId {
                enum_layout: layout,
                index: idx as u16,
            };
            let tag_word = self.index_value(idx as u64);
            self.store_to_ptr(
                addr,
                space,
                &RuntimeClass::Scalar(data.tag.clone()),
                tag_word,
            )?;
            for (field_idx, field) in data.variants[idx].fields.iter().enumerate() {
                let field_idx_value = self.index_value(field_idx as u64);
                let field_value = self.fb.insert_inst(
                    EnumExtract::new(
                        self.module.inst_set(),
                        src,
                        self.variant_ref(variant)?,
                        field_idx_value,
                    ),
                    self.module.ty_for_class(field)?,
                );
                let field_addr = self.offset_address(
                    addr,
                    variant
                        .field_offset_words(self.module.db, FieldIndex(field_idx as u16))
                        .ok_or_else(|| {
                            LowerError::Internal("variant field layout missing".to_string())
                        })?,
                    space,
                )?;
                self.copy_to_ptr(field_addr, space, field, field_value)?;
            }
            self.fb
                .insert_inst_no_result(Jump::new(self.module.inst_set(), done));
        }

        self.fb.switch_to_block(invalid);
        self.fb
            .insert_inst_no_result(Unreachable::new(self.module.inst_set()));

        self.fb.switch_to_block(done);
        Ok(())
    }

    fn load_from_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        match class {
            RuntimeClass::Scalar(scalar) => self.load_scalar(addr, space, scalar),
            RuntimeClass::Ref {
                kind:
                    RefKind::Provider {
                        space:
                            AddressSpaceKind::Storage
                            | AddressSpaceKind::Transient
                            | AddressSpaceKind::Calldata,
                        ..
                    },
                ..
            } => self.load_word(addr, space),
            RuntimeClass::RawAddr { .. } => self.load_word(addr, space),
            RuntimeClass::AggregateValue { layout } => match space {
                AddressSpaceKind::Memory => Err(LowerError::Unsupported(
                    "memory aggregate values should be addressed through object refs".to_string(),
                )),
                AddressSpaceKind::Storage
                | AddressSpaceKind::Transient
                | AddressSpaceKind::Calldata => self.load_aggregate_from_ptr(addr, space, *layout),
            },
            RuntimeClass::Ref { .. } => Err(LowerError::Unsupported(
                "loading handle values from raw-address places is not supported".to_string(),
            )),
        }
    }

    fn load_aggregate_from_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        layout: LayoutId<'db>,
    ) -> Result<ValueId, LowerError> {
        match layout.data(self.module.db) {
            Layout::Struct(data) => {
                let ty = self.module.ty_for_layout(layout)?;
                let mut value = self.fb.make_undef_value(ty);
                for (idx, field) in data.fields.iter().enumerate() {
                    let field_addr = self.offset_address(
                        addr,
                        data.field_offset_words(self.module.db, idx),
                        space,
                    )?;
                    let field_value = self.load_from_ptr(field_addr, space, field)?;
                    let expected_ty = self.module.ty_for_class(field)?;
                    let actual_ty = self.fb.type_of(field_value);
                    if actual_ty != expected_ty {
                        return Err(LowerError::Internal(format!(
                            "aggregate ptr load field type mismatch: layout={layout:?} idx={idx} class={field:?} expected_ty={expected_ty:?} actual_ty={actual_ty:?}"
                        )));
                    }
                    let idx = self.index_value(idx as u64);
                    value = self.fb.insert_inst(
                        sonatina_ir::inst::data::InsertValue::new(
                            self.module.inst_set(),
                            value,
                            idx,
                            field_value,
                        ),
                        ty,
                    );
                }
                Ok(value)
            }
            Layout::Array(data) => {
                let ty = self.module.ty_for_layout(layout)?;
                let mut value = self.fb.make_undef_value(ty);
                for idx in 0..data.len as usize {
                    let elem_addr = self.offset_address(
                        addr,
                        idx as u64 * data.elem.span_words(self.module.db),
                        space,
                    )?;
                    let elem = self.load_from_ptr(elem_addr, space, &data.elem)?;
                    let expected_ty = self.module.ty_for_class(&data.elem)?;
                    let actual_ty = self.fb.type_of(elem);
                    if actual_ty != expected_ty {
                        return Err(LowerError::Internal(format!(
                            "aggregate ptr load elem type mismatch: layout={layout:?} idx={idx} class={:?} expected_ty={expected_ty:?} actual_ty={actual_ty:?}",
                            data.elem
                        )));
                    }
                    let idx = self.index_value(idx as u64);
                    value = self.fb.insert_inst(
                        sonatina_ir::inst::data::InsertValue::new(
                            self.module.inst_set(),
                            value,
                            idx,
                            elem,
                        ),
                        ty,
                    );
                }
                Ok(value)
            }
            Layout::Enum(data) => self.load_enum_from_ptr(addr, space, layout, &data),
        }
    }

    fn load_enum_from_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        layout: LayoutId<'db>,
        data: &mir::runtime::EnumLayout<'db>,
    ) -> Result<ValueId, LowerError> {
        let layout_ty = self.module.ty_for_layout(layout)?;
        let object = self.fb.insert_inst(
            ObjAlloc::new(self.module.inst_set(), layout_ty),
            self.fb.module_builder.objref_type(layout_ty),
        );
        let tag = self.load_word(addr, space)?;
        let done = self.fb.append_block();
        let invalid = self.fb.append_block();
        let mut cases = Vec::with_capacity(data.variants.len());
        let mut blocks = Vec::with_capacity(data.variants.len());
        for (idx, _) in data.variants.iter().enumerate() {
            let block = self.fb.append_block();
            cases.push((self.index_value(idx as u64), block));
            blocks.push(block);
        }
        self.fb.insert_inst_no_result(BrTable::new(
            self.module.inst_set(),
            tag,
            Some(invalid),
            cases,
        ));

        for (idx, block) in blocks.into_iter().enumerate() {
            self.fb.switch_to_block(block);
            let variant = VariantId {
                enum_layout: layout,
                index: idx as u16,
            };
            let values = data.variants[idx]
                .fields
                .iter()
                .enumerate()
                .map(|(field_idx, field)| {
                    let field_addr = self.offset_address(
                        addr,
                        variant
                            .field_offset_words(self.module.db, FieldIndex(field_idx as u16))
                            .ok_or_else(|| {
                                LowerError::Internal("variant field layout missing".to_string())
                            })?,
                        space,
                    )?;
                    self.load_from_ptr(field_addr, space, field)
                })
                .collect::<Result<SmallVec<[ValueId; 2]>, _>>()?;
            self.fb.insert_inst_no_result(EnumWriteVariant::new(
                self.module.inst_set(),
                object,
                self.variant_ref(variant)?,
                values,
            ));
            self.fb
                .insert_inst_no_result(Jump::new(self.module.inst_set(), done));
        }

        self.fb.switch_to_block(invalid);
        self.fb
            .insert_inst_no_result(Unreachable::new(self.module.inst_set()));

        self.fb.switch_to_block(done);
        Ok(self
            .fb
            .insert_inst(ObjLoad::new(self.module.inst_set(), object), layout_ty))
    }

    fn load_word(&mut self, addr: ValueId, space: AddressSpaceKind) -> Result<ValueId, LowerError> {
        Ok(match space {
            AddressSpaceKind::Memory => self.fb.insert_inst(
                Mload::new(self.module.inst_set(), addr, Type::I256),
                Type::I256,
            ),
            AddressSpaceKind::Storage => self
                .fb
                .insert_inst(EvmSload::new(self.module.inst_set(), addr), Type::I256),
            AddressSpaceKind::Transient => self
                .fb
                .insert_inst(EvmTload::new(self.module.inst_set(), addr), Type::I256),
            AddressSpaceKind::Calldata => self.fb.insert_inst(
                EvmCalldataLoad::new(self.module.inst_set(), addr),
                Type::I256,
            ),
        })
    }

    fn load_scalar(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        scalar: &ScalarClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let word = self.load_word(addr, space)?;
        self.cast_scalar(word, scalar_ty(scalar))
    }

    fn store_to_ptr(
        &mut self,
        addr: ValueId,
        space: AddressSpaceKind,
        class: &RuntimeClass<'db>,
        src: ValueId,
    ) -> Result<(), LowerError> {
        let value = match class {
            RuntimeClass::Scalar(scalar) => self.cast_scalar(src, scalar_word_ty(scalar)),
            RuntimeClass::Ref {
                kind:
                    RefKind::Provider {
                        space:
                            AddressSpaceKind::Storage
                            | AddressSpaceKind::Transient
                            | AddressSpaceKind::Calldata,
                        ..
                    },
                ..
            } => self.coerce_value_to_ty(src, Type::I256),
            RuntimeClass::RawAddr { .. } => self.coerce_value_to_ty(src, Type::I256),
            RuntimeClass::AggregateValue { .. } | RuntimeClass::Ref { .. } => Err(
                LowerError::Unsupported("aggregate/handle ptr stores require CopyInto".to_string()),
            ),
        }?;
        match space {
            AddressSpaceKind::Memory => self.fb.insert_inst_no_result(Mstore::new(
                self.module.inst_set(),
                addr,
                value,
                Type::I256,
            )),
            AddressSpaceKind::Storage => {
                self.fb
                    .insert_inst_no_result(EvmSstore::new(self.module.inst_set(), addr, value))
            }
            AddressSpaceKind::Transient => {
                self.fb
                    .insert_inst_no_result(EvmTstore::new(self.module.inst_set(), addr, value))
            }
            AddressSpaceKind::Calldata => {
                return Err(LowerError::Unsupported(
                    "storing into calldata-backed providers is not supported".to_string(),
                ));
            }
        }
        Ok(())
    }

    fn extract_aggregate_field(
        &mut self,
        value: ValueId,
        idx: usize,
        class: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let idx = self.index_value(idx as u64);
        self.fb
            .insert_inst(
                sonatina_ir::inst::data::ExtractValue::new(self.module.inst_set(), value, idx),
                self.module.ty_for_class(class)?,
            )
            .pipe(Ok)
    }

    fn cast_scalar(&mut self, value: ValueId, ty: Type) -> Result<ValueId, LowerError> {
        let from = self.fb.type_of(value);
        if from == ty {
            return Ok(value);
        }
        if ty == Type::I1 {
            return Ok(condition_to_i1(&mut self.fb, value, self.module.inst_set()));
        }
        if from == Type::I1 {
            return Ok(self
                .fb
                .insert_inst(Zext::new(self.module.inst_set(), value, ty), ty));
        }
        let from_bits = int_bits(from);
        let to_bits = int_bits(ty);
        Ok(match from_bits.cmp(&to_bits) {
            std::cmp::Ordering::Less => self
                .fb
                .insert_inst(Zext::new(self.module.inst_set(), value, ty), ty),
            std::cmp::Ordering::Equal => value,
            std::cmp::Ordering::Greater => self
                .fb
                .insert_inst(Trunc::new(self.module.inst_set(), value, ty), ty),
        })
    }

    fn coerce_to_dst(
        &mut self,
        value: ValueId,
        dst: Option<RLocalId>,
    ) -> Result<ValueId, LowerError> {
        let Some(dst) = dst else {
            return Ok(value);
        };
        if self.body.value_class(dst).is_none() {
            return Ok(value);
        }
        let ty = self.local_ty(dst)?;
        self.coerce_value_to_ty(value, ty)
    }

    fn coerce_value_to_ty(&mut self, value: ValueId, ty: Type) -> Result<ValueId, LowerError> {
        let from = self.fb.type_of(value);
        if from == ty {
            return Ok(value);
        }

        let type_is_ref = |ty: Type| {
            matches!(
                ty.resolve_compound(&self.fb.module_builder.ctx),
                Some(CompoundType::ObjRef(_) | CompoundType::ConstRef(_))
            )
        };
        if type_is_ref(from) || type_is_ref(ty) {
            return Err(LowerError::Internal(format!(
                "cannot coerce reference value from {from:?} to {ty:?}"
            )));
        }

        let from_ptr = from.is_pointer(&self.fb.module_builder.ctx);
        let to_ptr = ty.is_pointer(&self.fb.module_builder.ctx);
        Ok(match (from_ptr, to_ptr) {
            (true, false) => {
                if !ty.is_integral() {
                    return Err(LowerError::Internal(format!(
                        "cannot coerce pointer value from {from:?} to non-integral {ty:?}"
                    )));
                }
                self.fb
                    .insert_inst(PtrToInt::new(self.module.inst_set(), value, ty), ty)
            }
            (false, true) => {
                if !from.is_integral() {
                    return Err(LowerError::Internal(format!(
                        "cannot coerce non-integral value from {from:?} to pointer {ty:?}"
                    )));
                }
                self.fb
                    .insert_inst(IntToPtr::new(self.module.inst_set(), value, ty), ty)
            }
            (true, true) => self
                .fb
                .insert_inst(Bitcast::new(self.module.inst_set(), value, ty), ty),
            (false, false) => self.cast_scalar(value, ty)?,
        })
    }

    fn lower_unary(
        &mut self,
        op: UnOp,
        value: ValueId,
        result: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = self.module.ty_for_class(result)?;
        Ok(match op {
            UnOp::Not => {
                let value = condition_to_i1(&mut self.fb, value, self.module.inst_set());
                self.fb
                    .insert_inst(IsZero::new(self.module.inst_set(), value), Type::I1)
            }
            UnOp::Minus => {
                let value = self.cast_scalar(value, ty)?;
                self.fb
                    .insert_inst(Neg::new(self.module.inst_set(), value), ty)
            }
            UnOp::BitNot => {
                let value = self.cast_scalar(value, ty)?;
                self.fb
                    .insert_inst(Not::new(self.module.inst_set(), value), ty)
            }
            UnOp::Plus | UnOp::Mut | UnOp::Ref => value,
        })
    }

    fn lower_binary(
        &mut self,
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
        operand: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        Ok(match op {
            BinOp::Arith(op) => self.lower_arith(op, lhs, rhs, operand)?,
            BinOp::Comp(op) => self.lower_comp(op, lhs, rhs, operand)?,
            BinOp::Logical(op) => match op {
                LogicalBinOp::And => {
                    let lhs = condition_to_i1(&mut self.fb, lhs, self.module.inst_set());
                    let rhs = condition_to_i1(&mut self.fb, rhs, self.module.inst_set());
                    self.fb
                        .insert_inst(And::new(self.module.inst_set(), lhs, rhs), Type::I1)
                }
                LogicalBinOp::Or => {
                    let lhs = condition_to_i1(&mut self.fb, lhs, self.module.inst_set());
                    let rhs = condition_to_i1(&mut self.fb, rhs, self.module.inst_set());
                    self.fb
                        .insert_inst(Or::new(self.module.inst_set(), lhs, rhs), Type::I1)
                }
            },
            BinOp::Index => {
                return Err(LowerError::Unsupported(
                    "index should not appear as a runtime binary op".to_string(),
                ));
            }
        })
    }

    fn lower_arith(
        &mut self,
        op: ArithBinOp,
        lhs: ValueId,
        rhs: ValueId,
        operand: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = self.module.ty_for_class(operand)?;
        let lhs = self.cast_scalar(lhs, ty)?;
        let rhs = self.cast_scalar(rhs, ty)?;
        let signed = scalar_is_signed(operand);
        Ok(match op {
            ArithBinOp::Add => {
                let [raw, overflow] = if signed {
                    self.fb.insert_saddo(lhs, rhs)
                } else {
                    self.fb.insert_uaddo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            ArithBinOp::Sub => {
                let [raw, overflow] = if signed {
                    self.fb.insert_ssubo(lhs, rhs)
                } else {
                    self.fb.insert_usubo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            ArithBinOp::Mul => {
                let [raw, overflow] = if signed {
                    self.fb.insert_smulo(lhs, rhs)
                } else {
                    self.fb.insert_umulo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            ArithBinOp::Div => {
                self.emit_division_by_zero_revert(rhs, ty)?;
                let [raw, overflow] = if signed {
                    self.fb.insert_evm_sdivo(lhs, rhs)
                } else {
                    self.fb.insert_evm_udivo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            ArithBinOp::Rem => {
                self.emit_division_by_zero_revert(rhs, ty)?;
                if signed {
                    self.fb
                        .insert_inst(EvmSmod::new(self.module.inst_set(), lhs, rhs), ty)
                } else {
                    self.fb
                        .insert_inst(EvmUmod::new(self.module.inst_set(), lhs, rhs), ty)
                }
            }
            ArithBinOp::Pow => self
                .fb
                .insert_inst(EvmExp::new(self.module.inst_set(), lhs, rhs), ty),
            ArithBinOp::LShift => self
                .fb
                .insert_inst(Shl::new(self.module.inst_set(), rhs, lhs), ty),
            ArithBinOp::RShift => {
                if signed {
                    self.fb
                        .insert_inst(Sar::new(self.module.inst_set(), rhs, lhs), ty)
                } else {
                    self.fb
                        .insert_inst(Shr::new(self.module.inst_set(), rhs, lhs), ty)
                }
            }
            ArithBinOp::BitOr => self
                .fb
                .insert_inst(Or::new(self.module.inst_set(), lhs, rhs), ty),
            ArithBinOp::BitXor => self
                .fb
                .insert_inst(Xor::new(self.module.inst_set(), lhs, rhs), ty),
            ArithBinOp::BitAnd => self
                .fb
                .insert_inst(And::new(self.module.inst_set(), lhs, rhs), ty),
            ArithBinOp::Range => {
                return Err(LowerError::Unsupported(
                    "range is not a runtime arithmetic op".to_string(),
                ));
            }
        })
    }

    fn lower_intrinsic_arith_builtin(
        &mut self,
        op: IntrinsicArithBinOp,
        checked: bool,
        lhs: RLocalId,
        rhs: RLocalId,
        class: &ScalarClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = scalar_ty(class);
        let lhs = self.local_value(lhs)?;
        let rhs = self.local_value(rhs)?;
        let lhs = self.cast_scalar(lhs, ty)?;
        let rhs = self.cast_scalar(rhs, ty)?;
        let signed = matches!(class.repr, ScalarRepr::Int { signed: true, .. });
        Ok(match (op, checked) {
            (IntrinsicArithBinOp::Add, false) => self
                .fb
                .insert_inst(Add::new(self.module.inst_set(), lhs, rhs), ty),
            (IntrinsicArithBinOp::Sub, false) => self
                .fb
                .insert_inst(Sub::new(self.module.inst_set(), lhs, rhs), ty),
            (IntrinsicArithBinOp::Mul, false) => self
                .fb
                .insert_inst(Mul::new(self.module.inst_set(), lhs, rhs), ty),
            (IntrinsicArithBinOp::Div, false) => {
                if signed {
                    self.fb
                        .insert_inst(EvmSdiv::new(self.module.inst_set(), lhs, rhs), ty)
                } else {
                    self.fb
                        .insert_inst(EvmUdiv::new(self.module.inst_set(), lhs, rhs), ty)
                }
            }
            (IntrinsicArithBinOp::Rem, false) => {
                if signed {
                    self.fb
                        .insert_inst(EvmSmod::new(self.module.inst_set(), lhs, rhs), ty)
                } else {
                    self.fb
                        .insert_inst(EvmUmod::new(self.module.inst_set(), lhs, rhs), ty)
                }
            }
            (IntrinsicArithBinOp::Pow, false) => self
                .fb
                .insert_inst(EvmExp::new(self.module.inst_set(), lhs, rhs), ty),
            (IntrinsicArithBinOp::Add, true) => {
                let [raw, overflow] = if signed {
                    self.fb.insert_saddo(lhs, rhs)
                } else {
                    self.fb.insert_uaddo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            (IntrinsicArithBinOp::Sub, true) => {
                let [raw, overflow] = if signed {
                    self.fb.insert_ssubo(lhs, rhs)
                } else {
                    self.fb.insert_usubo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            (IntrinsicArithBinOp::Mul, true) => {
                let [raw, overflow] = if signed {
                    self.fb.insert_smulo(lhs, rhs)
                } else {
                    self.fb.insert_umulo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            (IntrinsicArithBinOp::Div, true) => {
                self.emit_division_by_zero_revert(rhs, ty)?;
                let [raw, overflow] = if signed {
                    self.fb.insert_evm_sdivo(lhs, rhs)
                } else {
                    self.fb.insert_evm_udivo(lhs, rhs)
                };
                self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
                raw
            }
            (IntrinsicArithBinOp::Rem, true) => {
                self.emit_division_by_zero_revert(rhs, ty)?;
                if signed {
                    self.fb
                        .insert_inst(EvmSmod::new(self.module.inst_set(), lhs, rhs), ty)
                } else {
                    self.fb
                        .insert_inst(EvmUmod::new(self.module.inst_set(), lhs, rhs), ty)
                }
            }
            (IntrinsicArithBinOp::Pow, true) => {
                self.lower_checked_pow_builtin(lhs, rhs, ty, signed)?
            }
        })
    }

    fn lower_checked_pow_builtin(
        &mut self,
        base: ValueId,
        exp: ValueId,
        ty: Type,
        signed: bool,
    ) -> Result<ValueId, LowerError> {
        let zero = self.fb.make_imm_value(Immediate::zero(ty));
        let one = self.fb.make_imm_value(Immediate::one(ty));
        if signed {
            let negative = self
                .fb
                .insert_inst(Slt::new(self.module.inst_set(), exp, zero), Type::I1);
            self.emit_empty_revert(negative)?;
        }

        let entry = self
            .fb
            .current_block()
            .expect("checked pow requires a current block");
        let header = self.fb.append_block();
        let body = self.fb.append_block();
        let done = self.fb.append_block();
        self.fb
            .insert_inst_no_result(Jump::new(self.module.inst_set(), header));
        self.fb.switch_to_block(header);
        let result = self
            .fb
            .insert_inst(Phi::new(self.module.inst_set(), vec![(one, entry)]), ty);
        let idx = self
            .fb
            .insert_inst(Phi::new(self.module.inst_set(), vec![(zero, entry)]), ty);
        let done_cond = self
            .fb
            .insert_inst(Eq::new(self.module.inst_set(), idx, exp), Type::I1);
        self.fb
            .insert_inst_no_result(Br::new(self.module.inst_set(), done_cond, done, body));

        self.fb.switch_to_block(body);
        let [next_result, overflow] = if signed {
            self.fb.insert_smulo(result, base)
        } else {
            self.fb.insert_umulo(result, base)
        };
        self.emit_panic_revert(overflow, PANIC_OVERFLOW)?;
        let one_step = self.fb.make_imm_value(Immediate::one(ty));
        let next_idx = self
            .fb
            .insert_inst(Add::new(self.module.inst_set(), idx, one_step), ty);
        let loop_back = self
            .fb
            .current_block()
            .expect("checked pow body should stay in a block");
        self.fb.append_phi_arg(result, next_result, loop_back);
        self.fb.append_phi_arg(idx, next_idx, loop_back);
        self.fb
            .insert_inst_no_result(Jump::new(self.module.inst_set(), header));

        self.fb.switch_to_block(done);
        Ok(result)
    }

    fn lower_comp(
        &mut self,
        op: CompBinOp,
        lhs: ValueId,
        rhs: ValueId,
        operand: &RuntimeClass<'db>,
    ) -> Result<ValueId, LowerError> {
        let ty = self.module.ty_for_class(operand)?;
        let lhs = self.cast_scalar(lhs, ty)?;
        let rhs = self.cast_scalar(rhs, ty)?;
        let signed = scalar_is_signed(operand);
        Ok(match op {
            CompBinOp::Eq => self
                .fb
                .insert_inst(Eq::new(self.module.inst_set(), lhs, rhs), Type::I1),
            CompBinOp::NotEq => {
                let eq = self
                    .fb
                    .insert_inst(Eq::new(self.module.inst_set(), lhs, rhs), Type::I1);
                self.fb
                    .insert_inst(IsZero::new(self.module.inst_set(), eq), Type::I1)
            }
            CompBinOp::Lt => {
                if signed {
                    self.fb
                        .insert_inst(Slt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                } else {
                    self.fb
                        .insert_inst(Lt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                }
            }
            CompBinOp::LtEq => {
                let gt = if signed {
                    self.fb
                        .insert_inst(Slt::new(self.module.inst_set(), rhs, lhs), Type::I1)
                } else {
                    self.fb
                        .insert_inst(Gt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                };
                self.fb
                    .insert_inst(IsZero::new(self.module.inst_set(), gt), Type::I1)
            }
            CompBinOp::Gt => {
                if signed {
                    self.fb
                        .insert_inst(Slt::new(self.module.inst_set(), rhs, lhs), Type::I1)
                } else {
                    self.fb
                        .insert_inst(Gt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                }
            }
            CompBinOp::GtEq => {
                let lt = if signed {
                    self.fb
                        .insert_inst(Slt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                } else {
                    self.fb
                        .insert_inst(Lt::new(self.module.inst_set(), lhs, rhs), Type::I1)
                };
                self.fb
                    .insert_inst(IsZero::new(self.module.inst_set(), lt), Type::I1)
            }
        })
    }

    fn ensure_empty_revert_block(&mut self) -> BlockId {
        if let Some(block) = self.empty_revert_block {
            return block;
        }
        let revert_block = self.fb.append_block();
        let current = self
            .fb
            .current_block()
            .expect("overflow block requires current block");
        self.fb.switch_to_block(revert_block);
        let zero = zero_for_type(&mut self.fb, Type::I256);
        self.fb
            .insert_inst_no_result(EvmRevert::new(self.module.inst_set(), zero, zero));
        self.fb.switch_to_block(current);
        self.empty_revert_block = Some(revert_block);
        revert_block
    }

    fn ensure_panic_revert_block(&mut self, code: u64) -> BlockId {
        if code == PANIC_OVERFLOW
            && let Some(block) = self.overflow_panic_block
        {
            return block;
        }
        if code == PANIC_DIVISION_BY_ZERO
            && let Some(block) = self.division_by_zero_panic_block
        {
            return block;
        }
        let revert_block = self.fb.append_block();
        let current = self
            .fb
            .current_block()
            .expect("panic block requires current block");
        self.fb.switch_to_block(revert_block);
        self.emit_panic_revert_payload(code);
        self.fb.switch_to_block(current);
        match code {
            PANIC_OVERFLOW => self.overflow_panic_block = Some(revert_block),
            PANIC_DIVISION_BY_ZERO => self.division_by_zero_panic_block = Some(revert_block),
            _ => {}
        }
        revert_block
    }

    fn emit_panic_revert_payload(&mut self, code: u64) {
        let zero = self.fb.make_imm_value(I256::zero());
        let selector = self.fb.make_imm_value(panic_selector_immediate());
        let code_offset = self.fb.make_imm_value(I256::from(4u64));
        let code = self.fb.make_imm_value(I256::from(code));
        let len = self.fb.make_imm_value(I256::from(36u64));
        self.fb.insert_inst_no_result(Mstore::new(
            self.module.inst_set(),
            zero,
            selector,
            Type::I256,
        ));
        self.fb.insert_inst_no_result(Mstore::new(
            self.module.inst_set(),
            code_offset,
            code,
            Type::I256,
        ));
        self.fb
            .insert_inst_no_result(EvmRevert::new(self.module.inst_set(), zero, len));
    }

    fn emit_empty_revert(&mut self, overflow_flag: ValueId) -> Result<(), LowerError> {
        let revert_block = self.ensure_empty_revert_block();
        let continue_block = self.fb.append_block();
        self.fb.insert_inst_no_result(Br::new(
            self.module.inst_set(),
            overflow_flag,
            revert_block,
            continue_block,
        ));
        self.fb.switch_to_block(continue_block);
        Ok(())
    }

    fn emit_panic_revert(&mut self, overflow_flag: ValueId, code: u64) -> Result<(), LowerError> {
        let revert_block = self.ensure_panic_revert_block(code);
        let continue_block = self.fb.append_block();
        self.fb.insert_inst_no_result(Br::new(
            self.module.inst_set(),
            overflow_flag,
            revert_block,
            continue_block,
        ));
        self.fb.switch_to_block(continue_block);
        Ok(())
    }

    fn emit_division_by_zero_revert(&mut self, rhs: ValueId, ty: Type) -> Result<(), LowerError> {
        let zero = self.fb.make_imm_value(Immediate::zero(ty));
        let divisor_is_zero = self
            .fb
            .insert_inst(Eq::new(self.module.inst_set(), rhs, zero), Type::I1);
        self.emit_panic_revert(divisor_is_zero, PANIC_DIVISION_BY_ZERO)
    }

    fn emit_unconditional_empty_revert<T>(&mut self) -> Lowered<T> {
        let revert_block = self.ensure_empty_revert_block();
        self.fb
            .insert_inst_no_result(Jump::new(self.module.inst_set(), revert_block));
        Lowered::Terminated
    }

    fn checked_index_value(
        &mut self,
        base_class: &RuntimeClass<'db>,
        index: &IndexSource<RLocalId>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        let len = base_class.array_len(self.module.db).ok_or_else(|| {
            LowerError::Internal("index projection on non-array class".to_string())
        })?;
        match index {
            IndexSource::Constant(index) => {
                self.checked_constant_index_value(len, Some(*index as u64))
            }
            IndexSource::Dynamic(index) => {
                let index = self.local_value(*index)?;
                if let Value::Immediate { imm, .. } = self.fb.func.dfg.value(index) {
                    return self.checked_constant_index_value(len, immediate_to_u64_index(*imm));
                }
                let len = self.index_value(len);
                let in_bounds = self
                    .fb
                    .insert_inst(Lt::new(self.module.inst_set(), index, len), Type::I1);
                let out_of_bounds = self
                    .fb
                    .insert_inst(IsZero::new(self.module.inst_set(), in_bounds), Type::I1);
                self.emit_empty_revert(out_of_bounds)?;
                Ok(Lowered::Value(index))
            }
        }
    }

    fn checked_constant_index_value(
        &mut self,
        len: u64,
        index: Option<u64>,
    ) -> Result<Lowered<ValueId>, LowerError> {
        if let Some(index) = index
            && index < len
        {
            return Ok(Lowered::Value(self.index_value(index)));
        }
        Ok(self.emit_unconditional_empty_revert())
    }

    fn variant_ref(&self, variant: VariantId<'db>) -> Result<EnumVariantRef, LowerError> {
        let ty = self
            .module
            .type_cache
            .get(&variant.enum_layout)
            .copied()
            .ok_or_else(|| {
                LowerError::Internal("enum type must be declared before use".to_string())
            })?;
        let Type::Compound(compound) = ty else {
            return Err(LowerError::Internal(
                "enum type is not compound".to_string(),
            ));
        };
        self.fb
            .module_builder
            .ctx
            .with_ty_store(|_store| EnumVariantRef::new(compound, variant.index as u32))
            .pipe(Ok)
    }

    fn index_value(&mut self, value: u64) -> ValueId {
        self.fb.make_imm_value(I256::from(value))
    }

    fn offset_address(
        &mut self,
        base: ValueId,
        units: u64,
        space: AddressSpaceKind,
    ) -> Result<ValueId, LowerError> {
        if units == 0 {
            return Ok(base);
        }
        let offset = self.index_value(self.scale_for_space(space, units));
        Ok(self
            .fb
            .insert_inst(Add::new(self.module.inst_set(), base, offset), Type::I256))
    }

    fn scale_for_space(&self, space: AddressSpaceKind, units: u64) -> u64 {
        match space {
            AddressSpaceKind::Memory | AddressSpaceKind::Calldata => units.saturating_mul(32),
            AddressSpaceKind::Storage | AddressSpaceKind::Transient => units,
        }
    }
}

struct LowerBodyContext<'a, 'db> {
    db: &'db DriverDataBase,
    body: &'a RuntimeBody<'db>,
    context: String,
    block: Option<RBlockId>,
    stmt: Option<usize>,
}

impl<'a, 'db> LowerBodyContext<'a, 'db> {
    fn wrap(self, err: LowerError) -> LowerError {
        let excerpt = self.block.map_or_else(
            || mir::format_runtime_body(self.db, self.body),
            |block| mir::format_runtime_body_excerpt(self.db, self.body, block, self.stmt),
        );
        match err {
            LowerError::RuntimeLower(_) => err,
            LowerError::Unsupported(message) => LowerError::Unsupported(format!(
                "{}: {message}\n\nrMIR context:\n{}",
                self.context, excerpt
            )),
            LowerError::Internal(message) => LowerError::Internal(format!(
                "{}: {message}\n\nrMIR context:\n{}",
                self.context, excerpt
            )),
        }
    }
}

fn block_successors<'db>(terminator: &RTerminator<'db>) -> SmallVec<[RBlockId; 2]> {
    match terminator {
        RTerminator::Goto(block) => smallvec![*block],
        RTerminator::Branch {
            then_bb, else_bb, ..
        } => smallvec![*then_bb, *else_bb],
        RTerminator::SwitchScalar { cases, default, .. } => cases
            .iter()
            .map(|(_, block)| *block)
            .chain(std::iter::once(*default))
            .collect(),
        RTerminator::MatchEnumTag { cases, default, .. } => cases
            .iter()
            .map(|(_, block)| *block)
            .chain(default.iter().copied())
            .collect(),
        RTerminator::TerminalCall { .. }
        | RTerminator::ReturnData { .. }
        | RTerminator::Revert { .. }
        | RTerminator::SelfDestruct { .. }
        | RTerminator::Trap
        | RTerminator::Return(_)
        | RTerminator::Stop => SmallVec::new(),
    }
}

fn compute_reachable_blocks<'db>(body: &RuntimeBody<'db>) -> Vec<bool> {
    let mut reachable = vec![false; body.blocks.len()];
    let mut worklist = vec![0usize];
    while let Some(idx) = worklist.pop() {
        if std::mem::replace(&mut reachable[idx], true) {
            continue;
        }
        for succ in block_successors(&body.blocks[idx].terminator) {
            worklist.push(succ.as_u32() as usize);
        }
    }
    reachable
}

trait Pipe: Sized {
    fn pipe<T>(self, f: impl FnOnce(Self) -> T) -> T {
        f(self)
    }
}

impl<T> Pipe for T {}

fn linkage_for_runtime(linkage: RuntimeLinkage) -> Linkage {
    match linkage {
        RuntimeLinkage::Private => Linkage::Private,
        RuntimeLinkage::Internal => Linkage::Public,
    }
}

fn scalar_ty<'db>(scalar: &ScalarClass<'db>) -> Type {
    match scalar.repr {
        ScalarRepr::Bool => Type::I1,
        ScalarRepr::Int { bits, .. } => int_ty(bits),
        ScalarRepr::FixedBytes { len } => fixed_bytes_ty(len),
        ScalarRepr::Address { .. } => Type::I256,
    }
}

fn scalar_word_ty<'db>(scalar: &ScalarClass<'db>) -> Type {
    match scalar.repr {
        ScalarRepr::Bool
        | ScalarRepr::Int { .. }
        | ScalarRepr::FixedBytes { .. }
        | ScalarRepr::Address { .. } => Type::I256,
    }
}

fn scalar_is_signed(class: &RuntimeClass<'_>) -> bool {
    matches!(
        class,
        RuntimeClass::Scalar(ScalarClass {
            repr: ScalarRepr::Int { signed: true, .. },
            ..
        })
    )
}

fn prim_is_signed(prim: PrimTy) -> bool {
    matches!(
        prim,
        PrimTy::I8
            | PrimTy::I16
            | PrimTy::I32
            | PrimTy::I64
            | PrimTy::I128
            | PrimTy::I256
            | PrimTy::Isize
    )
}

fn intrinsic_prim_from_ty<'db>(
    db: &'db DriverDataBase,
    ty: TyId<'db>,
) -> Result<PrimTy, LowerError> {
    let base_ty = ty.base_ty(db);
    let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(db) else {
        return Err(LowerError::Internal(format!(
            "intrinsic type must be primitive, got `{}`",
            ty.pretty_print(db)
        )));
    };
    Ok(*prim)
}

fn intrinsic_value_type(prim: PrimTy) -> Type {
    match prim {
        PrimTy::Bool => Type::I1,
        PrimTy::U8 | PrimTy::I8 => Type::I8,
        PrimTy::U16 | PrimTy::I16 => Type::I16,
        PrimTy::U32 | PrimTy::I32 => Type::I32,
        PrimTy::U64 | PrimTy::I64 => Type::I64,
        PrimTy::U128 | PrimTy::I128 => Type::I128,
        PrimTy::U256 | PrimTy::I256 | PrimTy::Usize | PrimTy::Isize => Type::I256,
        PrimTy::String
        | PrimTy::Array
        | PrimTy::Tuple(_)
        | PrimTy::Ptr
        | PrimTy::View
        | PrimTy::BorrowMut
        | PrimTy::BorrowRef => Type::I256,
    }
}

fn lower_intrinsic_operand(
    fb: &mut FunctionBuilder<InstInserter>,
    is: &EvmInstSet,
    value: ValueId,
    prim: PrimTy,
    op_ty: Type,
) -> ValueId {
    if prim == PrimTy::Bool {
        condition_to_i1(fb, value, is)
    } else {
        cast_int_value(fb, is, value, op_ty, prim_is_signed(prim))
    }
}

fn cast_int_value(
    fb: &mut FunctionBuilder<InstInserter>,
    is: &EvmInstSet,
    value: ValueId,
    target_ty: Type,
    signed: bool,
) -> ValueId {
    let current_ty = fb.type_of(value);
    if current_ty == target_ty {
        return value;
    }

    let current_bits = int_bits(current_ty);
    let target_bits = int_bits(target_ty);
    if current_bits > target_bits {
        fb.insert_inst(Trunc::new(is, value, target_ty), target_ty)
    } else if current_bits < target_bits {
        if signed && current_ty != Type::I1 {
            fb.insert_inst(Sext::new(is, value, target_ty), target_ty)
        } else {
            fb.insert_inst(Zext::new(is, value, target_ty), target_ty)
        }
    } else {
        value
    }
}

fn intrinsic_binary_args<'ctx, 'db, 'a>(
    lowerer: &mut FunctionLowerer<'ctx, 'db, 'a>,
    args: &[RLocalId],
) -> Result<(ValueId, ValueId), LowerError> {
    let [lhs, rhs] = args else {
        return Err(LowerError::Internal(
            "intrinsic requires 2 arguments".to_string(),
        ));
    };
    Ok((lowerer.local_value(*lhs)?, lowerer.local_value(*rhs)?))
}

fn intrinsic_unary_arg<'ctx, 'db, 'a>(
    lowerer: &mut FunctionLowerer<'ctx, 'db, 'a>,
    args: &[RLocalId],
) -> Result<ValueId, LowerError> {
    let [value] = args else {
        return Err(LowerError::Internal(
            "intrinsic requires 1 argument".to_string(),
        ));
    };
    lowerer.local_value(*value)
}

fn int_ty(bits: u16) -> Type {
    match bits {
        0 | 1 => Type::I1,
        2..=8 => Type::I8,
        9..=16 => Type::I16,
        17..=32 => Type::I32,
        33..=64 => Type::I64,
        65..=128 => Type::I128,
        _ => Type::I256,
    }
}

fn fixed_bytes_ty(len: u16) -> Type {
    int_ty(len.saturating_mul(8))
}

fn int_bits(ty: Type) -> u16 {
    match ty {
        Type::I1 => 1,
        Type::I8 => 8,
        Type::I16 => 16,
        Type::I32 => 32,
        Type::I64 => 64,
        Type::I128 => 128,
        Type::I256 => 256,
        _ => 256,
    }
}

fn bytes_to_i256(bytes: &[u8], signed: bool) -> I256 {
    if bytes.is_empty() {
        return I256::zero();
    }
    let _ = signed;
    I256::from_be_bytes(bytes)
}

fn panic_selector_immediate() -> Immediate {
    let mut bytes = [0; 32];
    bytes[..4].copy_from_slice(&[0x4e, 0x48, 0x7b, 0x71]);
    Immediate::from_i256(bytes_to_i256(&bytes, false), Type::I256)
}

fn zero_for_type(fb: &mut FunctionBuilder<InstInserter>, ty: Type) -> ValueId {
    if ty.is_unit() || ty.is_compound() {
        fb.make_undef_value(ty)
    } else if ty.is_integral() || ty.is_enum_tag() {
        fb.make_imm_value(Immediate::zero(ty))
    } else {
        fb.make_undef_value(ty)
    }
}

fn condition_to_i1(
    fb: &mut FunctionBuilder<InstInserter>,
    cond: ValueId,
    is: &EvmInstSet,
) -> ValueId {
    if fb.type_of(cond) == Type::I1 {
        cond
    } else {
        let zero = zero_for_type(fb, fb.type_of(cond));
        fb.insert_inst(Ne::new(is, cond, zero), Type::I1)
    }
}

fn stable_hash<T: Hash>(value: &T) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn runtime_section_refs_match<'db>(
    db: &'db DriverDataBase,
    lhs: &mir::RuntimeSectionRef<'db>,
    rhs: &mir::RuntimeSectionRef<'db>,
) -> bool {
    let (lhs_object, lhs_section) = match lhs {
        mir::RuntimeSectionRef::Local { object, section }
        | mir::RuntimeSectionRef::External { object, section } => (object, section),
    };
    let (rhs_object, rhs_section) = match rhs {
        mir::RuntimeSectionRef::Local { object, section }
        | mir::RuntimeSectionRef::External { object, section } => (object, section),
    };
    lhs_object.name(db) == rhs_object.name(db) && lhs_section == rhs_section
}

fn compute_section_membership<'db>(
    db: &'db DriverDataBase,
    package: &RuntimePackage<'db>,
) -> FxHashMap<mir::RuntimeInstance<'db>, Vec<mir::RuntimeSectionRef<'db>>> {
    let mut membership =
        FxHashMap::<mir::RuntimeInstance<'db>, Vec<mir::RuntimeSectionRef<'db>>>::default();
    for object in package.objects(db) {
        for section in object.sections(db) {
            let section_ref = mir::RuntimeSectionRef::Local {
                object,
                section: section.name.clone(),
            };
            let mut stack = vec![section.entry.instance(db)];
            let mut seen = rustc_hash::FxHashSet::default();
            while let Some(instance) = stack.pop() {
                if !seen.insert(instance) {
                    continue;
                }
                membership
                    .entry(instance)
                    .or_default()
                    .push(section_ref.clone());
                for call in instance.calls(db) {
                    stack.push(call.callee);
                }
            }
        }
    }
    membership
}

fn code_region_symbol<'db>(
    db: &'db DriverDataBase,
    package: &RuntimePackage<'db>,
    region: mir::RuntimeCodeRegion<'db>,
) -> String {
    package
        .code_regions(db)
        .iter()
        .find(|resolved| resolved.region(db) == region)
        .map(|resolved| resolved.symbol(db).clone())
        .unwrap_or_else(|| format!("code_region_{}", stable_hash(&region)))
}

fn immediate_to_u64_index(imm: Immediate) -> Option<u64> {
    let value = imm.as_i256();
    if value.is_negative() || value > I256::from(u64::MAX) {
        return None;
    }
    Some(value.to_u256().low_u64())
}

trait ProjectionType<'db> {
    fn ty_for_object_projection(&mut self, class: &RuntimeClass<'db>) -> Result<Type, LowerError>;
    fn ty_for_const_projection(&mut self, class: &RuntimeClass<'db>) -> Result<Type, LowerError>;
}

impl<'db, 'a> ProjectionType<'db> for ModuleLowerer<'db, 'a> {
    fn ty_for_object_projection(&mut self, class: &RuntimeClass<'db>) -> Result<Type, LowerError> {
        let field_ty = self.ty_for_class(class)?;
        Ok(self.builder.objref_type(field_ty))
    }

    fn ty_for_const_projection(&mut self, class: &RuntimeClass<'db>) -> Result<Type, LowerError> {
        let field_ty = self.ty_for_class(class)?;
        Ok(self.builder.constref_type(field_ty))
    }
}
