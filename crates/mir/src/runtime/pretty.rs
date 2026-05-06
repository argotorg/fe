use std::fmt::Write;

use cranelift_entity::EntityRef;
use hir::analysis::{semantic::FieldIndex, ty::ty_def::TyId};
use hir::projection::IndexSource;

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{
        AddressSpaceKind, ConstScalar, Layout, LayoutId, PlaceElem, PlaceRoot, RBlockId, RExpr,
        RLocalId, RStmt, RTerminator, RefKind, RefView, RuntimeBody, RuntimeBuiltin,
        RuntimeCarrier, RuntimeClass, RuntimeCodeRegion, RuntimeFunction, RuntimeLinkage,
        RuntimeLocalRoot, RuntimeObject, RuntimePackage, RuntimePlace, RuntimeSection,
        RuntimeSectionName, RuntimeSectionRef, ScalarClass, ScalarRepr, ScalarRole, VariantId,
    },
    verify::{RuntimeVerifyFailure, RuntimeVerifySite},
};

pub fn format_runtime_verify_failure<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    failure: &RuntimeVerifyFailure<'db>,
) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "error: {:?}", failure.error);
    let _ = writeln!(out, "site: {}", format_verify_site(db, body, &failure.site));
    if let Some(local) = failure.error.local() {
        let _ = writeln!(out, "local: {}", format_local_line(db, body, local));
    }
    let _ = writeln!(out);
    out.push_str(&format_runtime_body(db, body));
    out
}

pub fn format_runtime_body<'db>(db: &'db dyn MirDb, body: &RuntimeBody<'db>) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "fn {}(", format_runtime_instance(db, body.owner));
    for (idx, param) in body.signature.params.iter().enumerate() {
        let suffix = if idx + 1 == body.signature.params.len() {
            ""
        } else {
            ","
        };
        let _ = writeln!(
            out,
            "  {}: {}{}",
            format_local_id(param.local),
            format_class(db, &param.class),
            suffix
        );
    }
    let _ = writeln!(
        out,
        ") -> {} {{",
        body.signature
            .ret
            .as_ref()
            .map(|class| format_class(db, class))
            .unwrap_or_else(|| "()".to_string())
    );
    if !body.provider_bindings.is_empty() {
        let _ = writeln!(out, "  providers:");
        for (idx, binding) in body.provider_bindings.iter().enumerate() {
            let _ = writeln!(
                out,
                "    @{} => value={}, provider={}, place={}",
                idx,
                format_local_id(binding.value),
                format_class(db, &binding.provider_class),
                format_class(db, &binding.place_class)
            );
        }
    }
    if !body.locals.is_empty() {
        let _ = writeln!(out, "  locals:");
        for (idx, local) in body.locals.iter().enumerate() {
            let _ = writeln!(
                out,
                "    {}",
                format_local_decl(db, body, RLocalId::from_u32(idx as u32), local.semantic_ty)
            );
        }
    }
    for (block_idx, block) in body.blocks.iter().enumerate() {
        let block_id = RBlockId::from_u32(block_idx as u32);
        let _ = writeln!(out, "  bb{}:", block_idx);
        for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
            let _ = writeln!(out, "    [{}] {};", stmt_idx, format_stmt(db, stmt));
        }
        let _ = writeln!(out, "    -> {}", format_terminator(db, &block.terminator));
        if block_id.index() + 1 != body.blocks.len() {
            let _ = writeln!(out);
        }
    }
    out.push('}');
    out
}

pub fn format_runtime_body_excerpt<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    block: RBlockId,
    stmt: Option<usize>,
) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "fn {} {{", format_runtime_instance(db, body.owner));
    match body.block(block) {
        Some(block_data) => {
            let center = stmt.unwrap_or(block_data.stmts.len());
            let start = center.saturating_sub(3);
            let end = if stmt.is_some() {
                (center + 4).min(block_data.stmts.len())
            } else {
                block_data.stmts.len()
            };
            let _ = writeln!(out, "  bb{}:", block.index());
            for (idx, stmt_data) in block_data.stmts.iter().enumerate().take(end).skip(start) {
                let marker = if Some(idx) == stmt { ">>" } else { "  " };
                let _ = writeln!(out, "  {marker} [{idx}] {};", format_stmt(db, stmt_data));
            }
            let term_marker = if stmt.is_none() { ">>" } else { "  " };
            let _ = writeln!(
                out,
                "  {term_marker} -> {}",
                format_terminator(db, &block_data.terminator)
            );
        }
        None => {
            let _ = writeln!(out, "  <missing block {}>", block.index());
        }
    }
    out.push('}');
    out
}

pub fn format_runtime_package<'db>(db: &'db dyn MirDb, package: &RuntimePackage<'db>) -> String {
    let mut out = String::new();
    let _ = writeln!(out, "package {:?} {{", package.top_mod(db).name(db));
    if let Some(primary) = package.primary_object(db) {
        let _ = writeln!(out, "  primary_object: {}", primary.name(db));
    }
    if !package.root_objects(db).is_empty() {
        let names = package
            .root_objects(db)
            .iter()
            .map(|object| object.name(db).clone())
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "  root_objects: [{names}]");
    }
    if !package.objects(db).is_empty() {
        let _ = writeln!(out, "  objects:");
        for object in package.objects(db) {
            write_object_summary(db, &mut out, object);
        }
    }
    if !package.code_regions(db).is_empty() {
        let _ = writeln!(out, "  code_regions:");
        for code_region in package.code_regions(db) {
            let _ = writeln!(
                out,
                "    {} -> {} @ {} root={}",
                code_region.symbol(db),
                format_code_region(db, code_region.region(db)),
                format_section_ref(db, &code_region.source(db)),
                code_region.root(db).symbol(db)
            );
        }
    }
    if !package.const_regions(db).is_empty() {
        let _ = writeln!(out, "  const_regions:");
        for const_region in package.const_regions(db) {
            let data = const_region.data(db);
            let _ = writeln!(
                out,
                "    {:?}: layout={}, node={:?}",
                const_region,
                format_layout_id(db, data.layout),
                data.value
            );
        }
    }
    if !package.functions(db).is_empty() {
        let _ = writeln!(out, "  functions:");
        for function in package.functions(db) {
            write_function_summary(db, &mut out, function);
        }
    }
    out.push('}');
    out
}

fn write_object_summary<'db>(db: &'db dyn MirDb, out: &mut String, object: RuntimeObject<'db>) {
    let _ = writeln!(out, "    object {}:", object.name(db));
    for section in object.sections(db) {
        write_section_summary(db, out, &section);
    }
}

fn write_section_summary<'db>(db: &'db dyn MirDb, out: &mut String, section: &RuntimeSection<'db>) {
    let _ = writeln!(
        out,
        "      {} entry={}",
        format_section_name(&section.name),
        section.entry.symbol(db)
    );
    for embed in &section.embeds {
        let _ = writeln!(
            out,
            "        embed {} as {}",
            format_section_ref(db, &embed.source),
            embed.as_symbol
        );
    }
    for const_region in &section.const_regions {
        let _ = writeln!(out, "        const {:?}", const_region);
    }
}

fn write_function_summary<'db>(
    db: &'db dyn MirDb,
    out: &mut String,
    function: RuntimeFunction<'db>,
) {
    let linkage = match function.linkage(db) {
        RuntimeLinkage::Private => "private",
        RuntimeLinkage::Internal => "internal",
    };
    let _ = writeln!(
        out,
        "    {} ({linkage}, inline={:?}, owner={:?})",
        function.symbol(db),
        function.inline_hint(db),
        function.owner(db)
    );
    if !function.referenced_const_regions(db).is_empty() {
        let refs = function
            .referenced_const_regions(db)
            .iter()
            .map(|region| format!("{region:?}"))
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(out, "      const_refs: [{refs}]");
    }
    let body = function.instance(db).body(db);
    for line in format_runtime_body(db, &body).lines() {
        let _ = writeln!(out, "      {line}");
    }
}

fn format_verify_site<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    site: &RuntimeVerifySite,
) -> String {
    match site {
        RuntimeVerifySite::SignatureParam(index) => format!("signature param {index}"),
        RuntimeVerifySite::LocalRoot(local) => {
            format!("local root {}", format_local_line(db, body, *local))
        }
        RuntimeVerifySite::LocalCarrier(local) => {
            format!("local carrier {}", format_local_line(db, body, *local))
        }
        RuntimeVerifySite::Stmt { block, stmt } => {
            let rendered = body
                .block(*block)
                .and_then(|block_data| block_data.stmts.get(*stmt))
                .map(|stmt| format_stmt(db, stmt))
                .unwrap_or_else(|| "<missing stmt>".to_string());
            format!("bb{}[{}]: {rendered}", block.index(), stmt)
        }
        RuntimeVerifySite::Terminator { block } => {
            let rendered = body
                .block(*block)
                .map(|block_data| format_terminator(db, &block_data.terminator))
                .unwrap_or_else(|| "<missing terminator>".to_string());
            format!("bb{}.term: {rendered}", block.index())
        }
        RuntimeVerifySite::Body => "body".to_string(),
    }
}

fn format_local_decl<'db>(
    db: &'db dyn MirDb,
    body: &RuntimeBody<'db>,
    local: RLocalId,
    semantic_ty: TyId<'db>,
) -> String {
    format!(
        "{}: ty={}, carrier={}, root={}",
        format_local_id(local),
        semantic_ty.pretty_print(db),
        body.local(local)
            .map(|local| format_carrier(db, &local.carrier))
            .unwrap_or_else(|| "<missing>".to_string()),
        body.local(local)
            .map(|local| format_local_root(db, &local.root))
            .unwrap_or_else(|| "<missing>".to_string())
    )
}

fn format_local_line<'db>(db: &'db dyn MirDb, body: &RuntimeBody<'db>, local: RLocalId) -> String {
    body.local(local)
        .map(|local_data| format_local_decl(db, body, local, local_data.semantic_ty))
        .unwrap_or_else(|| format!("{}: <missing>", format_local_id(local)))
}

fn format_carrier<'db>(db: &'db dyn MirDb, carrier: &RuntimeCarrier<'db>) -> String {
    match carrier {
        RuntimeCarrier::Erased => "erased".to_string(),
        RuntimeCarrier::Value(class) => format_class(db, class),
    }
}

fn format_local_root<'db>(db: &'db dyn MirDb, root: &RuntimeLocalRoot<'db>) -> String {
    match root {
        RuntimeLocalRoot::None => "none".to_string(),
        RuntimeLocalRoot::Slot(class) => format!("slot {}", format_class(db, class)),
        RuntimeLocalRoot::Ref(class) => format!("ref {}", format_class(db, class)),
        RuntimeLocalRoot::Ptr { space, class } => {
            format!("ptr {} {}", format_space(*space), format_class(db, class))
        }
    }
}

fn format_layout_id<'db>(db: &'db dyn MirDb, layout: LayoutId<'db>) -> String {
    format!("{layout:?} {}", format_layout(db, layout))
}

fn format_section_name(name: &RuntimeSectionName) -> String {
    match name {
        RuntimeSectionName::Init => "init".to_string(),
        RuntimeSectionName::Runtime => "runtime".to_string(),
        RuntimeSectionName::Main => "main".to_string(),
        RuntimeSectionName::Test(name) => format!("test({name})"),
        RuntimeSectionName::CodeRegion(name) => format!("code_region({name})"),
    }
}

fn format_section_ref<'db>(db: &'db dyn MirDb, section: &RuntimeSectionRef<'db>) -> String {
    match section {
        RuntimeSectionRef::Local { object, section } => {
            format!(
                "local {}::{}",
                object.name(db),
                format_section_name(section)
            )
        }
        RuntimeSectionRef::External { object, section } => {
            format!(
                "external {}::{}",
                object.name(db),
                format_section_name(section)
            )
        }
    }
}

fn format_code_region<'db>(db: &'db dyn MirDb, region: RuntimeCodeRegion<'db>) -> String {
    match region.key(db) {
        crate::runtime::RuntimeCodeRegionKey::ContractInit { contract } => {
            format!("contract_init({:?})", contract.name(db))
        }
        crate::runtime::RuntimeCodeRegionKey::ContractRuntime { contract } => {
            format!("contract_runtime({:?})", contract.name(db))
        }
        crate::runtime::RuntimeCodeRegionKey::ManualContractRoot { func } => {
            format!("manual_contract_root({:?})", func.name(db))
        }
        crate::runtime::RuntimeCodeRegionKey::FunctionRoot { symbol, callee } => {
            format!(
                "function_root({symbol}, {})",
                format_runtime_instance(db, callee)
            )
        }
    }
}

fn format_stmt<'db>(db: &'db dyn MirDb, stmt: &RStmt<'db>) -> String {
    match stmt {
        RStmt::Assign { dst, expr } => {
            format!("{} = {}", format_local_id(*dst), format_expr(db, expr))
        }
        RStmt::EnumAssertVariant { value, variant } => format!(
            "enum_assert_variant {} := {}",
            format_local_id(*value),
            format_variant(db, *variant)
        ),
        RStmt::Store { dst, src } => {
            format!("store {} <- {}", format_place(dst), format_local_id(*src))
        }
        RStmt::CopyInto { dst, src } => {
            format!(
                "copy_into {} <- {}",
                format_place(dst),
                format_local_id(*src)
            )
        }
        RStmt::EnumSetTag { root, variant } => {
            format!(
                "enum_set_tag {} := {}",
                format_local_id(*root),
                format_variant(db, *variant)
            )
        }
        RStmt::EnumWriteVariant {
            root,
            variant,
            fields,
        } => {
            let fields = fields
                .iter()
                .map(|value| format_local_id(*value))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "enum_write_variant {} := {}({fields})",
                format_local_id(*root),
                format_variant(db, *variant)
            )
        }
    }
}

fn format_expr<'db>(db: &'db dyn MirDb, expr: &RExpr<'db>) -> String {
    match expr {
        RExpr::Use(value) => format!("use {}", format_local_id(*value)),
        RExpr::ConstScalar(value) => format_const_scalar(value),
        RExpr::Placeholder { class } => format!("placeholder {}", format_class(db, class)),
        RExpr::Builtin(builtin) => format_builtin(db, builtin),
        RExpr::Unary { op, value } => format!("{op:?} {}", format_local_id(*value)),
        RExpr::Binary { op, lhs, rhs } => {
            format!(
                "{op:?} {}, {}",
                format_local_id(*lhs),
                format_local_id(*rhs)
            )
        }
        RExpr::Cast { value, to } => {
            format!(
                "cast {} as {}",
                format_local_id(*value),
                format_scalar_class(db, to)
            )
        }
        RExpr::ConstRef { region, layout } => {
            format!("const_ref @{:?}:{}", region, format_layout(db, *layout))
        }
        RExpr::AllocObject { layout } => format!("alloc {}", format_layout(db, *layout)),
        RExpr::MaterializeToObject { src } => {
            format!("materialize_to_object {}", format_local_id(*src))
        }
        RExpr::MaterializePlaceToObject { place } => {
            format!("materialize_place_to_object {}", format_place(place))
        }
        RExpr::ProviderFromRaw {
            raw,
            provider_ty,
            space,
            target,
        } => format!(
            "provider_from_raw {} {} {}{}",
            format_local_id(*raw),
            provider_ty.pretty_print(db),
            format_space(*space),
            target.map_or_else(String::new, |layout| format!(
                " {}",
                format_layout(db, layout)
            ))
        ),
        RExpr::WordToRawAddr {
            value,
            space,
            target,
        } => format!(
            "word_to_raw {} {}{}",
            format_local_id(*value),
            format_space(*space),
            target
                .map(|target| format!(" {}", format_layout(db, target)))
                .unwrap_or_default()
        ),
        RExpr::ProviderToRaw { value } => format!("provider_to_raw {}", format_local_id(*value)),
        RExpr::RetagRef { value } => format!("retag_ref {}", format_local_id(*value)),
        RExpr::AddrOf { place } => format!("addr_of {}", format_place(place)),
        RExpr::Load { place } => format!("load {}", format_place(place)),
        RExpr::Call { callee, args } => {
            let args = args
                .iter()
                .map(|value| format_local_id(*value))
                .collect::<Vec<_>>()
                .join(", ");
            format!("call {}({args})", format_runtime_instance(db, *callee))
        }
        RExpr::EnumMake {
            layout,
            variant,
            fields,
        } => {
            let fields = fields
                .iter()
                .map(|value| format_local_id(*value))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "enum_make {}::{}({fields})",
                format_layout(db, *layout),
                format_variant(db, *variant)
            )
        }
        RExpr::EnumTagOfValue { value } => format!("enum_tag_of {}", format_local_id(*value)),
        RExpr::EnumIsVariant { value, variant } => {
            format!(
                "enum_is_variant {}, {}",
                format_local_id(*value),
                format_variant(db, *variant)
            )
        }
        RExpr::EnumExtract {
            value,
            variant,
            field,
        } => format!(
            "enum_extract {}, {}.{}",
            format_local_id(*value),
            format_variant(db, *variant),
            field.0
        ),
        RExpr::EnumGetTag { root } => format!("enum_get_tag {}", format_local_id(*root)),
        RExpr::EnumAssertVariantRef { root, variant } => format!(
            "enum_assert_variant_ref {}, {}",
            format_local_id(*root),
            format_variant(db, *variant)
        ),
    }
}

fn format_terminator<'db>(db: &'db dyn MirDb, term: &RTerminator<'db>) -> String {
    match term {
        RTerminator::Goto(block) => format!("goto bb{}", block.index()),
        RTerminator::Branch {
            cond,
            then_bb,
            else_bb,
        } => format!(
            "branch {} ? bb{} : bb{}",
            format_local_id(*cond),
            then_bb.index(),
            else_bb.index()
        ),
        RTerminator::SwitchScalar {
            discr,
            cases,
            default,
        } => {
            let cases = cases
                .iter()
                .map(|(value, block)| {
                    format!("{} => bb{}", format_const_scalar(value), block.index())
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "switch {} [{}] else bb{}",
                format_local_id(*discr),
                cases,
                default.index()
            )
        }
        RTerminator::MatchEnumTag {
            tag,
            enum_layout,
            cases,
            default,
        } => {
            let cases = cases
                .iter()
                .map(|(variant, block)| {
                    format!("{} => bb{}", format_variant(db, *variant), block.index())
                })
                .collect::<Vec<_>>()
                .join(", ");
            let default = default
                .map(|block| format!(" else bb{}", block.index()))
                .unwrap_or_default();
            format!(
                "match_enum_tag {} : {} [{}]{}",
                format_local_id(*tag),
                format_layout(db, *enum_layout),
                cases,
                default
            )
        }
        RTerminator::TerminalCall { callee, args } => {
            let args = args
                .iter()
                .map(|value| format_local_id(*value))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "terminal_call {}({args})",
                format_runtime_instance(db, *callee)
            )
        }
        RTerminator::ReturnData { offset, len } => format!(
            "return_data {}, {}",
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RTerminator::Revert { offset, len } => format!(
            "revert {}, {}",
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RTerminator::SelfDestruct { beneficiary } => {
            format!("selfdestruct {}", format_local_id(*beneficiary))
        }
        RTerminator::Trap => "trap".to_string(),
        RTerminator::Return(value) => value
            .map(|value| format!("return {}", format_local_id(value)))
            .unwrap_or_else(|| "return".to_string()),
        RTerminator::Stop => "stop".to_string(),
    }
}

fn format_builtin<'db>(db: &'db dyn MirDb, builtin: &RuntimeBuiltin<'db>) -> String {
    match builtin {
        RuntimeBuiltin::Mload { addr } => format!("mload {}", format_local_id(*addr)),
        RuntimeBuiltin::Mstore { addr, value } => {
            format!(
                "mstore {}, {}",
                format_local_id(*addr),
                format_local_id(*value)
            )
        }
        RuntimeBuiltin::Mstore8 { addr, value } => {
            format!(
                "mstore8 {}, {}",
                format_local_id(*addr),
                format_local_id(*value)
            )
        }
        RuntimeBuiltin::Mcopy { dst, src, len } => format!(
            "mcopy {}, {}, {}",
            format_local_id(*dst),
            format_local_id(*src),
            format_local_id(*len)
        ),
        RuntimeBuiltin::Msize => "msize".to_string(),
        RuntimeBuiltin::Sload { slot } => format!("sload {}", format_local_id(*slot)),
        RuntimeBuiltin::Sstore { slot, value } => {
            format!(
                "sstore {}, {}",
                format_local_id(*slot),
                format_local_id(*value)
            )
        }
        RuntimeBuiltin::CallValue => "callvalue".to_string(),
        RuntimeBuiltin::ReturnDataSize => "returndatasize".to_string(),
        RuntimeBuiltin::ReturnDataCopy { dst, offset, len } => format!(
            "returndatacopy {}, {}, {}",
            format_local_id(*dst),
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RuntimeBuiltin::CallDataSize => "calldatasize".to_string(),
        RuntimeBuiltin::CallDataLoad { offset } => {
            format!("calldataload {}", format_local_id(*offset))
        }
        RuntimeBuiltin::CallDataCopy { dst, offset, len } => format!(
            "calldatacopy {}, {}, {}",
            format_local_id(*dst),
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RuntimeBuiltin::CodeSize => "codesize".to_string(),
        RuntimeBuiltin::CodeCopy { dst, offset, len } => format!(
            "codecopy {}, {}, {}",
            format_local_id(*dst),
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RuntimeBuiltin::Keccak256 { offset, len } => format!(
            "keccak256 {}, {}",
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RuntimeBuiltin::AddMod { lhs, rhs, modulus } => format!(
            "addmod {}, {}, {}",
            format_local_id(*lhs),
            format_local_id(*rhs),
            format_local_id(*modulus)
        ),
        RuntimeBuiltin::MulMod { lhs, rhs, modulus } => format!(
            "mulmod {}, {}, {}",
            format_local_id(*lhs),
            format_local_id(*rhs),
            format_local_id(*modulus)
        ),
        RuntimeBuiltin::SignExtend { byte, value } => format!(
            "signextend {}, {}",
            format_local_id(*byte),
            format_local_id(*value)
        ),
        RuntimeBuiltin::IntrinsicArith {
            op,
            checked,
            lhs,
            rhs,
            ..
        } => format!(
            "{}_{op:?} {}, {}",
            if *checked { "checked" } else { "unchecked" },
            format_local_id(*lhs),
            format_local_id(*rhs)
        ),
        RuntimeBuiltin::Saturating { op, lhs, rhs, .. } => format!(
            "saturating_{op:?} {}, {}",
            format_local_id(*lhs),
            format_local_id(*rhs)
        ),
        RuntimeBuiltin::Address => "address".to_string(),
        RuntimeBuiltin::Caller => "caller".to_string(),
        RuntimeBuiltin::Origin => "origin".to_string(),
        RuntimeBuiltin::GasPrice => "gasprice".to_string(),
        RuntimeBuiltin::CoinBase => "coinbase".to_string(),
        RuntimeBuiltin::Timestamp => "timestamp".to_string(),
        RuntimeBuiltin::Number => "number".to_string(),
        RuntimeBuiltin::PrevRandao => "prevrandao".to_string(),
        RuntimeBuiltin::GasLimit => "gaslimit".to_string(),
        RuntimeBuiltin::ChainId => "chainid".to_string(),
        RuntimeBuiltin::BaseFee => "basefee".to_string(),
        RuntimeBuiltin::SelfBalance => "selfbalance".to_string(),
        RuntimeBuiltin::BlockHash { block } => format!("blockhash {}", format_local_id(*block)),
        RuntimeBuiltin::Gas => "gas".to_string(),
        RuntimeBuiltin::CurrentCodeRegionLen => "current_code_region_len".to_string(),
        RuntimeBuiltin::CodeRegionOffset { region } => format!("code_region_offset {:?}", region),
        RuntimeBuiltin::CodeRegionLen { region } => format!("code_region_len {:?}", region),
        RuntimeBuiltin::Malloc { size } => format!("malloc {}", format_local_id(*size)),
        RuntimeBuiltin::Call {
            gas,
            addr,
            value,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => format!(
            "call {}, {}, {}, {}, {}, {}, {}",
            format_local_id(*gas),
            format_local_id(*addr),
            format_local_id(*value),
            format_local_id(*args_offset),
            format_local_id(*args_len),
            format_local_id(*ret_offset),
            format_local_id(*ret_len)
        ),
        RuntimeBuiltin::StaticCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => format!(
            "staticcall {}, {}, {}, {}, {}, {}",
            format_local_id(*gas),
            format_local_id(*addr),
            format_local_id(*args_offset),
            format_local_id(*args_len),
            format_local_id(*ret_offset),
            format_local_id(*ret_len)
        ),
        RuntimeBuiltin::DelegateCall {
            gas,
            addr,
            args_offset,
            args_len,
            ret_offset,
            ret_len,
        } => format!(
            "delegatecall {}, {}, {}, {}, {}, {}",
            format_local_id(*gas),
            format_local_id(*addr),
            format_local_id(*args_offset),
            format_local_id(*args_len),
            format_local_id(*ret_offset),
            format_local_id(*ret_len)
        ),
        RuntimeBuiltin::Create { value, offset, len } => format!(
            "create {}, {}, {}",
            format_local_id(*value),
            format_local_id(*offset),
            format_local_id(*len)
        ),
        RuntimeBuiltin::Create2 {
            value,
            offset,
            len,
            salt,
        } => format!(
            "create2 {}, {}, {}, {}",
            format_local_id(*value),
            format_local_id(*offset),
            format_local_id(*len),
            format_local_id(*salt)
        ),
        RuntimeBuiltin::Log0 { offset, len } => {
            format!(
                "log0 {}, {}",
                format_local_id(*offset),
                format_local_id(*len)
            )
        }
        RuntimeBuiltin::Log1 {
            offset,
            len,
            topic0,
        } => format!(
            "log1 {}, {}, {}",
            format_local_id(*offset),
            format_local_id(*len),
            format_local_id(*topic0)
        ),
        RuntimeBuiltin::Log2 {
            offset,
            len,
            topic0,
            topic1,
        } => format!(
            "log2 {}, {}, {}, {}",
            format_local_id(*offset),
            format_local_id(*len),
            format_local_id(*topic0),
            format_local_id(*topic1)
        ),
        RuntimeBuiltin::Log3 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
        } => format!(
            "log3 {}, {}, {}, {}, {}",
            format_local_id(*offset),
            format_local_id(*len),
            format_local_id(*topic0),
            format_local_id(*topic1),
            format_local_id(*topic2)
        ),
        RuntimeBuiltin::Log4 {
            offset,
            len,
            topic0,
            topic1,
            topic2,
            topic3,
        } => format!(
            "log4 {}, {}, {}, {}, {}, {}",
            format_local_id(*offset),
            format_local_id(*len),
            format_local_id(*topic0),
            format_local_id(*topic1),
            format_local_id(*topic2),
            format_local_id(*topic3)
        ),
        RuntimeBuiltin::CallDataSelector => "calldata_selector".to_string(),
        RuntimeBuiltin::MakeContractFieldRef { slot, class, kind } => format!(
            "make_contract_field_ref slot={slot} {} {}",
            format_class(db, class),
            format_ref_kind(db, kind)
        ),
    }
}

fn format_place<'db>(place: &RuntimePlace<'db>) -> String {
    let mut out = match &place.root {
        PlaceRoot::Slot(local) => format_local_id(*local),
        PlaceRoot::Ref(value) => format!("*{}", format_local_id(*value)),
        PlaceRoot::Provider(binding) => format!("@{}", binding.index()),
        PlaceRoot::Ptr { addr, space, .. } => {
            format!("ptr({} {})", format_space(*space), format_local_id(*addr))
        }
    };
    for elem in place.path.iter() {
        match elem {
            PlaceElem::Field(field) => {
                let _ = write!(out, ".{}", format_field(*field));
            }
            PlaceElem::Index(index) => {
                let _ = write!(out, "[{}]", format_index_source(*index));
            }
            PlaceElem::VariantField { variant, field } => {
                let _ = write!(out, ".variant{}.{}", variant.index, format_field(*field));
            }
            PlaceElem::Deref => {
                let _ = write!(out, ".*");
            }
        }
    }
    out
}

fn format_class<'db>(db: &'db dyn MirDb, class: &RuntimeClass<'db>) -> String {
    match class {
        RuntimeClass::Scalar(class) => format_scalar_class(db, class),
        RuntimeClass::AggregateValue { layout } => format!("agg {}", format_layout(db, *layout)),
        RuntimeClass::Ref {
            pointee,
            kind,
            view,
        } => format!(
            "ref {} {} {}",
            format_ref_kind(db, kind),
            format_ref_view(db, view),
            format_class(db, pointee)
        ),
        RuntimeClass::RawAddr { space, target } => format!(
            "raw {}{}",
            format_space(*space),
            target
                .map(|layout| format!(" {}", format_layout(db, layout)))
                .unwrap_or_default()
        ),
    }
}

fn format_scalar_class<'db>(db: &'db dyn MirDb, class: &ScalarClass<'db>) -> String {
    let repr = match class.repr {
        ScalarRepr::Bool => "bool".to_string(),
        ScalarRepr::Int { bits, signed } => {
            format!("{}int{bits}", if signed { "" } else { "u" })
        }
        ScalarRepr::FixedBytes { len } => format!("bytes{len}"),
        ScalarRepr::Address { bits } => format!("address{bits}"),
    };
    match &class.role {
        ScalarRole::Plain => repr,
        ScalarRole::EnumTag { enum_layout } => {
            format!("{repr}<tag {}>", format_layout(db, *enum_layout))
        }
    }
}

fn format_ref_kind<'db>(db: &'db dyn MirDb, kind: &RefKind<'db>) -> String {
    match kind {
        RefKind::Const => "const".to_string(),
        RefKind::Object => "object".to_string(),
        RefKind::Provider { provider_ty, space } => {
            format!(
                "provider {} {}",
                provider_ty.pretty_print(db),
                format_space(*space)
            )
        }
    }
}

fn format_ref_view<'db>(db: &'db dyn MirDb, view: &RefView<'db>) -> String {
    match view {
        RefView::Whole => "whole".to_string(),
        RefView::EnumVariant(variant) => format!("variant {}", format_variant(db, *variant)),
    }
}

fn format_layout<'db>(db: &'db dyn MirDb, layout: LayoutId<'db>) -> String {
    match layout.data(db) {
        Layout::Struct(layout) => format!("struct {}", layout.source_ty.pretty_print(db)),
        Layout::Array(layout) => format!(
            "array {} x {}",
            layout.source_ty.pretty_print(db),
            layout.len
        ),
        Layout::Enum(layout) => format!("enum {}", layout.source_ty.pretty_print(db)),
    }
}

fn format_variant<'db>(db: &'db dyn MirDb, variant: VariantId<'db>) -> String {
    variant
        .layout(db)
        .and_then(|layout| {
            layout
                .variants
                .get(variant.index as usize)
                .map(|variant_layout| variant_layout.name.clone())
        })
        .unwrap_or_else(|| format!("#{}", variant.index))
}

fn format_field(field: FieldIndex) -> String {
    format!("f{}", field.0)
}

fn format_index_source(index: IndexSource<RLocalId>) -> String {
    match index {
        IndexSource::Constant(value) => value.to_string(),
        IndexSource::Dynamic(local) => format_local_id(local),
    }
}

fn format_const_scalar(value: &ConstScalar) -> String {
    match value {
        ConstScalar::Bool(value) => value.to_string(),
        ConstScalar::Int {
            bits,
            signed,
            words,
        } => format!(
            "{}int{bits}(0x{})",
            if *signed { "" } else { "u" },
            format_bytes(words)
        ),
        ConstScalar::FixedBytes(bytes) => format!("bytes(0x{})", format_bytes(bytes)),
        ConstScalar::Address { bits, bytes } => format!("address{bits}(0x{})", format_bytes(bytes)),
    }
}

fn format_runtime_instance<'db>(db: &'db dyn MirDb, instance: RuntimeInstance<'db>) -> String {
    let key = instance.key(db);
    match key.source(db) {
        crate::instance::RuntimeInstanceSource::Semantic(semantic) => {
            let owner = semantic.key(db).owner(db);
            let owner_summary = match owner {
                hir::analysis::ty::ty_check::BodyOwner::Func(func) => format!(
                    "func={} has_body={} trait={:?} impl_trait={:?} impl={:?} owner={owner:?}",
                    func.name(db)
                        .to_opt()
                        .map(|name| name.data(db).to_string())
                        .unwrap_or_else(|| "<unnamed>".to_string()),
                    func.body(db).is_some(),
                    func.containing_trait(db).and_then(|trait_| trait_
                        .name(db)
                        .to_opt()
                        .map(|name| name.data(db).to_string())),
                    func.containing_impl_trait(db)
                        .map(|impl_trait| impl_trait.ty(db).pretty_print(db).to_string()),
                    func.containing_impl(db)
                        .map(|impl_| impl_.ty(db).pretty_print(db).to_string()),
                ),
                _ => format!("{owner:?}"),
            };
            format!(
                "{key:?} [semantic {owner_summary} subst={:?}]",
                semantic.key(db).subst(db).generic_args(db),
            )
        }
        crate::instance::RuntimeInstanceSource::Synthetic(synthetic) => {
            format!("{key:?} [synthetic spec={:?}]", synthetic.spec(db))
        }
    }
}

fn format_space(space: AddressSpaceKind) -> &'static str {
    match space {
        AddressSpaceKind::Memory => "mem",
        AddressSpaceKind::Storage => "storage",
        AddressSpaceKind::Transient => "transient",
        AddressSpaceKind::Calldata => "calldata",
    }
}

fn format_local_id(local: RLocalId) -> String {
    format!("%{}", local.index())
}

fn format_bytes(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        let _ = write!(out, "{byte:02x}");
    }
    out
}
