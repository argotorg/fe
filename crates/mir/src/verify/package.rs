use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        DispatchDefault, RExpr, RStmt, RTerminator, ResolvedCodeRegion, RuntimeCodeRegion,
        RuntimeFunctionOwner, RuntimeLinkage, RuntimeObject, RuntimePackage, RuntimeProgramView,
        RuntimeReturnPlan, RuntimeSyntheticSpec,
        code_region::{code_region_runtime_entry, code_region_section_name, code_region_symbol},
    },
    verify::{VerifyError, storage_layout::verify_contract_storage_seam, verify_runtime_body},
};

struct PackageView<'db> {
    db: &'db dyn MirDb,
    package: RuntimePackage<'db>,
}

impl<'db> RuntimeProgramView<'db> for PackageView<'db> {
    fn interface_signature(
        &self,
        id: crate::instance::RuntimeInstance<'db>,
    ) -> crate::runtime::RuntimeInterfaceSignature<'db> {
        id.interface_signature(self.db)
    }

    fn exit_behavior(
        &self,
        id: crate::instance::RuntimeInstance<'db>,
    ) -> crate::runtime::RuntimeExitBehavior {
        id.exit_behavior(self.db)
    }

    fn body(&self, id: crate::instance::RuntimeInstance<'db>) -> crate::runtime::RuntimeBody<'db> {
        id.body(self.db).clone()
    }

    fn layout(&self, id: crate::runtime::LayoutId<'db>) -> crate::runtime::Layout<'db> {
        id.data(self.db)
    }

    fn const_region(
        &self,
        id: crate::runtime::ConstRegionId<'db>,
    ) -> crate::runtime::ConstRegion<'db> {
        id.data(self.db)
    }

    fn code_region(&self, id: RuntimeCodeRegion<'db>) -> Option<ResolvedCodeRegion<'db>> {
        self.package
            .code_regions(self.db)
            .iter()
            .find(|region| region.region(self.db) == id)
            .copied()
    }
}

pub fn verify_runtime_package<'db>(
    db: &'db dyn MirDb,
    package: RuntimePackage<'db>,
) -> Result<(), VerifyError<'db>> {
    let view = PackageView { db, package };
    let functions = package.functions(db);
    let function_instances = functions
        .iter()
        .map(|function| function.instance(db))
        .collect::<FxHashSet<_>>();
    let objects = package.objects(db);
    let object_set = objects.iter().copied().collect::<FxHashSet<_>>();

    let mut seen_symbols = FxHashSet::default();
    for function in functions {
        if !seen_symbols.insert(function.symbol(db).clone()) {
            return Err(VerifyError::DuplicateRuntimeSymbol(
                function.symbol(db).clone(),
            ));
        }
        let owner = function.owner(db);
        verify_contract_storage_seam(db, &owner)?;
        if function.linkage(db) == RuntimeLinkage::External {
            continue;
        }
        let body = function.instance(db).body(db);
        verify_runtime_body(db, &view, &body)?;
        verify_code_region_refs(&view, &body)?;
        verify_synthetic_function(db, owner, &body)?;
    }
    for region in package.code_regions(db) {
        if !seen_symbols.insert(region.symbol(db).clone()) {
            return Err(VerifyError::DuplicateRuntimeSymbol(
                region.symbol(db).clone(),
            ));
        }
        verify_resolved_code_region(db, &region, &function_instances, &objects)?;
    }
    for &object in objects.iter() {
        verify_object(db, object, &function_instances, &objects)?;
    }
    for object in package.root_objects(db) {
        if !object_set.contains(&object) {
            return Err(VerifyError::InvalidPackageObject(object));
        }
    }
    if let Some(primary) = package.primary_object(db)
        && !package.root_objects(db).contains(&primary)
    {
        return Err(VerifyError::InvalidPackageObject(primary));
    }
    Ok(())
}

fn verify_code_region_refs<'db>(
    view: &PackageView<'db>,
    body: &crate::runtime::RuntimeBody<'db>,
) -> Result<(), VerifyError<'db>> {
    for block in &body.blocks {
        for stmt in &block.stmts {
            let RStmt::Assign { expr, .. } = stmt else {
                continue;
            };
            match expr {
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::CurrentCodeRegionLen) => {}
                RExpr::Builtin(
                    crate::runtime::RuntimeBuiltin::CodeRegionOffset { region }
                    | crate::runtime::RuntimeBuiltin::CodeRegionLen { region },
                ) if view.code_region(*region).is_none() => {
                    return Err(VerifyError::InvalidCodeRegion(*region));
                }
                _ => {}
            }
        }
    }
    Ok(())
}

fn verify_synthetic_function<'db>(
    db: &'db dyn MirDb,
    owner: RuntimeFunctionOwner<'db>,
    body: &crate::runtime::RuntimeBody<'db>,
) -> Result<(), VerifyError<'db>> {
    match owner {
        RuntimeFunctionOwner::Semantic(_) => Ok(()),
        RuntimeFunctionOwner::Synthetic(spec) => match spec {
            RuntimeSyntheticSpec::ContractRuntimeRoot {
                dispatch, default, ..
            } => {
                let Some(entry) = body.blocks.first() else {
                    return Err(VerifyError::InvalidReturnClass);
                };
                let (cases, default_bb) = match &entry.terminator {
                    RTerminator::SwitchScalar { cases, default, .. } => (cases, default),
                    RTerminator::Branch {
                        then_bb, else_bb, ..
                    } => {
                        let Some(selector_block) = body.block(*else_bb) else {
                            return Err(VerifyError::MissingRuntimeBlock(*else_bb));
                        };
                        let RTerminator::SwitchScalar {
                            cases,
                            default: default_bb,
                            ..
                        } = &selector_block.terminator
                        else {
                            return Err(VerifyError::InvalidReturnClass);
                        };
                        if then_bb != default_bb {
                            return Err(VerifyError::InvalidReturnClass);
                        }
                        (cases, default_bb)
                    }
                    _ => return Err(VerifyError::InvalidReturnClass),
                };
                if cases.len() != dispatch.len() {
                    return Err(VerifyError::InvalidReturnClass);
                }
                for ((_, block), arm) in cases.iter().zip(dispatch.iter()) {
                    let Some(target) = body.block(*block) else {
                        return Err(VerifyError::MissingRuntimeBlock(*block));
                    };
                    let RTerminator::TerminalCall { callee, args } = &target.terminator else {
                        return Err(VerifyError::InvalidReturnClass);
                    };
                    if *callee != arm.wrapper || !args.is_empty() {
                        return Err(VerifyError::InvalidReturnClass);
                    }
                }

                let Some(default_target) = body.block(*default_bb) else {
                    return Err(VerifyError::MissingRuntimeBlock(*default_bb));
                };
                match (default, &default_target.terminator) {
                    (DispatchDefault::RevertEmpty, RTerminator::RevertEmpty) => {}
                    (
                        DispatchDefault::Call { wrapper },
                        RTerminator::TerminalCall { callee, args },
                    ) if *callee == wrapper && args.is_empty() => {}
                    _ => return Err(VerifyError::InvalidReturnClass),
                }
                Ok(())
            }
            RuntimeSyntheticSpec::ContractInitRoot { .. } => {
                verify_has_terminator(body, |term| matches!(term, RTerminator::ReturnData { .. }))
            }
            RuntimeSyntheticSpec::ContractRecvAbi { plan } => {
                if matches!(plan.ret, RuntimeReturnPlan::Value { .. }) {
                    return verify_value_recv_exits(db, body, &plan);
                }
                if body
                    .blocks
                    .iter()
                    .any(|block| matches!(block.terminator, RTerminator::TerminalCall { .. }))
                {
                    return Err(VerifyError::InvalidReturnClass);
                }
                verify_has_terminator(body, |term| {
                    matches!(
                        term,
                        RTerminator::ReturnData { .. }
                            | RTerminator::TerminalCall { .. }
                            | RTerminator::Revert { .. }
                            | RTerminator::RevertEmpty
                    )
                })
            }
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ManualContractRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. } => Ok(()),
        },
    }
}

/// Synthetic recv wrappers keep the handler call and return preparation in one
/// block. Follow only value-preserving carrier conversions in that block, so a
/// same-class decoded input cannot stand in for the handler result or host.
fn verify_recv_return_args<'db>(
    db: &'db dyn MirDb,
    body: &crate::runtime::RuntimeBody<'db>,
    block: &crate::runtime::RBlock<'db>,
    plan: &crate::runtime::ContractRecvAbiPlan<'db>,
    args: &[crate::runtime::RLocalId],
) -> bool {
    use crate::runtime::{
        PlaceRoot, RLocalId, RuntimeBuiltin, RuntimeMemoryLayout,
        TargetRootProviderMaterialization, lower::interface::runtime_visible_binding_plans,
    };
    use hir::analysis::ty::ty_check::LocalBinding;

    let RuntimeReturnPlan::Value {
        host, return_value, ..
    } = &plan.ret
    else {
        return false;
    };
    let Some(semantic) = return_value.key(db).semantic(db) else {
        return false;
    };
    let bindings = runtime_visible_binding_plans(db, semantic);
    if args.len() != bindings.len() {
        return false;
    }
    let source = |mut value: RLocalId| {
        let mut before = block.stmts.len();
        loop {
            let (index, expr) = block.stmts[..before].iter().enumerate().rev().find_map(
                |(i, stmt)| match stmt {
                    RStmt::Assign { dst, expr } if *dst == value => Some((i, expr)),
                    _ => None,
                },
            )?;
            // The generated return preparation does not mutate the source
            // through a place after defining it.
            if block.stmts[index + 1..].iter().any(|stmt| match stmt {
                RStmt::Store { dst, .. } | RStmt::CopyInto { dst, .. } => match dst.root {
                    PlaceRoot::Slot(root) | PlaceRoot::Ref(root) => root == value,
                    PlaceRoot::Ptr { addr, .. } => addr == value,
                    PlaceRoot::Provider(_) => false,
                },
                _ => false,
            }) {
                return None;
            }
            value = match expr {
                RExpr::Use(src)
                | RExpr::MaterializeToObject { src }
                | RExpr::NativeRef { value: src }
                | RExpr::RetagRef { value: src }
                | RExpr::ProviderRefFromRaw { raw: src, .. }
                | RExpr::ProviderRefToRaw { value: src } => *src,
                RExpr::AddrOf { place } | RExpr::Load { place } if place.path.is_empty() => {
                    match place.root {
                        PlaceRoot::Slot(root) | PlaceRoot::Ref(root) => root,
                        PlaceRoot::Ptr { addr, .. } => addr,
                        PlaceRoot::Provider(_) => return None,
                    }
                }
                _ => return Some((value, expr, index)),
            };
            before = index;
        }
    };
    args.iter().zip(bindings).all(|(&arg, binding)| {
        let Some((local, expr, index)) = source(arg) else {
            return false;
        };
        match binding.binding {
            LocalBinding::Param { idx: 1, .. } => matches!(expr,
                RExpr::Call { callee, .. }
                    if callee.key(db).semantic(db).is_some()
                        && callee.key(db).semantic(db) == plan.user_recv.key(db).semantic(db)),
            LocalBinding::Param { idx: 0, .. } => {
                if body
                    .local(local)
                    .is_none_or(|local| local.semantic_ty != host.declared_ty)
                {
                    return false;
                }
                match (&host.materialization, expr) {
                    (
                        TargetRootProviderMaterialization::MemoryObject { layout },
                        RExpr::AllocObject { layout: actual },
                    ) => layout == actual,
                    (
                        TargetRootProviderMaterialization::MemoryRawAddr { layout },
                        RExpr::Builtin(RuntimeBuiltin::Malloc { size }),
                    ) => {
                        let Ok(expected_size) = RuntimeMemoryLayout::raw(db).layout_size(*layout)
                        else {
                            return false;
                        };
                        block.stmts[..index]
                            .iter()
                            .rev()
                            .find_map(|stmt| match stmt {
                                RStmt::Assign { dst, expr } if dst == size => Some(expr),
                                _ => None,
                            })
                            == Some(&RExpr::ConstScalar(crate::runtime::ConstScalar::Int {
                                bits: 256,
                                signed: false,
                                words: expected_size
                                    .to_be_bytes()
                                    .into_iter()
                                    .skip_while(|byte| *byte == 0)
                                    .collect(),
                            }))
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    })
}

fn verify_value_recv_exits<'db>(
    db: &'db dyn MirDb,
    body: &crate::runtime::RuntimeBody<'db>,
    plan: &crate::runtime::ContractRecvAbiPlan<'db>,
) -> Result<(), VerifyError<'db>> {
    use crate::runtime::RBlockId;
    let RuntimeReturnPlan::Value { return_value, .. } = plan.ret else {
        unreachable!()
    };
    let expected = return_value.key(db).semantic(db);
    let mut pending = vec![RBlockId::from_u32(0)];
    let mut visited = FxHashSet::default();
    let mut has_return = false;
    while let Some(id) = pending.pop() {
        if !visited.insert(id) {
            continue;
        }
        let block = body.block(id).ok_or(VerifyError::MissingRuntimeBlock(id))?;
        match &block.terminator {
            RTerminator::Goto(next) => pending.push(*next),
            RTerminator::Branch {
                then_bb, else_bb, ..
            } => pending.extend([*then_bb, *else_bb]),
            RTerminator::SwitchScalar { cases, default, .. } => {
                pending.extend(cases.iter().map(|(_, target)| *target));
                pending.push(*default);
            }
            RTerminator::MatchEnumTag { cases, default, .. } => {
                pending.extend(cases.iter().map(|(_, target)| *target));
                pending.extend(default);
            }
            RTerminator::TerminalCall { callee, args }
                if expected.is_some()
                    && callee.key(db).semantic(db) == expected
                    && verify_recv_return_args(db, body, block, plan, args) =>
            {
                has_return = true;
            }
            RTerminator::Revert { .. } | RTerminator::RevertEmpty | RTerminator::Trap => {}
            _ => return Err(VerifyError::InvalidReturnClass),
        }
    }
    if has_return {
        Ok(())
    } else {
        Err(VerifyError::InvalidReturnClass)
    }
}

fn verify_has_terminator<'db>(
    body: &crate::runtime::RuntimeBody<'db>,
    pred: impl Fn(&RTerminator<'db>) -> bool,
) -> Result<(), VerifyError<'db>> {
    if body.blocks.iter().any(|block| pred(&block.terminator)) {
        Ok(())
    } else {
        Err(VerifyError::InvalidReturnClass)
    }
}

fn verify_object<'db>(
    db: &'db dyn MirDb,
    object: RuntimeObject<'db>,
    function_instances: &FxHashSet<crate::instance::RuntimeInstance<'db>>,
    objects: &[RuntimeObject<'db>],
) -> Result<(), VerifyError<'db>> {
    for section in object.sections(db) {
        if !function_instances.contains(&section.entry.instance(db)) {
            return Err(VerifyError::InvalidPackageFunction(
                section.entry.instance(db),
            ));
        }
        for embed in &section.embeds {
            let source_object_name = embed.source.object();
            let source_section = embed.source.section();
            let Some(source_object) = resolve_package_object(db, objects, source_object_name)
            else {
                return Err(VerifyError::UnknownPackageObject(
                    source_object_name.to_string(),
                ));
            };
            if !source_object
                .sections(db)
                .iter()
                .any(|candidate| candidate.name == *source_section)
            {
                return Err(VerifyError::InvalidPackageSection(
                    source_object,
                    source_section.clone(),
                ));
            }
            if matches!(
                &embed.source,
                crate::runtime::RuntimeSectionRef::Local { .. }
            ) && source_object_name == object.name(db)
                && *source_section == section.name
            {
                return Err(VerifyError::InvalidPackageSection(
                    object,
                    section.name.clone(),
                ));
            }
        }
    }
    Ok(())
}

fn verify_resolved_code_region<'db>(
    db: &'db dyn MirDb,
    region: &ResolvedCodeRegion<'db>,
    function_instances: &FxHashSet<crate::instance::RuntimeInstance<'db>>,
    objects: &[RuntimeObject<'db>],
) -> Result<(), VerifyError<'db>> {
    if !function_instances.contains(&region.root(db).instance(db)) {
        return Err(VerifyError::InvalidPackageFunction(
            region.root(db).instance(db),
        ));
    }
    let source = region.source(db);
    let Some(object) = resolve_package_object(db, objects, source.object()) else {
        return Err(VerifyError::UnknownPackageObject(
            source.object().to_string(),
        ));
    };
    if !object
        .sections(db)
        .iter()
        .any(|candidate| candidate.name == *source.section())
    {
        return Err(VerifyError::InvalidPackageSection(
            object,
            source.section().clone(),
        ));
    }
    if matches!(
        region.region(db).key(db),
        crate::runtime::RuntimeCodeRegionKey::ManualContractRoot { .. }
    ) {
        let expected_entry = code_region_runtime_entry(db, region.region(db))
            .ok_or_else(|| VerifyError::InvalidCodeRegion(region.region(db)))?;
        if region.root(db).instance(db) != expected_entry {
            return Err(VerifyError::InvalidCodeRegion(region.region(db)));
        }
        let expected_symbol = code_region_symbol(db, region.region(db));
        if region.symbol(db) != expected_symbol {
            return Err(VerifyError::InvalidCodeRegion(region.region(db)));
        }
        let expected_section = code_region_section_name(db, region.region(db))
            .ok_or_else(|| VerifyError::InvalidCodeRegion(region.region(db)))?;
        if *source.section() != expected_section {
            return Err(VerifyError::InvalidCodeRegion(region.region(db)));
        }
    }
    Ok(())
}

fn resolve_package_object<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
    name: &str,
) -> Option<RuntimeObject<'db>> {
    objects
        .iter()
        .find(|candidate| candidate.name(db) == name)
        .copied()
}

#[cfg(test)]
mod tests {
    use super::*;
    use common::InputDb;
    use cranelift_entity::EntityRef;
    use driver::DriverDataBase;
    use url::Url;

    #[test]
    fn recv_terminal_calls_must_use_the_planned_semantic_return_helper() {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///recv_return_verifier.fe").unwrap(),
            Some(
                r#"
use std::abi::sol
msg M {
    #[selector = sol("wide()")]
    Wide -> String<8>,
    #[selector = sol("narrow()")]
    Narrow -> String<4>,
    #[selector = sol("paid()")]
    Paid -> String<8>,
    #[selector = sol("scalar(uint256)")]
    Scalar { value: u256 } -> u256,
}
pub contract C {
    recv M {
        Wide -> String<8> { "COOL" }
        Narrow -> String<4> { "COOL" }
        #[payable]
        Paid -> String<8> { "COOL" }
        #[payable]
        Scalar { value } -> u256 { value + 1 }
    }
}
"#
                .to_string(),
            ),
        );
        let package = crate::build_runtime_package(&db, db.top_mod(file)).unwrap();
        let functions = package.functions(&db);
        let wrappers: Vec<_> = functions
            .iter()
            .filter(|f| {
                matches!(
                    f.owner(&db),
                    RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { .. })
                )
            })
            .collect();
        assert_eq!(wrappers.len(), 4);
        let root = functions
            .iter()
            .find(|f| {
                matches!(
                    f.owner(&db),
                    RuntimeFunctionOwner::Synthetic(
                        RuntimeSyntheticSpec::ContractRuntimeRoot { .. }
                    )
                )
            })
            .unwrap()
            .instance(&db);
        let view = PackageView { db: &db, package };
        let mut guarded = false;
        let mut payable = false;
        let mut checked_decoded_input = false;
        let mut checked_specialization = false;
        for wrapper in &wrappers {
            let owner = wrapper.owner(&db);
            let original = wrapper.instance(&db).body(&db).clone();
            assert!(verify_synthetic_function(&db, owner.clone(), &original).is_ok());
            guarded |= original
                .blocks
                .iter()
                .any(|b| matches!(b.terminator, RTerminator::RevertEmpty));
            payable |= !original
                .blocks
                .iter()
                .any(|b| matches!(b.terminator, RTerminator::RevertEmpty));
            let index = original
                .blocks
                .iter()
                .position(|b| matches!(b.terminator, RTerminator::TerminalCall { .. }))
                .unwrap();
            let RTerminator::TerminalCall { callee, ref args } = original.blocks[index].terminator
            else {
                unreachable!()
            };
            let other = wrappers
                .iter()
                .filter_map(|f| {
                    f.instance(&db)
                        .body(&db)
                        .blocks
                        .iter()
                        .find_map(|b| match b.terminator {
                            RTerminator::TerminalCall { callee: other, .. }
                                if other.key(&db).semantic(&db)
                                    != callee.key(&db).semantic(&db)
                                    && other.interface_signature(&db)
                                        == callee.interface_signature(&db) =>
                            {
                                Some(other)
                            }
                            _ => None,
                        })
                })
                .next();
            for replacement in [
                RTerminator::ReturnData {
                    offset: crate::runtime::RLocalId::from_u32(0),
                    len: crate::runtime::RLocalId::from_u32(0),
                },
                RTerminator::RevertEmpty,
                RTerminator::Stop,
            ] {
                let mut body = original.clone();
                body.blocks[index].terminator = replacement;
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
            }
            // A matching helper in dead code must not conceal a successful
            // entry path that skips ABI encoding (or a revert-only wrapper).
            for replacement in [
                RTerminator::Stop,
                RTerminator::Return(None),
                RTerminator::RevertEmpty,
            ] {
                let mut body = original.clone();
                body.blocks.push(body.blocks[index].clone());
                body.blocks[index].terminator = replacement;
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
            }
            let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { ref plan }) =
                owner
            else {
                unreachable!()
            };
            for (revert_index, _) in original
                .blocks
                .iter()
                .enumerate()
                .filter(|(_, block)| matches!(block.terminator, RTerminator::RevertEmpty))
            {
                let mut body = original.clone();
                body.blocks[revert_index].terminator = RTerminator::Stop;
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
            }
            for stmt in &original.blocks[index].stmts {
                if let RStmt::Assign {
                    dst,
                    expr:
                        RExpr::Call {
                            callee,
                            args: inputs,
                        },
                } = stmt
                    && callee.key(&db).semantic(&db) == plan.user_recv.key(&db).semantic(&db)
                    && let Some(&input) = inputs.first()
                    && original.value_class(input) == original.value_class(*dst)
                    && let Some(position) = args.iter().position(|arg| arg == dst)
                {
                    let mut body = original.clone();
                    let RTerminator::TerminalCall { args, .. } = &mut body.blocks[index].terminator
                    else {
                        unreachable!()
                    };
                    args[position] = input;
                    assert!(verify_runtime_body(&db, &view, &body).is_ok());
                    assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
                    checked_decoded_input = true;
                }
            }
            // Remove the user handler call while keeping its result's carrier.
            let mut body = original.clone();
            let mut replaced = false;
            for stmt in &mut body.blocks[index].stmts {
                if let RStmt::Assign {
                    dst,
                    expr: RExpr::Call { callee, .. },
                } = stmt
                    && callee.key(&db).semantic(&db) == plan.user_recv.key(&db).semantic(&db)
                {
                    *stmt = RStmt::Assign {
                        dst: *dst,
                        expr: RExpr::Placeholder {
                            class: body.locals[dst.index()]
                                .carrier
                                .value_class()
                                .unwrap()
                                .clone(),
                        },
                    };
                    replaced = true;
                }
            }
            assert!(replaced);
            assert!(verify_runtime_body(&db, &view, &body).is_ok());
            assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());

            // Substituting another value of the same carrier remains valid
            // runtime MIR, but violates the wrapper's semantic return plan.
            for (arg_index, &arg) in args.iter().enumerate() {
                let mut body = original.clone();
                let local = body.locals[arg.index()].clone();
                let replacement = crate::runtime::RLocalId::from_u32(body.locals.len() as u32);
                body.locals.push(local.clone());
                body.blocks[index].stmts.push(RStmt::Assign {
                    dst: replacement,
                    expr: RExpr::Placeholder {
                        class: local.carrier.value_class().unwrap().clone(),
                    },
                });
                let RTerminator::TerminalCall { args, .. } = &mut body.blocks[index].terminator
                else {
                    unreachable!()
                };
                args[arg_index] = replacement;
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
            }
            checked_specialization |= other.is_some();
            let replacements = std::iter::once((root, Box::default()))
                .chain(other.map(|other| (other, args.clone())));
            for (wrong, wrong_args) in replacements {
                let mut body = original.clone();
                body.blocks[index].terminator = RTerminator::TerminalCall {
                    callee: wrong,
                    args: wrong_args,
                };
                // These calls are structurally valid; only the wrapper's return
                // plan reveals the wrong helper or generic specialization.
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
            }
        }
        assert!(guarded && payable && checked_decoded_input && checked_specialization);
    }
}
