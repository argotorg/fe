use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        DispatchArm, DispatchDefault, RBlockId, RExpr, RStmt, RTerminator, ResolvedCodeRegion,
        RuntimeBody, RuntimeCodeRegion, RuntimeFunctionOwner, RuntimeObject, RuntimePackage,
        RuntimeProgramView, RuntimeSyntheticSpec,
        code_region::{code_region_runtime_entry, code_region_section_name, code_region_symbol},
    },
    verify::{VerifyError, verify_runtime_body},
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
        let body = function.instance(db).body(db);
        verify_runtime_body(db, &view, &body)?;
        verify_code_region_refs(&view, &body)?;
        verify_synthetic_function(function.owner(db), &body)?;
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
    owner: RuntimeFunctionOwner<'db>,
    body: &crate::runtime::RuntimeBody<'db>,
) -> Result<(), VerifyError<'db>> {
    match owner {
        RuntimeFunctionOwner::Semantic(_) => Ok(()),
        RuntimeFunctionOwner::Synthetic(spec) => match spec {
            RuntimeSyntheticSpec::ContractRuntimeRoot {
                dispatch, default, ..
            } => verify_contract_runtime_dispatch(body, &dispatch, default),
            RuntimeSyntheticSpec::ContractInitRoot { .. } => {
                verify_has_terminator(body, |term| matches!(term, RTerminator::ReturnData { .. }))
            }
            RuntimeSyntheticSpec::ContractRecvAbi { .. } => verify_has_terminator(body, |term| {
                matches!(
                    term,
                    RTerminator::ReturnData { .. } | RTerminator::Revert { .. }
                )
            }),
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ManualContractRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. }
            | RuntimeSyntheticSpec::CodeRegionRoot { .. } => Ok(()),
        },
    }
}

fn verify_contract_runtime_dispatch<'db>(
    body: &RuntimeBody<'db>,
    dispatch: &[DispatchArm<'db>],
    default: DispatchDefault<'db>,
) -> Result<(), VerifyError<'db>> {
    let Some(entry) = body.blocks.first() else {
        return Err(VerifyError::InvalidReturnClass);
    };

    if let RTerminator::SwitchScalar {
        cases,
        default: default_bb,
        ..
    } = &entry.terminator
    {
        return verify_contract_runtime_switch(body, dispatch, default, cases, *default_bb);
    }

    let mut seen_blocks = FxHashSet::default();
    let mut seen_arms = vec![false; dispatch.len()];
    let mut saw_default = false;
    verify_contract_runtime_dispatch_block(
        body,
        RBlockId::from_u32(0),
        dispatch,
        default,
        &mut seen_arms,
        &mut saw_default,
        &mut seen_blocks,
    )?;

    if saw_default && seen_arms.into_iter().all(|seen| seen) {
        Ok(())
    } else {
        Err(VerifyError::InvalidReturnClass)
    }
}

fn verify_contract_runtime_switch<'db>(
    body: &RuntimeBody<'db>,
    dispatch: &[DispatchArm<'db>],
    default: DispatchDefault<'db>,
    cases: &[(crate::runtime::ConstScalar, RBlockId)],
    default_bb: RBlockId,
) -> Result<(), VerifyError<'db>> {
    if cases.len() != dispatch.len() {
        return Err(VerifyError::InvalidReturnClass);
    }
    for ((_, block), arm) in cases.iter().zip(dispatch.iter()) {
        verify_contract_runtime_arm_target(body, *block, arm)?;
    }

    verify_contract_runtime_default_target(body, default_bb, default)
}

fn verify_contract_runtime_dispatch_block<'db>(
    body: &RuntimeBody<'db>,
    block: RBlockId,
    dispatch: &[DispatchArm<'db>],
    default: DispatchDefault<'db>,
    seen_arms: &mut [bool],
    saw_default: &mut bool,
    seen_blocks: &mut FxHashSet<RBlockId>,
) -> Result<(), VerifyError<'db>> {
    if !seen_blocks.insert(block) {
        return Ok(());
    }

    let Some(target) = body.block(block) else {
        return Err(VerifyError::MissingRuntimeBlock(block));
    };
    match &target.terminator {
        RTerminator::Goto(target) => verify_contract_runtime_dispatch_block(
            body,
            *target,
            dispatch,
            default,
            seen_arms,
            saw_default,
            seen_blocks,
        ),
        RTerminator::Branch {
            then_bb, else_bb, ..
        } => {
            verify_contract_runtime_dispatch_block(
                body,
                *then_bb,
                dispatch,
                default,
                seen_arms,
                saw_default,
                seen_blocks,
            )?;
            verify_contract_runtime_dispatch_block(
                body,
                *else_bb,
                dispatch,
                default,
                seen_arms,
                saw_default,
                seen_blocks,
            )
        }
        RTerminator::TerminalCall { callee, args } if args.is_empty() => {
            if let Some(idx) = dispatch.iter().position(|arm| arm.wrapper == *callee) {
                seen_arms[idx] = true;
                Ok(())
            } else if matches!(default, DispatchDefault::Call { wrapper } if wrapper == *callee) {
                *saw_default = true;
                Ok(())
            } else {
                Err(VerifyError::InvalidReturnClass)
            }
        }
        RTerminator::Revert { .. } if matches!(default, DispatchDefault::RevertEmpty) => {
            *saw_default = true;
            Ok(())
        }
        _ => Err(VerifyError::InvalidReturnClass),
    }
}

fn verify_contract_runtime_arm_target<'db>(
    body: &RuntimeBody<'db>,
    block: RBlockId,
    arm: &DispatchArm<'db>,
) -> Result<(), VerifyError<'db>> {
    let Some(target) = body.block(block) else {
        return Err(VerifyError::MissingRuntimeBlock(block));
    };
    let RTerminator::TerminalCall { callee, args } = &target.terminator else {
        return Err(VerifyError::InvalidReturnClass);
    };
    if *callee != arm.wrapper || !args.is_empty() {
        return Err(VerifyError::InvalidReturnClass);
    }
    Ok(())
}

fn verify_contract_runtime_default_target<'db>(
    body: &RuntimeBody<'db>,
    block: RBlockId,
    default: DispatchDefault<'db>,
) -> Result<(), VerifyError<'db>> {
    let Some(target) = body.block(block) else {
        return Err(VerifyError::MissingRuntimeBlock(block));
    };
    match (default, &target.terminator) {
        (DispatchDefault::RevertEmpty, RTerminator::Revert { .. }) => Ok(()),
        (DispatchDefault::Call { wrapper }, RTerminator::TerminalCall { callee, args })
            if *callee == wrapper && args.is_empty() =>
        {
            Ok(())
        }
        _ => Err(VerifyError::InvalidReturnClass),
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
            match &embed.source {
                crate::runtime::RuntimeSectionRef::Local {
                    object: source_object,
                    section: source_section,
                }
                | crate::runtime::RuntimeSectionRef::External {
                    object: source_object,
                    section: source_section,
                } => {
                    let Some(source_object) = resolve_package_object(db, objects, *source_object)
                    else {
                        return Err(VerifyError::InvalidPackageObject(*source_object));
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
                        crate::runtime::RuntimeSectionRef::Local {
                            object: source_object,
                            section: source_section,
                        } if source_object.name(db) == object.name(db) && *source_section == section.name
                    ) {
                        return Err(VerifyError::InvalidPackageSection(
                            object,
                            section.name.clone(),
                        ));
                    }
                }
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
    match region.source(db) {
        crate::runtime::RuntimeSectionRef::Local {
            object,
            ref section,
        }
        | crate::runtime::RuntimeSectionRef::External {
            object,
            ref section,
        } => {
            let Some(object) = resolve_package_object(db, objects, object) else {
                return Err(VerifyError::InvalidPackageObject(object));
            };
            if !object
                .sections(db)
                .iter()
                .any(|candidate| candidate.name == *section)
            {
                return Err(VerifyError::InvalidPackageSection(object, section.clone()));
            }
        }
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
        let source_section = match region.source(db).clone() {
            crate::runtime::RuntimeSectionRef::Local { section, .. }
            | crate::runtime::RuntimeSectionRef::External { section, .. } => section,
        };
        if source_section != expected_section {
            return Err(VerifyError::InvalidCodeRegion(region.region(db)));
        }
    }
    Ok(())
}

fn resolve_package_object<'db>(
    db: &'db dyn MirDb,
    objects: &[RuntimeObject<'db>],
    object: RuntimeObject<'db>,
) -> Option<RuntimeObject<'db>> {
    objects
        .iter()
        .find(|candidate| candidate.name(db) == object.name(db))
        .copied()
}
