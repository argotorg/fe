use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    runtime::{
        DispatchDefault, RExpr, RStmt, RTerminator, ResolvedCodeRegion, RuntimeCodeRegion,
        RuntimeFunctionOwner, RuntimeLinkage, RuntimeObject, RuntimePackage, RuntimeProgramView,
        RuntimeSyntheticSpec,
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
                verify_recv_wrapper_plan(db, body, plan)
            }
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ManualContractRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. } => Ok(()),
        },
    }
}

/// Package verification runs before optimization: recv wrappers must be the
/// canonical expansion of their ABI plan. Re-expand from the plan (not the
/// cached instance body) so checking a modified body cannot validate it against
/// itself. This includes decoded inputs, effect/layout arguments, the payment
/// guard, and every statement between the handler and its return helper.
///
/// Comparing the executable structure also rejects writes through fresh aliases
/// and additional calls with side effects, without an incomplete alias analysis.
/// Source origins are diagnostic metadata and do not participate in this check.
fn verify_recv_wrapper_plan<'db>(
    db: &'db dyn MirDb,
    body: &crate::runtime::RuntimeBody<'db>,
    plan: crate::runtime::ContractRecvAbiPlan<'db>,
) -> Result<(), VerifyError<'db>> {
    let expected = crate::runtime::synthetic::lower_synthetic_runtime_body(
        db,
        body.owner,
        RuntimeSyntheticSpec::ContractRecvAbi { plan },
    )
    .map_err(|_| VerifyError::InvalidReturnClass)?;
    if body.key != expected.key
        || body.signature != expected.signature
        || body.provider_bindings != expected.provider_bindings
        || body.locals != expected.locals
        || body.blocks != expected.blocks
    {
        return Err(VerifyError::InvalidReturnClass);
    }
    Ok(())
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

    fn recv_test_db() -> DriverDataBase {
        let mut db = DriverDataBase::default();
        db.workspace().touch(
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
    #[selector = sol("aggregate()")]
    Aggregate -> [u256; 2],
}
pub contract C {
    count: u256,
    recv M {
        Wide -> String<8> { "COOL" }
        Narrow -> String<4> { "COOL" }
        #[payable]
        Paid -> String<8> { "COOL" }
        #[payable]
        Scalar { value } -> u256 uses (count) { value + count }
        #[payable]
        Aggregate -> [u256; 2] { [1, 2] }
    }
}
"#
                .to_string(),
            ),
        );
        db
    }

    fn recv_test_package(db: &DriverDataBase) -> RuntimePackage<'_> {
        let file = db
            .workspace()
            .get(db, &Url::parse("file:///recv_return_verifier.fe").unwrap())
            .unwrap();
        crate::build_runtime_package(db, db.top_mod(file)).unwrap()
    }

    #[test]
    fn recv_terminal_calls_must_use_the_planned_semantic_return_helper() {
        let db = recv_test_db();
        let package = recv_test_package(&db);
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
        assert_eq!(wrappers.len(), 5);
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

    #[test]
    fn recv_rejects_writes_through_return_aliases() {
        use crate::runtime::{PlaceRoot, RLocalId, RuntimeClass, RuntimePlace};
        let db = recv_test_db();
        let package = recv_test_package(&db);
        let view = PackageView { db: &db, package };
        let mut checked = 0;
        for wrapper in package.functions(&db) {
            let owner = wrapper.owner(&db);
            let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { ref plan }) =
                owner
            else {
                continue;
            };
            let original = wrapper.instance(&db).body(&db).clone();
            for (block_index, block) in original.blocks.iter().enumerate() {
                for stmt in &block.stmts {
                    let RStmt::Assign {
                        dst: result,
                        expr: RExpr::Call { callee, .. },
                    } = stmt
                    else {
                        continue;
                    };
                    if callee.key(&db).semantic(&db) != plan.user_recv.key(&db).semantic(&db)
                        || matches!(original.value_class(*result), Some(RuntimeClass::Scalar(_)))
                    {
                        continue;
                    }
                    // Add a fresh alias that is not on the return argument's
                    // backwards provenance chain, then overwrite through it.
                    for copy in [false, true] {
                        let mut body = original.clone();
                        let alias = RLocalId::from_u32(body.locals.len() as u32);
                        body.locals.push(body.locals[result.index()].clone());
                        let replacement = RLocalId::from_u32(body.locals.len() as u32);
                        body.locals.push(body.locals[result.index()].clone());
                        let class = body.value_class(*result).unwrap().clone();
                        let stmts = &mut body.blocks[block_index].stmts;
                        stmts.push(RStmt::Assign {
                            dst: alias,
                            expr: RExpr::Use(*result),
                        });
                        stmts.push(RStmt::Assign {
                            dst: replacement,
                            expr: RExpr::Placeholder { class },
                        });
                        let dst = RuntimePlace {
                            root: PlaceRoot::Ref(alias),
                            path: Box::default(),
                        };
                        stmts.push(if copy {
                            RStmt::CopyInto {
                                dst,
                                src: replacement,
                            }
                        } else {
                            RStmt::Store {
                                dst,
                                src: replacement,
                            }
                        });
                        assert!(verify_runtime_body(&db, &view, &body).is_ok());
                        assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
                        checked += 1;
                    }
                }
            }
        }
        assert_eq!(checked, 2);
    }

    #[test]
    fn recv_rejects_wrong_handler_inputs() {
        use crate::runtime::RLocalId;
        let db = recv_test_db();
        let package = recv_test_package(&db);
        let view = PackageView { db: &db, package };
        let mut checked = false;
        for wrapper in package.functions(&db) {
            let owner = wrapper.owner(&db);
            let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { ref plan }) =
                owner
            else {
                continue;
            };
            let mut body = wrapper.instance(&db).body(&db).clone();
            let zero = RLocalId::from_u32(0);
            let zero_class = body.value_class(zero).unwrap().clone();
            let mut replaced = false;
            for block in &mut body.blocks {
                for stmt in &mut block.stmts {
                    if let RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } = stmt
                        && callee.key(&db).semantic(&db) == plan.user_recv.key(&db).semantic(&db)
                        && let Some(arg) = args.first_mut()
                        && body.locals[arg.index()].carrier.value_class() == Some(&zero_class)
                    {
                        assert_ne!(*arg, zero);
                        *arg = zero;
                        replaced = true;
                    }
                }
            }
            if replaced {
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner, &body).is_err());
                checked = true;
            }
        }
        assert!(checked);
    }

    #[test]
    fn recv_rejects_wrong_handler_effect_arguments() {
        use crate::runtime::RLocalId;
        let db = recv_test_db();
        let package = recv_test_package(&db);
        let view = PackageView { db: &db, package };
        let mut checked = 0;
        for wrapper in package.functions(&db) {
            let owner = wrapper.owner(&db);
            let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { ref plan }) =
                owner
            else {
                continue;
            };
            if plan.entry_args.effects.is_empty() {
                continue;
            }
            let original = wrapper.instance(&db).body(&db).clone();
            for (bi, block) in original.blocks.iter().enumerate() {
                for (si, stmt) in block.stmts.iter().enumerate() {
                    let RStmt::Assign {
                        expr: RExpr::Call { callee, args },
                        ..
                    } = stmt
                    else {
                        continue;
                    };
                    if callee.key(&db).semantic(&db) != plan.user_recv.key(&db).semantic(&db) {
                        continue;
                    }
                    // This fixture supplies a decoded scalar followed by its
                    // storage effect. A same-class placeholder is not the
                    // planned storage binding.
                    assert_eq!(args.len(), 2);
                    let mut body = original.clone();
                    let replacement = RLocalId::from_u32(body.locals.len() as u32);
                    let local = body.locals[args[1].index()].clone();
                    let class = local.carrier.value_class().unwrap().clone();
                    body.locals.push(local);
                    if let RStmt::Assign {
                        expr: RExpr::Call { args, .. },
                        ..
                    } = &mut body.blocks[bi].stmts[si]
                    {
                        args[1] = replacement;
                    }
                    body.blocks[bi].stmts.insert(
                        si,
                        RStmt::Assign {
                            dst: replacement,
                            expr: RExpr::Placeholder { class },
                        },
                    );
                    assert!(verify_runtime_body(&db, &view, &body).is_ok());
                    assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
                    checked += 1;
                }
            }
        }
        assert_eq!(checked, 1);
    }

    #[test]
    fn recv_rejects_missing_inverted_or_unplanned_payment_guards() {
        let db = recv_test_db();
        let package = recv_test_package(&db);
        let view = PackageView { db: &db, package };
        let mut checked = 0;
        for wrapper in package.functions(&db) {
            let owner = wrapper.owner(&db);
            let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi { ref plan }) =
                owner
            else {
                continue;
            };
            let original = wrapper.instance(&db).body(&db).clone();
            assert!(verify_synthetic_function(&db, owner.clone(), &original).is_ok());
            if let RTerminator::Branch {
                cond,
                then_bb,
                else_bb,
            } = original.blocks[0].terminator
            {
                for replacement in [
                    RTerminator::Goto(else_bb),
                    RTerminator::Branch {
                        cond,
                        then_bb: else_bb,
                        else_bb: then_bb,
                    },
                ] {
                    let mut body = original.clone();
                    body.blocks[0].terminator = replacement;
                    assert!(verify_runtime_body(&db, &view, &body).is_ok());
                    assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
                    checked += 1;
                }
            }
            if !plan.payable {
                let mut body = original.clone();
                let mut replaced = false;
                for stmt in &mut body.blocks[0].stmts {
                    if let RStmt::Assign { expr, .. } = stmt
                        && matches!(
                            expr,
                            RExpr::Builtin(crate::runtime::RuntimeBuiltin::CallValue)
                        )
                    {
                        *expr = RExpr::ConstScalar(crate::runtime::ConstScalar::Int {
                            bits: 256,
                            signed: false,
                            words: Vec::new(),
                        });
                        replaced = true;
                    }
                }
                assert!(replaced);
                assert!(verify_runtime_body(&db, &view, &body).is_ok());
                assert!(verify_synthetic_function(&db, owner.clone(), &body).is_err());
                checked += 1;
            }
            // A well-typed wrapper built with the opposite payment policy
            // must not satisfy the original plan (in either direction).
            let mut wrong_plan = plan.clone();
            wrong_plan.payable = !wrong_plan.payable;
            let body = crate::runtime::synthetic::lower_synthetic_runtime_body(
                &db,
                original.owner,
                RuntimeSyntheticSpec::ContractRecvAbi { plan: wrong_plan },
            )
            .unwrap();
            assert!(verify_runtime_body(&db, &view, &body).is_ok());
            assert!(verify_synthetic_function(&db, owner, &body).is_err());
            checked += 1;
        }
        assert_eq!(checked, 11);
    }
}
