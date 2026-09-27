use hir::hir_def::expr::{BinOp, CompBinOp};
use rustc_hash::FxHashSet;

use crate::{
    db::MirDb,
    instance::RuntimeInstance,
    runtime::{
        ConstScalar, DispatchDefault, RBlock, RExpr, RLocalId, RStmt, RTerminator,
        ResolvedCodeRegion, RuntimeBuiltin, RuntimeCodeRegion, RuntimeFunctionOwner,
        RuntimeLinkage, RuntimeObject, RuntimePackage, RuntimeProgramView, RuntimeReturnPlan,
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
                // A recv wrapper is straight-line code: a nonpayable wrapper's
                // entry block reverts when `callvalue != 0` and otherwise jumps
                // to the block that calls the handler once and exits through the
                // planned ABI return; a payable wrapper does all of this in its
                // entry block. Unit arms return no data. Other operands are fixed
                // by construction, so this checks the control flow, payment
                // policy, return and callees the plan prescribes. Call
                // preparation may specialize runtime carriers, so callees are
                // compared by semantic instance, which retains generic arguments
                // such as the returned type.
                let semantic = |callee: RuntimeInstance<'db>| callee.key(db).semantic(db);
                let is_zero = |def: Option<(usize, &RExpr<'db>)>| {
                    matches!(
                        def,
                        Some((_, RExpr::ConstScalar(ConstScalar::Int { words, .. })))
                            if words.iter().all(|byte| *byte == 0)
                    )
                };
                let handler_calls = |block: &RBlock<'db>| {
                    block
                        .stmts
                        .iter()
                        .filter(|stmt| {
                            matches!(stmt, RStmt::Assign { expr: RExpr::Call { callee, .. }, .. }
                                if semantic(*callee) == semantic(plan.user_recv))
                        })
                        .count()
                };
                let Some(entry) = body.blocks.first() else {
                    return Err(VerifyError::InvalidRecvAbiWrapper);
                };
                let exit = if plan.payable {
                    entry
                } else {
                    let RTerminator::Branch {
                        cond,
                        then_bb,
                        else_bb,
                    } = entry.terminator
                    else {
                        return Err(VerifyError::InvalidRecvAbiWrapper);
                    };
                    let guarded = match last_assignment(&entry.stmts, cond) {
                        Some((
                            idx,
                            RExpr::Binary {
                                op: BinOp::Comp(CompBinOp::NotEq),
                                lhs,
                                rhs,
                            },
                        )) => {
                            matches!(
                                last_assignment(&entry.stmts[..idx], *lhs),
                                Some((_, RExpr::Builtin(RuntimeBuiltin::CallValue)))
                            ) && is_zero(last_assignment(&entry.stmts[..idx], *rhs))
                        }
                        _ => false,
                    };
                    let reverts = body
                        .block(then_bb)
                        .is_some_and(|block| block.terminator == RTerminator::RevertEmpty);
                    if !guarded || !reverts || handler_calls(entry) != 0 {
                        return Err(VerifyError::InvalidRecvAbiWrapper);
                    }
                    body.block(else_bb)
                        .ok_or(VerifyError::MissingRuntimeBlock(else_bb))?
                };
                let returns = match (&plan.ret, &exit.terminator) {
                    // The exit block runs after the entry block (they are the
                    // same block when payable), so its assignments are later.
                    (RuntimeReturnPlan::Unit, RTerminator::ReturnData { len, .. }) => is_zero(
                        last_assignment(&exit.stmts, *len)
                            .or_else(|| last_assignment(&entry.stmts, *len)),
                    ),
                    (
                        RuntimeReturnPlan::Value { return_value, .. },
                        RTerminator::TerminalCall { callee, .. },
                    ) => semantic(*callee) == semantic(*return_value),
                    _ => false,
                };
                if returns && handler_calls(exit) == 1 {
                    Ok(())
                } else {
                    Err(VerifyError::InvalidRecvAbiWrapper)
                }
            }
            RuntimeSyntheticSpec::MainRoot { .. }
            | RuntimeSyntheticSpec::TestRoot { .. }
            | RuntimeSyntheticSpec::ManualContractRoot { .. }
            | RuntimeSyntheticSpec::ContractInitAbi { .. } => Ok(()),
        },
    }
}

/// The latest assignment to `local` in `stmts`, with its statement index.
fn last_assignment<'a, 'db>(
    stmts: &'a [RStmt<'db>],
    local: RLocalId,
) -> Option<(usize, &'a RExpr<'db>)> {
    stmts
        .iter()
        .enumerate()
        .rev()
        .find_map(|(idx, stmt)| match stmt {
            RStmt::Assign { dst, expr } if *dst == local => Some((idx, expr)),
            _ => None,
        })
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
    use crate::runtime::{
        ContractRecvAbiPlan, RBlockId, RuntimeBody, synthetic::lower_synthetic_runtime_body,
    };
    use common::InputDb;
    use cranelift_entity::EntityRef;
    use driver::DriverDataBase;
    use url::Url;

    const SOURCE_URL: &str = "file:///recv_wrapper_verifier.fe";

    fn recv_test_db() -> DriverDataBase {
        let mut db = DriverDataBase::default();
        db.workspace().touch(
            &mut db,
            Url::parse(SOURCE_URL).unwrap(),
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
    #[selector = sol("ping()")]
    Ping,
    #[selector = sol("tip()")]
    Tip,
}
pub contract C {
    recv M {
        Wide -> String<8> { "COOL" }
        Narrow -> String<4> { "COOL" }
        #[payable]
        Paid -> String<8> { "COOL" }
        Ping {}
        #[payable]
        Tip {}
    }
}
"#
                .to_string(),
            ),
        );
        db
    }

    type RecvWrapper<'db> = (
        RuntimeFunctionOwner<'db>,
        ContractRecvAbiPlan<'db>,
        RuntimeBody<'db>,
    );

    fn recv_wrappers(db: &DriverDataBase) -> (RuntimePackage<'_>, Vec<RecvWrapper<'_>>) {
        let file = db
            .workspace()
            .get(db, &Url::parse(SOURCE_URL).unwrap())
            .unwrap();
        let package = crate::build_runtime_package(db, db.top_mod(file)).unwrap();
        let wrappers =
            package
                .functions(db)
                .iter()
                .filter_map(|function| {
                    let owner = function.owner(db);
                    let RuntimeFunctionOwner::Synthetic(RuntimeSyntheticSpec::ContractRecvAbi {
                        plan,
                    }) = &owner
                    else {
                        return None;
                    };
                    let plan = plan.clone();
                    Some((owner, plan, function.instance(db).body(db).clone()))
                })
                .collect();
        (package, wrappers)
    }

    #[test]
    fn recv_wrappers_exit_through_the_planned_return() {
        let db = recv_test_db();
        let (package, wrappers) = recv_wrappers(&db);
        let view = PackageView { db: &db, package };
        let root = package
            .functions(&db)
            .iter()
            .find(|function| {
                matches!(
                    function.owner(&db),
                    RuntimeFunctionOwner::Synthetic(
                        RuntimeSyntheticSpec::ContractRuntimeRoot { .. }
                    )
                )
            })
            .unwrap()
            .instance(&db);
        let helpers: Vec<_> = wrappers
            .iter()
            .filter_map(|(_, _, body)| {
                body.blocks.iter().find_map(|block| match block.terminator {
                    RTerminator::TerminalCall { callee, .. } => Some(callee),
                    _ => None,
                })
            })
            .collect();
        let (mut units, mut values) = (false, false);
        for (owner, plan, original) in &wrappers {
            assert_eq!(
                verify_synthetic_function(&db, owner.clone(), original),
                Ok(())
            );
            let exit = original
                .blocks
                .iter()
                .position(|block| {
                    matches!(
                        block.terminator,
                        RTerminator::ReturnData { .. } | RTerminator::TerminalCall { .. }
                    )
                })
                .unwrap();
            let mut wrong = vec![
                RTerminator::Stop,
                RTerminator::Return(None),
                RTerminator::RevertEmpty,
                RTerminator::Goto(RBlockId::from_u32(exit as u32)),
            ];
            match (&plan.ret, &original.blocks[exit].terminator) {
                (RuntimeReturnPlan::Unit, RTerminator::ReturnData { offset, len }) => {
                    units = true;
                    wrong.extend(helpers.iter().map(|&callee| RTerminator::TerminalCall {
                        callee,
                        args: Box::default(),
                    }));
                    // A structurally valid non-empty return reveals memory
                    // instead of the unit arm's empty return data.
                    let mut body = original.clone();
                    let size = RLocalId::from_u32(body.locals.len() as u32);
                    body.locals.push(body.locals[len.index()].clone());
                    body.blocks[exit].stmts.push(RStmt::Assign {
                        dst: size,
                        expr: RExpr::ConstScalar(ConstScalar::Int {
                            bits: 256,
                            signed: false,
                            words: vec![32],
                        }),
                    });
                    body.blocks[exit].terminator = RTerminator::ReturnData {
                        offset: *offset,
                        len: size,
                    };
                    assert_eq!(verify_runtime_body(&db, &view, &body), Ok(()));
                    assert_eq!(
                        verify_synthetic_function(&db, owner.clone(), &body),
                        Err(VerifyError::InvalidRecvAbiWrapper)
                    );
                }
                (RuntimeReturnPlan::Value { .. }, RTerminator::TerminalCall { callee, args }) => {
                    values = true;
                    wrong.push(RTerminator::ReturnData {
                        offset: RLocalId::from_u32(0),
                        len: RLocalId::from_u32(0),
                    });
                    let other = helpers
                        .iter()
                        .copied()
                        .find(|other| other.key(&db).semantic(&db) != callee.key(&db).semantic(&db))
                        .unwrap();
                    for (callee, args) in [(root, Box::default()), (other, args.clone())] {
                        // These calls are structurally valid; only the wrapper's
                        // return plan reveals the wrong helper or specialization.
                        let mut body = original.clone();
                        body.blocks[exit].terminator = RTerminator::TerminalCall { callee, args };
                        assert_eq!(verify_runtime_body(&db, &view, &body), Ok(()));
                        wrong.push(body.blocks[exit].terminator.clone());
                    }
                }
                _ => panic!("recv wrapper should exit through its planned return"),
            }
            for terminator in wrong {
                // A dead copy of the planned exit must not conceal a wrong
                // reachable one.
                let mut body = original.clone();
                body.blocks.push(body.blocks[exit].clone());
                body.blocks[exit].terminator = terminator;
                assert_eq!(
                    verify_synthetic_function(&db, owner.clone(), &body),
                    Err(VerifyError::InvalidRecvAbiWrapper)
                );
            }
        }
        assert!(units && values);
    }

    #[test]
    fn recv_wrappers_enforce_the_planned_payment_policy() {
        let db = recv_test_db();
        let (package, wrappers) = recv_wrappers(&db);
        let view = PackageView { db: &db, package };
        let (mut payable, mut nonpayable) = (false, false);
        for (owner, plan, original) in &wrappers {
            // A well-formed wrapper built with the opposite payment policy.
            let mut mutated = vec![
                lower_synthetic_runtime_body(
                    &db,
                    original.owner,
                    RuntimeSyntheticSpec::ContractRecvAbi {
                        plan: ContractRecvAbiPlan {
                            payable: !plan.payable,
                            ..plan.clone()
                        },
                    },
                )
                .unwrap(),
            ];
            if plan.payable {
                payable = true;
            } else {
                nonpayable = true;
                let RTerminator::Branch {
                    cond,
                    then_bb,
                    else_bb,
                } = original.blocks[0].terminator
                else {
                    panic!("nonpayable recv wrapper should start with its payment guard")
                };
                // Skip or invert the guard.
                for terminator in [
                    RTerminator::Goto(else_bb),
                    RTerminator::Branch {
                        cond,
                        then_bb: else_bb,
                        else_bb: then_bb,
                    },
                ] {
                    let mut body = original.clone();
                    body.blocks[0].terminator = terminator;
                    mutated.push(body);
                }
                // Let the guard's failure path succeed.
                let mut body = original.clone();
                body.blocks[then_bb.index()].terminator = RTerminator::Stop;
                mutated.push(body);
                // Compare a constant instead of the call value.
                let mut body = original.clone();
                for stmt in &mut body.blocks[0].stmts {
                    if let RStmt::Assign { expr, .. } = stmt
                        && matches!(expr, RExpr::Builtin(RuntimeBuiltin::CallValue))
                    {
                        *expr = RExpr::ConstScalar(ConstScalar::Int {
                            bits: 256,
                            signed: false,
                            words: Vec::new(),
                        });
                    }
                }
                assert_ne!(body.blocks, original.blocks);
                mutated.push(body);
            }
            for body in mutated {
                assert_eq!(verify_runtime_body(&db, &view, &body), Ok(()));
                assert_eq!(
                    verify_synthetic_function(&db, owner.clone(), &body),
                    Err(VerifyError::InvalidRecvAbiWrapper)
                );
            }
        }
        assert!(payable && nonpayable);
    }

    #[test]
    fn recv_wrappers_call_their_handler_once_after_the_payment_guard() {
        let db = recv_test_db();
        let (_, wrappers) = recv_wrappers(&db);
        for (owner, plan, original) in &wrappers {
            let handler = plan.user_recv.key(&db).semantic(&db);
            let (block, stmt) = original
                .blocks
                .iter()
                .enumerate()
                .find_map(|(block, data)| {
                    data.stmts
                        .iter()
                        .position(|stmt| {
                            matches!(stmt, RStmt::Assign { expr: RExpr::Call { callee, .. }, .. }
                                if callee.key(&db).semantic(&db) == handler)
                        })
                        .map(|stmt| (block, stmt))
                })
                .expect("recv wrapper should call its handler");
            let call = original.blocks[block].stmts[stmt].clone();
            let mut omitted = original.clone();
            omitted.blocks[block].stmts.remove(stmt);
            let mut repeated = original.clone();
            repeated.blocks[block].stmts.insert(stmt, call.clone());
            let mut mutated = vec![omitted.clone(), repeated];
            if !plan.payable {
                let mut early = omitted;
                early.blocks[0].stmts.insert(0, call);
                mutated.push(early);
            }
            for body in mutated {
                assert_eq!(
                    verify_synthetic_function(&db, owner.clone(), &body),
                    Err(VerifyError::InvalidRecvAbiWrapper)
                );
            }
        }
    }
}
