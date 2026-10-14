//! Track validation dependencies without assigning a harmless effect to opaque calls.
use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SemOrigin, SemanticInstance,
        diagnostics::{SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan},
        normalized::{NExpr, NStatementKind},
    },
};
use cranelift_entity::EntityRef;

use super::{ir::PendingSemanticValidation, solver::Borrowck};

pub(super) fn can_specialize(db: &dyn HirAnalysisDb, instance: SemanticInstance<'_>) -> bool {
    instance
        .key(db)
        .subst(db)
        .generic_args(db)
        .iter()
        .any(|ty| ty.has_param(db) || ty.has_var(db) || ty.contains_assoc_ty_of_param(db))
        || crate::analysis::semantic::instance::has_unbound_evidence_calls(db, instance)
}

impl<'db> PendingSemanticValidation<'db> {
    pub(super) fn diagnostic(
        &self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> SemanticDiagnostic<'db> {
        let owner = instance.key(db).owner(db);
        let mut diagnostic = SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::UnresolvedCall,
            "executable calls require concrete implementations or verified effect contracts".into(),
            SemanticDiagnosticSpan::Origin {
                owner,
                origin: SemOrigin::Body(owner),
            },
        );
        for callee in &self.callees {
            let owner = callee.owner(db);
            diagnostic.push_secondary(
                "validation depends on this opaque callee".into(),
                SemanticDiagnosticSpan::Origin {
                    owner,
                    origin: SemOrigin::Body(owner),
                },
            );
        }
        diagnostic
    }
}

impl Borrowck<'_> {
    /// An unbounded opaque effect makes subsequent memory facts conditional.
    /// Keep checks before that effect, including the call's operand checks.
    pub(super) fn prepare_validation_dependencies(&mut self) {
        self.validation_dependencies = self
            .body
            .blocks
            .iter()
            .map(|block| vec![false; block.statements.len() + 1])
            .collect();
        if !can_specialize(self.db, self.instance) || self.pending.callees.is_empty() {
            return;
        }
        let mut entries = vec![false; self.body.blocks.len()];
        loop {
            let mut changed = false;
            for (index, block) in self.body.blocks.iter().enumerate() {
                let mut pending = entries[index];
                for (statement_index, statement) in block.statements.iter().enumerate() {
                    self.validation_dependencies[index][statement_index] = pending;
                    pending |= matches!(statement.kind,
                        NStatementKind::Define { result, expr: NExpr::Call { .. } }
                            if self.calls.get(&result).is_some_and(|call| call.pending));
                }
                self.validation_dependencies[index][block.statements.len()] = pending;
                for successor in block.terminator.kind.successors() {
                    if pending && !entries[successor.block.index()] {
                        entries[successor.block.index()] = true;
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
    }

    pub(super) fn call_validation_pending(&self, block: usize, index: usize) -> bool {
        can_specialize(self.db, self.instance)
            && matches!(self.body.blocks[block].statements[index].kind,
                NStatementKind::Define { result, expr: NExpr::Call { .. } }
                    if self.calls.get(&result).is_some_and(|call| call.pending))
    }
}

#[cfg(test)]
mod evidence_specialization_tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                EvalOutcome, GenericSubst, SemanticInstanceKey, check_semantic_borrows,
                eval_body_owner_const, get_or_build_semantic_instance,
                identity_semantic_instance_key,
            },
            ty::ty_check::BodyOwner,
        },
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn delivered_evidence_closes_concrete_call_validation() {
        let mut db = HirAnalysisTestDb::default();
        let source = "use core::obligations::{Evidence, prove}\ntrait Pick { fn value() -> u256 }\nstruct Subject {}\nimpl Pick for Subject { fn value() -> u256 { 1 } }\ntype Goal = Pick<Subject>\nconst fn choice() -> Evidence<Goal> { prove<Goal>() }\nfn consume(_ witness: Evidence<Goal>) -> u256 { with (witness) { <Subject as Pick>::value() } }\nfn ordinary() -> u256 { 1 }\n";
        let file = db.new_stand_alone("concrete-evidence-validation.fe".into(), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let owner = BodyOwner::Func(find_func(&db, module, "consume"));
        let key = identity_semantic_instance_key(&db, owner);
        let symbolic = get_or_build_semantic_instance(&db, key);
        assert!(can_specialize(&db, symbolic));
        assert!(matches!(
            check_semantic_borrows(&db, symbolic),
            Err(crate::analysis::semantic::SemanticAnalysisError::Pending(_))
        ));
        let factory = BodyOwner::Func(find_func(&db, module, "choice"));
        let EvalOutcome::Ready(value) = eval_body_owner_const(
            &db,
            factory,
            GenericSubst::for_body_owner(&db, factory, vec![]),
        ) else {
            panic!("issued value required")
        };
        let env = key
            .impl_env(&db)
            .with_argument_evidence_values(&db, &[(0, value)]);
        let key =
            SemanticInstanceKey::new(&db, owner, key.subst(&db), key.effect_providers(&db), env);
        let delivered = get_or_build_semantic_instance(&db, key);
        assert!(!can_specialize(&db, delivered));
        assert!(check_semantic_borrows(&db, delivered).is_ok());
        let ordinary = BodyOwner::Func(find_func(&db, module, "ordinary"));
        assert!(!can_specialize(
            &db,
            get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, ordinary))
        ));
    }

    #[test]
    fn nominal_default_body_does_not_close_pending_evidence_dispatch() {
        let mut db = HirAnalysisTestDb::default();
        let source = "use core::obligations::{Evidence, prove}\ntrait Pick { fn value() -> u256 { 999 } }\nstruct Subject {}\nimpl Pick for Subject { fn value() -> u256 { 1 } }\ntype Goal = Pick<Subject>\nconst fn choice() -> Evidence<Goal> { prove<Goal>() }\nfn consume(_ witness: Evidence<Goal>) -> u256 { with (witness) { <Subject as Pick>::value() } }\nfn relay(_ witness: Evidence<Goal>) -> u256 { consume(witness) }\n";
        let file = db.new_stand_alone("pending-evidence-default.fe".into(), source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        for name in ["consume", "relay"] {
            let owner = BodyOwner::Func(find_func(&db, module, name));
            let key = identity_semantic_instance_key(&db, owner);
            let symbolic = get_or_build_semantic_instance(&db, key);
            if name == "consume" {
                assert!(
                    symbolic
                        .callees(&db)
                        .iter()
                        .any(|callee| !callee.concrete_dispatch)
                );
            }
            assert!(matches!(
                check_semantic_borrows(&db, symbolic),
                Err(crate::analysis::semantic::SemanticAnalysisError::Pending(_))
            ));
            let factory = BodyOwner::Func(find_func(&db, module, "choice"));
            let EvalOutcome::Ready(value) = eval_body_owner_const(
                &db,
                factory,
                GenericSubst::for_body_owner(&db, factory, vec![]),
            ) else {
                panic!("issued value required")
            };
            let env = key
                .impl_env(&db)
                .with_argument_evidence_values(&db, &[(0, value)]);
            let key = SemanticInstanceKey::new(
                &db,
                owner,
                key.subst(&db),
                key.effect_providers(&db),
                env,
            );
            let delivered = get_or_build_semantic_instance(&db, key);
            assert!(
                delivered
                    .callees(&db)
                    .iter()
                    .all(|callee| callee.concrete_dispatch)
            );
            assert!(check_semantic_borrows(&db, delivered).is_ok());
        }
    }
}
