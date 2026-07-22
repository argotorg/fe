use rustc_hash::FxHashSet;

use crate::analysis::semantic::{
    SBlockId, SLocalId, SStmtKind, STerminatorKind, SemanticBody, VariantIndex,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticVerifyError {
    MissingSemanticBlock(SBlockId),
    MissingSemanticLocal(SLocalId),
    DuplicateSemanticEntryLocal(SLocalId),
    DuplicateSemanticVariantCase(VariantIndex),
}

pub fn verify_semantic_body<'db>(body: &SemanticBody<'db>) -> Result<(), SemanticVerifyError> {
    let mut entry_locals = FxHashSet::default();
    for local in &body.entry_locals {
        if body.local(*local).is_none() {
            return Err(SemanticVerifyError::MissingSemanticLocal(*local));
        }
        if !entry_locals.insert(*local) {
            return Err(SemanticVerifyError::DuplicateSemanticEntryLocal(*local));
        }
    }
    for block in &body.blocks {
        for stmt in &block.stmts {
            match &stmt.kind {
                SStmtKind::Assign { dst, .. } => {
                    if body.local(*dst).is_none() {
                        return Err(SemanticVerifyError::MissingSemanticLocal(*dst));
                    }
                }
                SStmtKind::Store { dst, src } => {
                    if body.local(dst.local).is_none() {
                        return Err(SemanticVerifyError::MissingSemanticLocal(dst.local));
                    }
                    if body.local(src.value).is_none() {
                        return Err(SemanticVerifyError::MissingSemanticLocal(src.value));
                    }
                }
            }
        }

        if let STerminatorKind::MatchEnum { cases, .. } = &block.terminator.kind {
            let mut seen = FxHashSet::default();
            for (variant, target) in cases.iter().copied() {
                if !seen.insert(variant) {
                    return Err(SemanticVerifyError::DuplicateSemanticVariantCase(variant));
                }
                if body.block(target).is_none() {
                    return Err(SemanticVerifyError::MissingSemanticBlock(target));
                }
            }
        }
    }

    Ok(())
}
