use crate::{
    analysis::HirAnalysisDb,
    hir_def::{Body, Contract, ContractRecvArm, Func, PathId, scope_graph::ScopeId},
    span::item::{LazyContractRecvSpan, LazyRecvArmSpan},
};

/// Identifies the HIR owner of a [`Body`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BodyOwner<'db> {
    Func(Func<'db>),
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
        msg_path: Option<PathId<'db>>,
        arm: ContractRecvArm<'db>,
    },
}

impl<'db> BodyOwner<'db> {
    pub fn body(self, db: &'db dyn HirAnalysisDb) -> Option<Body<'db>> {
        match self {
            BodyOwner::Func(func) => func.body(db),
            BodyOwner::ContractRecvArm { arm, .. } => Some(arm.body),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            BodyOwner::Func(func) => func.scope(),
            BodyOwner::ContractRecvArm { contract, .. } => contract.scope(),
        }
    }

    pub fn recv_span(self) -> Option<LazyContractRecvSpan<'db>> {
        match self {
            BodyOwner::ContractRecvArm {
                contract, recv_idx, ..
            } => Some(contract.span().recv(recv_idx as usize)),
            _ => None,
        }
    }

    pub fn recv_arm_span(self) -> Option<LazyRecvArmSpan<'db>> {
        match self {
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => Some(
                contract
                    .span()
                    .recv(recv_idx as usize)
                    .arms()
                    .arm(arm_idx as usize),
            ),
            _ => None,
        }
    }
}
