use crate::{
    analysis::HirAnalysisDb,
    hir_def::scope_graph::ScopeId,
    hir_def::{Body, Contract, ContractRecvArm, Func, PathId},
};

/// Identifies the HIR owner of a [`Body`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BodyOwner<'db> {
    Func(Func<'db>),
    ContractInit(Contract<'db>),
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

#[derive(Clone, Debug)]
pub struct RecvArmInfo<'db> {
    pub contract: Contract<'db>,
    pub recv_idx: u32,
    pub _arm_idx: u32,
    pub msg_path: Option<PathId<'db>>,
    pub arm: ContractRecvArm<'db>,
}

impl<'db> BodyOwner<'db> {
    pub fn body(self, db: &'db dyn HirAnalysisDb) -> Option<Body<'db>> {
        match self {
            BodyOwner::Func(func) => func.body(db),
            BodyOwner::ContractInit(contract) => contract.init_body(db),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => {
                let recv = contract.recvs(db).data(db).get(recv_idx as usize)?;
                recv.arms.data(db).get(arm_idx as usize).map(|arm| arm.body)
            }
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            BodyOwner::Func(func) => func.scope(),
            BodyOwner::ContractInit(contract) => contract.scope(),
            BodyOwner::ContractRecvArm { contract, .. } => contract.scope(),
        }
    }

    pub fn recv_arm_info(self, db: &'db dyn HirAnalysisDb) -> Option<RecvArmInfo<'db>> {
        match self {
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => {
                let recv = contract.recvs(db).data(db).get(recv_idx as usize)?;
                let arm = recv.arms.data(db).get(arm_idx as usize)?.clone();
                Some(RecvArmInfo {
                    contract,
                    recv_idx,
                    _arm_idx: arm_idx,
                    msg_path: recv.msg_path,
                    arm,
                })
            }
            _ => None,
        }
    }
}
