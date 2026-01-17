use crate::span::DynLazySpan;
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{Body, Const, Contract, EffectParamListId, Func, PathId, scope_graph::ScopeId},
    span::item::{LazyContractRecvSpan, LazyRecvArmSpan},
};
use crate::analysis::ty::ty_def::TyId;
use salsa::Update;

/// Identifies the HIR owner of a [`Body`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum BodyOwner<'db> {
    Func(Func<'db>),
    Const(Const<'db>),
    AnonConstBody { body: Body<'db>, expected: TyId<'db> },
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

/// Identifies the HIR owner of an effect parameter list (`uses (...)`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub enum EffectParamOwner<'db> {
    Func(Func<'db>),
    Contract(Contract<'db>),
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

impl<'db> EffectParamOwner<'db> {
    pub fn effects(self, db: &'db dyn HirAnalysisDb) -> EffectParamListId<'db> {
        match self {
            EffectParamOwner::Func(func) => func.effects(db),
            EffectParamOwner::Contract(contract) => contract.effects(db),
            EffectParamOwner::ContractInit { contract } => contract
                .init(db)
                .map(|init| init.effects(db))
                .unwrap_or_else(|| EffectParamListId::new(db, Vec::new())),
            EffectParamOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract
                .recv_arm(db, recv_idx as usize, arm_idx as usize)
                .map(|arm| arm.effects)
                .unwrap_or_else(|| EffectParamListId::new(db, Vec::new())),
        }
    }

    pub fn effect_param_path_span(
        self,
        _db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> DynLazySpan<'db> {
        match self {
            EffectParamOwner::Func(func) => func.span().effects().param_idx(idx).path().into(),
            EffectParamOwner::Contract(contract) => {
                contract.span().effects().param_idx(idx).path().into()
            }
            EffectParamOwner::ContractInit { contract } => contract
                .span()
                .init_block()
                .effects()
                .param_idx(idx)
                .path()
                .into(),
            EffectParamOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract
                .span()
                .recv(recv_idx as usize)
                .arms()
                .arm(arm_idx as usize)
                .effects()
                .param_idx(idx)
                .path()
                .into(),
        }
    }
}

impl<'db> BodyOwner<'db> {
    pub fn recv_arm(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::hir_def::ContractRecvArm<'db>> {
        match self {
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => None,
            BodyOwner::ContractInit { .. } => None,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract.recv_arm(db, recv_idx as usize, arm_idx as usize),
            _ => None,
        }
    }

    pub fn recv_msg_path(self, db: &'db dyn HirAnalysisDb) -> Option<PathId<'db>> {
        match self {
            BodyOwner::ContractRecvArm {
                contract, recv_idx, ..
            } => contract.recvs(db).data(db).get(recv_idx as usize)?.msg_path,
            _ => None,
        }
    }

    pub fn body(self, db: &'db dyn HirAnalysisDb) -> Option<Body<'db>> {
        match self {
            BodyOwner::Func(func) => func.body(db),
            BodyOwner::Const(const_) => const_.body(db).to_opt(),
            BodyOwner::AnonConstBody { body, .. } => Some(body),
            BodyOwner::ContractInit { contract } => Some(contract.init(db)?.body(db)),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => Some(
                contract
                    .recv_arm(db, recv_idx as usize, arm_idx as usize)?
                    .body,
            ),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            BodyOwner::Func(func) => func.scope(),
            BodyOwner::Const(const_) => const_.scope(),
            BodyOwner::AnonConstBody { body, .. } => body.scope(),
            BodyOwner::ContractInit { contract } => contract.scope(),
            BodyOwner::ContractRecvArm { contract, .. } => contract.scope(),
        }
    }

    pub fn effects(self, db: &'db dyn HirAnalysisDb) -> EffectParamListId<'db> {
        match self {
            BodyOwner::Func(func) => func.effects(db),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => {
                EffectParamListId::new(db, Vec::new())
            }
            BodyOwner::ContractInit { contract } => contract
                .init(db)
                .map(|init| init.effects(db))
                .unwrap_or_else(|| EffectParamListId::new(db, Vec::new())),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => contract
                .recv_arm(db, recv_idx as usize, arm_idx as usize)
                .map(|arm| arm.effects)
                .unwrap_or_else(|| EffectParamListId::new(db, Vec::new())),
        }
    }

    pub fn effect_param_path_span(
        self,
        _db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> DynLazySpan<'db> {
        match self {
            BodyOwner::Func(func) => func.span().effects().param_idx(idx).path().into(),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => DynLazySpan::invalid(),
            BodyOwner::ContractInit { contract } => contract
                .span()
                .init_block()
                .effects()
                .param_idx(idx)
                .path()
                .into(),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => contract
                .span()
                .recv(recv_idx as usize)
                .arms()
                .arm(arm_idx as usize)
                .effects()
                .param_idx(idx)
                .path()
                .into(),
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
