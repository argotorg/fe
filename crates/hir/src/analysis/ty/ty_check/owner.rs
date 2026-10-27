use crate::analysis::ty::ty_def::{BorrowKind, ClosureDef, ClosureTy, TyId};
use crate::hir_def::{
    Expr, ExprId, ItemKind, Partial, attr::ArithmeticMode, params::FuncParamMode,
};
use crate::span::{DynLazySpan, types::LazyTySpan};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{Body, Const, Contract, EffectParamListId, Func, scope_graph::ScopeId},
    span::item::LazyRecvArmSpan,
};
use salsa::SalsaValue;

/// Identifies the HIR owner of a [`Body`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum BodyOwner<'db> {
    /// A function's body, at the mode it is checked at if it is a `mut?`
    /// function: one body per mode.
    Func(Func<'db>, Option<BorrowKind>),
    Const(Const<'db>),
    AnonConstBody {
        body: Body<'db>,
        expected: TyId<'db>,
    },
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
    /// The body of a closure called through a callable trait whose
    /// receiver, `self` or `mut self`, is `receiver`: one body per receiver
    /// mode it is called with.
    Closure {
        ty: ClosureTy<'db>,
        def: ClosureDef<'db>,
        receiver: FuncParamMode,
    },
}

/// Identifies the HIR owner of an effect parameter list (`uses (...)`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
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
            EffectParamOwner::ContractInit { contract } => contract.init(db).map_or_else(
                || EffectParamListId::new(db, Vec::new()),
                |init| init.effects(db),
            ),
            EffectParamOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract
                .recv_arm(db, recv_idx as usize, arm_idx as usize)
                .map_or_else(|| EffectParamListId::new(db, Vec::new()), |arm| arm.effects),
        }
    }

    pub fn effect_param_ty_span(self, db: &'db dyn HirAnalysisDb, idx: usize) -> LazyTySpan<'db> {
        match self {
            EffectParamOwner::Func(func) => func
                .span()
                .effects()
                .param_idx(func.effect_origin(db, idx))
                .ty(),
            EffectParamOwner::Contract(contract) => contract.span().effects().param_idx(idx).ty(),
            EffectParamOwner::ContractInit { contract } => {
                contract.span().init_block().effects().param_idx(idx).ty()
            }
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
                .ty(),
        }
    }
}

impl<'db> BodyOwner<'db> {
    /// The owner of `closure`'s body; `def` is always the closure type's.
    pub fn closure(
        db: &'db dyn HirAnalysisDb,
        closure: ClosureTy<'db>,
        receiver: FuncParamMode,
    ) -> Self {
        Self::Closure {
            ty: closure,
            def: closure.def(db),
            receiver,
        }
    }

    /// The bodies of `func`: one per mode a `mut?` function is checked at,
    /// and one for any other function.
    pub fn func_bodies(
        db: &'db dyn HirAnalysisDb,
        func: Func<'db>,
    ) -> impl Iterator<Item = Self> + 'db {
        func.modes(db)
            .iter()
            .map(move |&mode| Self::Func(func, mode))
    }

    /// The body of `func` that questions every mode answers alike read: a
    /// `mut?` function's `ref` body.
    pub fn func(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Self {
        Self::Func(func, func.modes(db)[0])
    }

    /// The body of `func` a call at `mode` runs: a `mut?` function's body at
    /// that mode, and the one body of any other function, such as either
    /// body of a method with one body per mode.
    pub fn func_at(db: &'db dyn HirAnalysisDb, func: Func<'db>, mode: Option<BorrowKind>) -> Self {
        Self::Func(func, if func.is_maybe_mut(db) { mode } else { None })
    }

    /// The mode this body is checked at: a `mut?` function's.
    pub fn mode(self) -> Option<BorrowKind> {
        match self {
            Self::Func(_, mode) => mode,
            _ => None,
        }
    }

    /// The owner of the item body that defines the closure `def`: a `mut?`
    /// function's body at the mode `def` was defined at.
    pub fn closure_parent(db: &'db dyn HirAnalysisDb, def: ClosureDef<'db>) -> Option<Self> {
        let body = def.body;
        if let Some(func) = body.containing_func(db) {
            return Some(Self::Func(func, def.mode));
        }
        match body.scope().parent_item(db)? {
            ItemKind::Const(const_) if const_.body(db).to_opt() == Some(body) => {
                Some(Self::Const(const_))
            }
            ItemKind::Contract(contract) => {
                if contract.init(db).is_some_and(|init| init.body(db) == body) {
                    return Some(Self::ContractInit { contract });
                }
                contract
                    .recvs(db)
                    .data(db)
                    .iter()
                    .enumerate()
                    .find_map(|(recv_idx, recv)| {
                        let arm_idx = recv.arms.data(db).iter().position(|arm| arm.body == body)?;
                        Some(Self::ContractRecvArm {
                            contract,
                            recv_idx: recv_idx as u32,
                            arm_idx: arm_idx as u32,
                        })
                    })
            }
            _ => None,
        }
    }

    /// The closure whose body this owner is, which only the expressions and
    /// statements of its region (`ty::closure::closure_regions`) are part of.
    pub fn closure_region(self) -> Option<ExprId> {
        match self {
            Self::Closure { def, .. } => Some(def.expr),
            _ => None,
        }
    }

    /// The expression whose value the body returns: a closure's body
    /// expression, or the whole body.
    pub fn root_expr(self, db: &'db dyn HirAnalysisDb) -> Option<ExprId> {
        match self {
            Self::Closure { def, .. } => match def.expr.data(db, def.body) {
                Partial::Present(Expr::Closure { body, .. }) => Some(*body),
                _ => None,
            },
            _ => self.body(db).map(|body| body.expr(db)),
        }
    }

    /// A `where` clause condition, checked as a `bool` constant.
    pub fn const_predicate(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> Self {
        Self::AnonConstBody {
            body,
            expected: TyId::bool(db),
        }
    }

    /// Whether this owner is a `where` clause condition.
    pub fn is_const_predicate(self, db: &'db dyn HirAnalysisDb) -> bool {
        let Self::AnonConstBody { body, expected } = self else {
            return false;
        };
        expected == TyId::bool(db)
            && body
                .scope()
                .parent_item(db)
                .and_then(crate::hir_def::WhereClauseOwner::from_item_opt)
                .is_some_and(|owner| owner.where_clause(db).const_predicates(db).contains(&body))
    }

    /// The `where` clause conditions `item` declares, as body owners.
    pub fn const_predicates_of(
        db: &'db dyn HirAnalysisDb,
        item: ItemKind<'db>,
    ) -> impl Iterator<Item = Self> + 'db {
        crate::hir_def::WhereClauseOwner::from_item_opt(item)
            .into_iter()
            .flat_map(move |owner| {
                owner
                    .where_clause(db)
                    .const_predicates(db)
                    .iter()
                    .map(move |&body| Self::const_predicate(db, body))
            })
    }

    /// The arithmetic mode the body is written in, which selects what its
    /// arithmetic operators do on overflow. A closure's is its parent's.
    pub fn arithmetic_mode(self, db: &'db dyn HirAnalysisDb) -> ArithmeticMode {
        match self {
            Self::Func(func, _) => return func.arithmetic_mode(db),
            Self::Closure { def, .. } if let Some(parent) = Self::closure_parent(db, def) => {
                return parent.arithmetic_mode(db);
            }
            _ => {}
        }

        let mut scope = self.scope().parent_module(db);
        while let Some(module_scope) = scope {
            if let ItemKind::Mod(mod_) = module_scope.item()
                && let Some(mode) = mod_.arithmetic_mode(db)
            {
                return mode;
            }
            scope = module_scope.parent_module(db);
        }

        let top_mod = self.scope().top_mod(db);
        if let Some(mode) = top_mod.attributes(db).arithmetic_mode(db) {
            return mode;
        }
        if let Some(mode) = top_mod.ingot(db).arithmetic_mode(db) {
            return match mode {
                common::config::ArithmeticMode::Checked => ArithmeticMode::Checked,
                common::config::ArithmeticMode::Unchecked => ArithmeticMode::Unchecked,
            };
        }

        ArithmeticMode::Checked
    }

    pub fn recv_arm(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<crate::hir_def::ContractRecvArm<'db>> {
        match self {
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                None
            }
            BodyOwner::ContractInit { .. } => None,
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => contract.recv_arm(db, recv_idx as usize, arm_idx as usize),
            _ => None,
        }
    }

    pub fn body(self, db: &'db dyn HirAnalysisDb) -> Option<Body<'db>> {
        match self {
            BodyOwner::Func(func, _) => func.body(db),
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
            BodyOwner::Closure { def, .. } => Some(def.body),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            BodyOwner::Func(func, _) => func.scope(),
            BodyOwner::Const(const_) => const_.scope(),
            BodyOwner::AnonConstBody { body, .. } => body.scope(),
            BodyOwner::ContractInit { contract } => contract.scope(),
            BodyOwner::ContractRecvArm { contract, .. } => contract.scope(),
            BodyOwner::Closure { def, .. } => def.body.scope(),
        }
    }

    pub fn effects(self, db: &'db dyn HirAnalysisDb) -> EffectParamListId<'db> {
        match self {
            BodyOwner::Func(func, _) => func.effects(db),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                EffectParamListId::new(db, Vec::new())
            }
            BodyOwner::ContractInit { contract } => contract.init(db).map_or_else(
                || EffectParamListId::new(db, Vec::new()),
                |init| init.effects(db),
            ),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
                ..
            } => contract
                .recv_arm(db, recv_idx as usize, arm_idx as usize)
                .map_or_else(|| EffectParamListId::new(db, Vec::new()), |arm| arm.effects),
        }
    }

    pub fn effect_param_ty_span(self, db: &'db dyn HirAnalysisDb, idx: usize) -> DynLazySpan<'db> {
        match self {
            BodyOwner::Func(func, _) => func
                .span()
                .effects()
                .param_idx(func.effect_origin(db, idx))
                .ty()
                .into(),
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                DynLazySpan::invalid()
            }
            BodyOwner::ContractInit { contract } => contract
                .span()
                .init_block()
                .effects()
                .param_idx(idx)
                .ty()
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
                .ty()
                .into(),
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
