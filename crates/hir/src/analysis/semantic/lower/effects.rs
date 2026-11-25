use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        place::PlaceBase,
        semantic::{
            Mutability, SEffectArg, SExpr, SLocalId, SOperand, SPlace, SStmtKind, SValueId,
            SemOrigin, instance::CallEffectArg, provisional_provider_binding_for_instance_effect,
            provisional_provider_idx_for_requirement,
            resolved_provider_binding_for_instance_effect,
        },
        ty::{
            AddressSpace,
            effects::EffectKeyKind,
            ty_check::{BodyOwner, EffectArg, EffectParamSite, LocalBinding},
        },
    },
    hir_def::{BinOp, Expr, ExprId, Partial, Stmt, UnOp},
    semantic::{
        EffectEnvSite, EffectEnvView, ProviderBinding, resolved_effect_binding_infos_for_site,
    },
};

use super::body::SmirLowerCtxt;

/// A provider that names a place is borrowed from it on each call, and one
/// that is a value from a temporary holding it.
#[derive(Clone)]
pub(super) enum WithBindingSource<'db> {
    Place(SPlace<'db>),
    Temporary(SLocalId),
}

impl<'a, 'db> SmirLowerCtxt<'a, 'db> {
    pub(super) fn lower_with_expr(
        &mut self,
        bindings: &[crate::hir_def::expr::WithBinding<'db>],
        body: ExprId,
    ) -> SValueId {
        let mut saved = Vec::with_capacity(bindings.len());
        // The block's scope holds what its providers' expressions bind and
        // allocate. A provider that is a projection's result opens its
        // session here, and a scope begun right after it holds it until
        // control leaves the block, or the later providers' expressions.
        self.push_drop_scope(true);
        let mut scopes = 1;
        for binding in bindings {
            let value_expr = binding.value;
            let value_ty = self.expr_ty(value_expr);
            let source = if let Some(place) = self.try_lower_place_expr(value_expr, true) {
                if self.names_grant(value_expr) {
                    self.push_with_scope(place.local);
                    scopes += 1;
                }
                WithBindingSource::Place(place)
            } else {
                let value = self.lower_expr(value_expr);
                let local = self.alloc_local(value_ty, Mutability::Immutable, None);
                self.push_stmt(
                    SemOrigin::Expr(value_expr),
                    SStmtKind::Assign {
                        dst: local,
                        expr: SExpr::UseValue(SOperand::expr(value, value_expr)),
                    },
                );
                WithBindingSource::Temporary(local)
            };
            saved.push((
                value_expr,
                self.with_binding_sources.insert(value_expr, source),
            ));
        }

        let body_value = self.lower_expr(body);
        for _ in 0..scopes {
            self.pop_drop_scope(Some(body_value));
        }
        for (expr, previous) in saved.into_iter().rev() {
            if let Some(previous) = previous {
                self.with_binding_sources.insert(expr, previous);
            } else {
                self.with_binding_sources.remove(&expr);
            }
        }
        body_value
    }

    /// Whether `expr` names a place in a projection's grant: the grant, or a
    /// field or element of it, through `mut` and `ref` markers and blocks'
    /// tails. A raw pointer read from a grant is a copy, which ends the
    /// session, so a place through it is not in the grant.
    fn names_grant(&self, expr: ExprId) -> bool {
        match expr.data(self.db, self.body) {
            Partial::Present(
                Expr::Call(..)
                | Expr::MethodCall(..)
                | Expr::Bin(_, _, BinOp::Index)
                | Expr::Try(_),
            ) if self.typed_body.expr_prop(self.db, expr).access().is_some() => true,
            Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::MaybeMut | UnOp::Ref)) => {
                self.names_grant(*inner)
            }
            Partial::Present(Expr::Block(stmts, _)) => matches!(
                stmts.last().map(|stmt| stmt.data(self.db, self.body)),
                Some(Partial::Present(Stmt::Expr(tail))) if self.names_grant(*tail)
            ),
            Partial::Present(Expr::Field(base, _))
                if self
                    .projectable_place_ty(self.expr_ty(*base))
                    .as_ptr(self.db)
                    .is_none() =>
            {
                self.names_grant(*base)
            }
            Partial::Present(Expr::Bin(base, _, BinOp::Index)) => self.names_grant(*base),
            _ => false,
        }
    }

    fn effect_arg_provider_space(&self, arg: &CallEffectArg<'db>) -> Option<AddressSpace> {
        arg.provider.or_else(|| {
            let binding = match arg.arg {
                EffectArg::With(value) => {
                    let PlaceBase::Binding(binding) = self.typed_body.expr_place(value)?.base;
                    binding
                }
                EffectArg::Binding(binding) => binding,
            };
            Some(self.binding_provider(binding)?.semantics.address_space)
        })
    }

    fn binding_provider(&self, binding: LocalBinding<'db>) -> Option<ProviderBinding<'db>> {
        match self.binding_role_mode {
            super::body::BindingRoleMode::Final => {
                resolved_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
            super::body::BindingRoleMode::Provisional => {
                provisional_provider_binding_for_instance_effect(self.db, self.instance, binding)
            }
        }
    }

    pub(super) fn lower_effect_arg_slice(
        &mut self,
        args: &[CallEffectArg<'db>],
    ) -> Box<[SEffectArg<'db>]> {
        args.iter().map(|arg| self.lower_effect_arg(arg)).collect()
    }

    fn lower_effect_arg(&mut self, arg: &CallEffectArg<'db>) -> SEffectArg<'db> {
        let place = match arg.arg {
            EffectArg::With(value) => match self
                .with_binding_sources
                .get(&value)
                .expect("effect provider should be captured by its with binding")
                .clone()
            {
                WithBindingSource::Place(place) => place,
                WithBindingSource::Temporary(local) => {
                    // Only owned temporaries acquire mutability from their uses. Read-only
                    // providers can remain const-backed, and captured places keep their access.
                    if arg.required_mut {
                        self.locals[local.index()].mutability = Mutability::Mutable;
                    }
                    SPlace::new(local)
                }
            },
            // The caller's own provider, passed on.
            EffectArg::Binding(binding) => SPlace::new(self.alloc_binding_local(binding)),
        };
        SEffectArg {
            binding_idx: arg.binding_idx,
            place,
            required_mut: arg.required_mut,
            provider_target_ty: arg.provider_target_ty,
            provider: self.effect_arg_provider_space(arg),
        }
    }
}

pub fn owner_effect_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<LocalBinding<'db>> {
    effect_param_site(owner)
        .into_iter()
        .flat_map(|site| {
            resolved_effect_binding_infos_for_site(db, EffectEnvSite::new(db, site))
                .iter()
                .filter_map(|binding| binding.as_ref())
                .filter(|binding| {
                    matches!(
                        binding.requirement.key.kind(),
                        EffectKeyKind::Type | EffectKeyKind::Trait
                    )
                })
                .map(LocalBinding::effect_param)
                .collect::<Vec<_>>()
        })
        .collect()
}

pub(super) fn provisional_owner_effect_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Vec<LocalBinding<'db>> {
    effect_param_site(owner)
        .into_iter()
        .flat_map(|site| {
            EffectEnvView::new(site)
                .requirements(db)
                .into_iter()
                .filter_map(move |requirement| {
                    if !matches!(
                        requirement.key.kind(),
                        EffectKeyKind::Type | EffectKeyKind::Trait
                    ) {
                        return None;
                    }
                    let provider_idx = provisional_provider_idx_for_requirement(
                        db,
                        site,
                        requirement.binding_idx,
                    )?;
                    Some(LocalBinding::effect(&requirement, provider_idx))
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

pub fn effect_param_site<'db>(owner: BodyOwner<'db>) -> Option<EffectParamSite<'db>> {
    match owner {
        BodyOwner::Func(func, mode) => Some(EffectParamSite::Func(func, mode)),
        BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => None,
        BodyOwner::Closure { def, .. } => Some(EffectParamSite::Closure(def)),
        BodyOwner::ContractInit { contract } => Some(EffectParamSite::ContractInit { contract }),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => Some(EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        }),
    }
}

pub fn same_owner_effect_binding<'db>(lhs: LocalBinding<'db>, rhs: LocalBinding<'db>) -> bool {
    match (lhs, rhs) {
        (
            LocalBinding::EffectParam {
                site: lhs_site,
                idx: lhs_idx,
                ..
            },
            LocalBinding::EffectParam {
                site: rhs_site,
                idx: rhs_idx,
                ..
            },
        ) => lhs_site == rhs_site && lhs_idx == rhs_idx,
        (
            LocalBinding::Param {
                site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
                idx: lhs_idx,
                ty: lhs_ty,
                ..
            },
            LocalBinding::Param {
                site: crate::analysis::ty::ty_check::ParamSite::EffectField(_),
                idx: rhs_idx,
                ty: rhs_ty,
                ..
            },
        ) => lhs_idx == rhs_idx && lhs_ty == rhs_ty,
        _ => lhs == rhs,
    }
}
