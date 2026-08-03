use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::instantiate_with_generic_args,
        ty::{
            ProviderAddressSpace,
            trait_resolution::PredicateListId,
            ty_check::{
                ClosureCapture, ClosureCaptureConstruction, ClosureInfo, LocalBinding, ParamSite,
            },
            ty_def::{
                ClosureCaptureAccess, ClosureParamMode, ClosureTy, TyId,
                closure_field_count_is_supported,
            },
            ty_is_copy,
        },
    },
    hir_def::{EffectParamListId, ExprId, attr::ArithmeticMode},
};
use rustc_hash::FxHashSet;

use super::{BodyOwner, ClosureReceiverMode, ReturnProvenance, ReturnSource, TypedBody};

/// The complete typed view of a callable body.
///
/// A closure reuses its enclosing HIR [`Body`] and [`TypedBody`], but has its
/// own root expression, parameters, result type, and return-borrow provider.
/// Keeping the owner and typed body paired prevents consumers from
/// accidentally reading enclosing-callable metadata for a closure.
#[derive(Clone, Copy, Debug)]
pub struct TypedCallableBody<'db> {
    owner: BodyOwner<'db>,
    typed_body: &'db TypedBody<'db>,
}

/// A validated join between a closure's body-local binding metadata and its
/// specialized type-level signature.
///
/// Capture and parameter order is ABI-significant. Consumers should use this
/// view instead of independently indexing [`ClosureInfo`] and [`ClosureTy`].
/// Coherence is established by first validating the body-local binding
/// identities against their template descriptor, then applying the
/// specialized closure's parent arguments before comparing field types.
#[derive(Clone, Copy, Debug)]
pub struct TypedClosureBody<'db> {
    ty: ClosureTy<'db>,
    info: &'db ClosureInfo<'db>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypedClosureCapture<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
    pub access_without_return: ClosureCaptureAccess,
    pub access: ClosureCaptureAccess,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypedClosureParam<'db> {
    pub binding: LocalBinding<'db>,
    pub ty: TyId<'db>,
    pub mode: ClosureParamMode,
}

impl<'db> TypedClosureBody<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        ty: ClosureTy<'db>,
        info: &'db ClosureInfo<'db>,
    ) -> Option<Self> {
        let def = ty.def(db);
        if info.def != def
            || info.ty.def(db) != def
            || info.ty.parent_args(db).len() != ty.parent_args(db).len()
            || info.params.len() != info.ty.params(db).len()
            || info.params.len() != ty.params(db).len()
            || info.captures.len() != info.ty.captures(db).len()
            || info.captures.len() != ty.captures(db).len()
            || info.capture_expr_accesses.len() != info.captures.len()
            || !closure_field_count_is_supported(info.params.len())
            || !closure_field_count_is_supported(info.captures.len())
        {
            return None;
        }

        let mut seen_bindings = FxHashSet::default();
        if !info
            .params
            .iter()
            .copied()
            .all(|binding| seen_bindings.insert(binding))
        {
            return None;
        }
        // Capture slots are unique and disjoint from parameter slots.
        // Downstream lowering keys both collections by `LocalBinding`.
        if !info
            .captures
            .iter()
            .all(|capture| seen_bindings.insert(capture.binding))
        {
            return None;
        }

        let params_match = info.params.iter().enumerate().all(|(idx, binding)| {
            let LocalBinding::Param {
                site: ParamSite::Closure(site),
                idx: binding_idx,
                mode,
                ty: binding_ty,
                ..
            } = binding
            else {
                return false;
            };
            let binding_mode = match mode {
                crate::hir_def::params::FuncParamMode::Own => ClosureParamMode::Own,
                crate::hir_def::params::FuncParamMode::View => {
                    let Some(mode) = ClosureParamMode::try_from_carrier(db, *binding_ty) else {
                        return false;
                    };
                    mode
                }
            };
            *site == def
                && *binding_idx == idx
                && *binding_ty == info.ty.params(db)[idx]
                && binding_mode == info.ty.param_modes(db)[idx]
        });
        if !params_match {
            return None;
        }

        let captures_match = info.captures.iter().enumerate().all(|(idx, capture)| {
            capture.ty == info.ty.captures(db)[idx]
                && capture.access == info.ty.capture_accesses(db)[idx]
        });
        if !captures_match {
            return None;
        }

        let parent_args = ty.parent_args(db);
        let specializes = |template: TyId<'db>, specialized: TyId<'db>| {
            instantiate_with_generic_args(db, template, parent_args) == specialized
        };
        if !info
            .ty
            .parent_args(db)
            .iter()
            .copied()
            .zip(parent_args.iter().copied())
            .all(|(template, specialized)| specializes(template, specialized))
            || !info
                .ty
                .params(db)
                .iter()
                .copied()
                .zip(ty.params(db).iter().copied())
                .all(|(template, specialized)| specializes(template, specialized))
            || info.ty.param_modes(db) != ty.param_modes(db)
            || !info
                .ty
                .captures(db)
                .iter()
                .copied()
                .zip(ty.captures(db).iter().copied())
                .all(|(template, specialized)| specializes(template, specialized))
            || info.ty.capture_accesses(db) != ty.capture_accesses(db)
            || !specializes(info.ty.ret_ty(db), ty.ret_ty(db))
        {
            return None;
        }

        Some(Self { ty, info })
    }

    pub fn ty(self) -> ClosureTy<'db> {
        self.ty
    }

    pub fn physical_param_bindings(
        self,
        db: &'db dyn HirAnalysisDb,
        receiver_mode: ClosureReceiverMode,
    ) -> [LocalBinding<'db>; 2] {
        [
            LocalBinding::closure_env(db, self.ty, receiver_mode),
            LocalBinding::closure_args(db, self.ty),
        ]
    }

    pub fn param_bindings(self) -> &'db [LocalBinding<'db>] {
        &self.info.params
    }

    pub fn params(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl ExactSizeIterator<Item = TypedClosureParam<'db>> + 'db {
        self.info
            .params
            .iter()
            .copied()
            .zip(self.ty.params(db).iter().copied())
            .zip(self.ty.param_modes(db).iter().copied())
            .map(|((binding, ty), mode)| TypedClosureParam { binding, ty, mode })
    }

    pub fn captures(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl ExactSizeIterator<Item = TypedClosureCapture<'db>> + 'db {
        self.info
            .captures
            .iter()
            .zip(self.ty.captures_with_accesses(db))
            .map(|(capture, (ty, access))| TypedClosureCapture {
                binding: capture.binding,
                ty,
                access_without_return: capture.access_without_return,
                access,
            })
    }

    fn capture_plan(
        self,
        db: &'db dyn HirAnalysisDb,
        assumptions: PredicateListId<'db>,
    ) -> Vec<ClosureCapture<'db>> {
        let scope = self.ty.def(db).body.scope();
        self.captures(db)
            .map(|capture| ClosureCapture {
                binding: capture.binding,
                ty: capture.ty,
                construction: if ty_is_copy(db, scope, capture.ty, assumptions) {
                    ClosureCaptureConstruction::Copy
                } else {
                    ClosureCaptureConstruction::Move
                },
                access_without_return: capture.access_without_return,
                access: capture.access,
            })
            .collect()
    }

    fn return_borrow_provider(self) -> Option<ProviderAddressSpace> {
        self.info.return_borrow_provider
    }
}

impl<'db> TypedCallableBody<'db> {
    pub fn new(owner: BodyOwner<'db>, typed_body: &'db TypedBody<'db>) -> Self {
        Self { owner, typed_body }
    }

    pub fn root_expr(self, db: &'db dyn HirAnalysisDb) -> Option<ExprId> {
        self.owner.root_expr(db)
    }

    pub fn param_bindings(self, db: &'db dyn HirAnalysisDb) -> Vec<LocalBinding<'db>> {
        if matches!(self.owner, BodyOwner::Closure { .. }) {
            let Some((closure, receiver_mode)) = self.owner_closure_body(db) else {
                return Vec::new();
            };
            return closure.physical_param_bindings(db, receiver_mode).to_vec();
        }

        self.typed_body.param_bindings.clone()
    }

    pub fn param_binding(
        self,
        db: &'db dyn HirAnalysisDb,
        idx: usize,
    ) -> Option<LocalBinding<'db>> {
        if matches!(self.owner, BodyOwner::Closure { .. }) {
            let (closure, receiver_mode) = self.owner_closure_body(db)?;
            return closure
                .physical_param_bindings(db, receiver_mode)
                .get(idx)
                .copied();
        }

        self.typed_body.param_binding(idx)
    }

    pub fn result_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self.owner {
            BodyOwner::Closure { ty, .. } => ty.ret_ty(db),
            _ => self.typed_body.result_ty(),
        }
    }

    pub fn return_borrow_provider(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<ProviderAddressSpace> {
        match self.owner {
            BodyOwner::Closure { .. } => self
                .owner_closure_body(db)
                .and_then(|(closure, _)| closure.return_borrow_provider()),
            _ => self.typed_body.return_borrow_provider(),
        }
    }

    pub fn return_provenance(self, db: &'db dyn HirAnalysisDb) -> ReturnProvenance {
        self.typed_body.callable_return_provenance(db, self.owner)
    }

    /// Conservative input sources that may contribute to the returned value.
    ///
    /// Unlike [`Self::return_provenance`], this retains known sources from
    /// mixed paths even when the exact carrier provenance is `Fresh`.
    pub fn forwarded_return_sources(self, db: &'db dyn HirAnalysisDb) -> Vec<ReturnSource> {
        self.typed_body
            .callable_forwarded_return_sources(db, self.owner)
    }

    /// Conservative return sources plus whether every return path was
    /// represented by those sources.
    pub fn forwarded_return_sources_with_completeness(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> (Vec<ReturnSource>, bool) {
        self.typed_body
            .callable_forwarded_return_sources_with_completeness(db, self.owner)
    }

    pub fn closure_capture_plan(
        self,
        db: &'db dyn HirAnalysisDb,
        closure: ClosureTy<'db>,
    ) -> Option<Vec<ClosureCapture<'db>>> {
        Some(
            self.closure_body(db, closure)?
                .capture_plan(db, self.typed_body.assumptions()),
        )
    }

    pub fn arithmetic_mode(self, db: &'db dyn HirAnalysisDb) -> ArithmeticMode {
        self.owner.arithmetic_mode(db)
    }

    pub fn effects(self, db: &'db dyn HirAnalysisDb) -> EffectParamListId<'db> {
        self.owner.effects(db)
    }

    /// Returns the descriptor for `closure` only when its body metadata and
    /// specialized type agree on every ABI-significant ordering invariant.
    pub fn closure_body(
        self,
        db: &'db dyn HirAnalysisDb,
        closure: ClosureTy<'db>,
    ) -> Option<TypedClosureBody<'db>> {
        let def = closure.def(db);
        let info = self
            .typed_body
            .closure_info(def.expr)
            .filter(|info| info.def == def)?;
        TypedClosureBody::new(db, closure, info)
    }

    pub fn owner_closure_body(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<(TypedClosureBody<'db>, ClosureReceiverMode)> {
        let BodyOwner::Closure {
            ty,
            def,
            receiver_mode,
        } = self.owner
        else {
            return None;
        };
        if ty.def(db) != def {
            return None;
        }
        self.closure_body(db, ty)
            .map(|closure| (closure, receiver_mode))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        analysis::ty::{
            ty_check::{BodyOwner, check_func_body},
            ty_def::{ClosureCaptures, ClosureSignature, ClosureTy, MAX_CLOSURE_FIELDS},
        },
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };
    use common::indexmap::IndexMap;

    use super::{TypedCallableBody, TypedClosureBody};

    #[test]
    fn typed_closure_descriptor_rejects_duplicate_overlapping_and_oversized_captures() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "malformed_closure_capture_descriptor.fe".into(),
            r#"
fn probe(captured: u8) {
    let closure = |argument: u8| {
        let _observed = argument
        captured
    }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let probe = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "probe") =>
                {
                    Some(*func)
                }
                _ => None,
            })
            .expect("missing probe");
        let (_, typed_body) = check_func_body(&db, probe);
        let (_, template) = typed_body
            .closure_infos()
            .next()
            .expect("missing closure metadata");
        let capture = template.captures[0];

        let mut duplicated = template.clone();
        duplicated.captures = vec![capture; 2];
        duplicated.capture_expr_accesses = vec![IndexMap::new(); 2];
        let duplicated_ty = ClosureTy::new(
            &db,
            template.def,
            template.ty.parent_args(&db).clone(),
            ClosureCaptures::new(vec![capture.ty; 2], vec![capture.access; 2]),
            ClosureSignature::new(
                template.ty.params(&db).to_vec(),
                template.ty.param_modes(&db).to_vec(),
                template.ty.ret_ty(&db),
            ),
        );
        duplicated.ty = duplicated_ty;
        assert!(
            TypedClosureBody::new(&db, duplicated_ty, &duplicated).is_none(),
            "a capture binding must occupy exactly one environment slot",
        );

        let mut overlapping = template.clone();
        overlapping.captures[0].binding = template.params[0];
        assert!(
            TypedClosureBody::new(&db, template.ty, &overlapping).is_none(),
            "a closure parameter binding must not also occupy a capture slot",
        );

        let mut oversized = template.clone();
        oversized.captures = vec![capture; MAX_CLOSURE_FIELDS + 1];
        oversized.capture_expr_accesses = vec![IndexMap::new(); MAX_CLOSURE_FIELDS + 1];
        let ty = ClosureTy::new(
            &db,
            template.def,
            template.ty.parent_args(&db).clone(),
            ClosureCaptures::new(
                vec![capture.ty; MAX_CLOSURE_FIELDS + 1],
                vec![capture.access; MAX_CLOSURE_FIELDS + 1],
            ),
            ClosureSignature::new(
                template.ty.params(&db).to_vec(),
                template.ty.param_modes(&db).to_vec(),
                template.ty.ret_ty(&db),
            ),
        );
        oversized.ty = ty;

        assert!(
            TypedClosureBody::new(&db, ty, &oversized).is_none(),
            "an unrepresentable capture environment must not cross the validated descriptor boundary",
        );
        assert!(
            TypedCallableBody::new(BodyOwner::Func(probe), typed_body)
                .closure_body(&db, template.ty)
                .is_some(),
            "the source-level descriptor must remain valid",
        );
    }
}
