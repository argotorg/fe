use common::indexmap::IndexMap;
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            corelib::resolve_lib_type_path,
            normalize::normalize_ty,
            trait_def::{
                ImplementorOrigin, ResolvedImplInstance, TraitInstId, resolve_trait_impl_instance,
            },
            trait_resolution::{PredicateListId, Selection, TraitSolveCx},
            ty_check::EffectParamSite,
            ty_def::{CapabilityKind, TyBase, TyData, TyId},
            visitor::{TyVisitable, TyVisitor, walk_ty},
        },
    },
    hir_def::{Contract, Func, IdentId, scope_graph::ScopeId},
};

use super::resolve_default_root_effect_ty;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderAddressSpace {
    Memory,
    Storage,
    Transient,
    Calldata,
    Code,
}

impl ProviderAddressSpace {
    pub fn pretty(self) -> &'static str {
        match self {
            Self::Memory => "memory",
            Self::Storage => "storage",
            Self::Transient => "transient storage",
            Self::Calldata => "calldata",
            Self::Code => "code",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderKind {
    RootObject,
    Handle,
    RawAddress,
    InvalidHandle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderTransport {
    ByValue,
    ByPlace,
    ByTempPlace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct ProviderSemantics<'db> {
    pub provider_ty: TyId<'db>,
    pub kind: ProviderKind,
    pub address_space: Option<ProviderAddressSpace>,
    pub target_ty: Option<TyId<'db>>,
    pub transport: ProviderTransport,
    pub evidence: ProviderLayoutEvidence<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderLayoutFailure {
    Ambiguous,
    UnresolvedTarget,
    UnresolvedSpace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderLayoutEvidence<'db> {
    Capability,
    NotHandle,
    ResolvedHandle(ResolvedImplInstance<'db>),
    InvalidHandle(ProviderLayoutFailure),
    ContractField,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProviderLayoutResolution<'db> {
    NotHandle,
    Resolved {
        impl_instance: ResolvedImplInstance<'db>,
        target_template: TyId<'db>,
        space: ProviderAddressSpace,
    },
    Ambiguous,
    UnresolvedTarget,
    UnresolvedSpace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum StaticSlotLayoutResolution {
    NotStaticSlot,
    Resolved(ProviderAddressSpace),
    Ambiguous,
    UnresolvedSpace,
}

fn can_select_nominal_effect_handle_impl<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    matches!(
        ty.base_ty(db).data(db),
        TyData::TyParam(_)
            | TyData::AssocTy(_)
            | TyData::QualifiedTy(_)
            | TyData::TyBase(TyBase::Adt(_))
    )
}

fn contains_unresolved_type_projection<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    struct Finder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for Finder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }
            if matches!(
                ty.data(self.db),
                TyData::AssocTy(_) | TyData::QualifiedTy(_)
            ) {
                self.found = true;
            } else {
                walk_ty(self, ty);
            }
        }
    }

    let mut finder = Finder { db, found: false };
    ty.visit_with(&mut finder);
    finder.found
}

pub(crate) enum EffectHandleTargetResolution<'db> {
    NotHandle,
    Resolved {
        impl_instance: ResolvedImplInstance<'db>,
        target_template: TyId<'db>,
        target_ty: TyId<'db>,
    },
    Ambiguous,
    UnresolvedTarget,
}

pub(crate) fn resolve_effect_handle_target<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
) -> EffectHandleTargetResolution<'db> {
    if provider_ty.has_var(db) {
        return EffectHandleTargetResolution::Ambiguous;
    }
    if provider_ty.as_capability(db).is_some()
        || !can_select_nominal_effect_handle_impl(db, provider_ty)
    {
        return EffectHandleTargetResolution::NotHandle;
    }
    let Some(effect_handle) = super::corelib::resolve_core_trait(db, scope, &["EffectHandle"])
    else {
        return EffectHandleTargetResolution::NotHandle;
    };
    let inst = TraitInstId::new(db, effect_handle, vec![provider_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved)
            if !matches!(
                resolved.selected().origin(db),
                ImplementorOrigin::Assumption
            ) =>
        {
            resolved
        }
        Selection::Unique(_) | Selection::Ambiguous(_) => {
            return EffectHandleTargetResolution::Ambiguous;
        }
        Selection::NotFound => return EffectHandleTargetResolution::NotHandle,
    };
    let target_ident = IdentId::new(db, "Target".to_string());
    let Some(target_template) = resolved.assoc_ty_template(db, target_ident) else {
        return EffectHandleTargetResolution::UnresolvedTarget;
    };
    let Some(target_ty) = resolved.instantiated_assoc_ty(db, target_ident) else {
        return EffectHandleTargetResolution::UnresolvedTarget;
    };
    let target_ty = normalize_ty(db, target_ty, scope, assumptions);
    if target_ty.has_invalid(db)
        || target_ty.has_var(db)
        || !target_ty.has_star_kind(db)
        || contains_unresolved_type_projection(db, target_ty)
    {
        return EffectHandleTargetResolution::UnresolvedTarget;
    }
    EffectHandleTargetResolution::Resolved {
        impl_instance: resolved,
        target_template,
        target_ty,
    }
}

/// Returns the semantic layout value carried by a nominal effect handle.
/// Unlike provider allocation validation, this permits generic parameters in
/// the target so declaration schemas can retain their const bindings.
#[salsa::tracked]
pub fn effect_handle_layout_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
) -> Option<TyId<'db>> {
    if let Some((_, inner)) = provider_ty.as_capability(db) {
        return Some(inner);
    }
    match resolve_effect_handle_target(db, scope, assumptions, provider_ty) {
        EffectHandleTargetResolution::Resolved { target_ty, .. } => Some(target_ty),
        EffectHandleTargetResolution::NotHandle
        | EffectHandleTargetResolution::Ambiguous
        | EffectHandleTargetResolution::UnresolvedTarget => None,
    }
}

#[salsa::tracked]
pub fn resolve_effect_handle_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
) -> ProviderLayoutResolution<'db> {
    let (impl_instance, target_template, target_ty) =
        match resolve_effect_handle_target(db, scope, assumptions, provider_ty) {
            EffectHandleTargetResolution::NotHandle => {
                return ProviderLayoutResolution::NotHandle;
            }
            EffectHandleTargetResolution::Resolved {
                impl_instance,
                target_template,
                target_ty,
            } => (impl_instance, target_template, target_ty),
            EffectHandleTargetResolution::Ambiguous => {
                return ProviderLayoutResolution::Ambiguous;
            }
            EffectHandleTargetResolution::UnresolvedTarget => {
                return ProviderLayoutResolution::UnresolvedTarget;
            }
        };
    if target_ty.has_param(db) {
        return ProviderLayoutResolution::UnresolvedTarget;
    }
    let resolved = impl_instance;
    let Some(space) = effect_space_from_resolved_trait_const(db, scope, resolved) else {
        return ProviderLayoutResolution::UnresolvedSpace;
    };
    ProviderLayoutResolution::Resolved {
        impl_instance,
        target_template,
        space,
    }
}

#[salsa::tracked]
pub fn resolve_static_slot_layout<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    owner_ty: TyId<'db>,
) -> StaticSlotLayoutResolution {
    let Some(static_slot) = super::corelib::resolve_core_trait(db, scope, &["StaticSlot"]) else {
        return StaticSlotLayoutResolution::NotStaticSlot;
    };
    let inst = TraitInstId::new(db, static_slot, vec![owner_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
    let resolved = match resolve_trait_impl_instance(db, solve_cx, inst) {
        Selection::Unique(resolved)
            if !matches!(
                resolved.selected().origin(db),
                ImplementorOrigin::Assumption
            ) =>
        {
            resolved
        }
        Selection::Unique(_) | Selection::Ambiguous(_) => {
            return StaticSlotLayoutResolution::Ambiguous;
        }
        Selection::NotFound => return StaticSlotLayoutResolution::NotStaticSlot,
    };
    effect_space_from_resolved_trait_const(db, scope, resolved).map_or(
        StaticSlotLayoutResolution::UnresolvedSpace,
        StaticSlotLayoutResolution::Resolved,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum RootProviderSiteKind {
    Func,
    Contract,
    ContractInit,
    ContractRecvArm,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct RootProviderRegistration<'db> {
    pub idx: u32,
    pub site_kind: RootProviderSiteKind,
    pub provider_ty: TyId<'db>,
}

pub fn registered_root_providers<'db>(
    db: &'db dyn HirAnalysisDb,
    site: EffectParamSite<'db>,
) -> &'db [RootProviderRegistration<'db>] {
    match site {
        EffectParamSite::Func(func) => registered_root_providers_for_func(db, func).as_slice(),
        EffectParamSite::Contract(contract) => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::Contract)
                .as_slice()
        }
        EffectParamSite::ContractInit { contract } => {
            registered_root_providers_for_contract(db, contract, RootProviderSiteKind::ContractInit)
                .as_slice()
        }
        EffectParamSite::ContractRecvArm { contract, .. } => {
            registered_root_providers_for_contract(
                db,
                contract,
                RootProviderSiteKind::ContractRecvArm,
            )
            .as_slice()
        }
    }
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_func<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, RootProviderSiteKind::Func, func.scope())
}

#[salsa::tracked(return_ref)]
fn registered_root_providers_for_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
    site_kind: RootProviderSiteKind,
) -> Vec<RootProviderRegistration<'db>> {
    registered_root_providers_for_scope(db, site_kind, contract.scope())
}

fn registered_root_providers_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    site_kind: RootProviderSiteKind,
    scope: ScopeId<'db>,
) -> Vec<RootProviderRegistration<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    let Some(provider_ty) = resolve_default_root_effect_ty(db, scope, assumptions) else {
        return Vec::new();
    };
    vec![RootProviderRegistration {
        idx: 0,
        site_kind,
        provider_ty,
    }]
}

pub fn provider_semantics<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
) -> ProviderSemantics<'db> {
    if let Some((kind, inner)) = provider_ty.as_capability(db) {
        let address_space = match kind {
            CapabilityKind::View | CapabilityKind::Ref | CapabilityKind::Mut => {
                Some(ProviderAddressSpace::Memory)
            }
        };
        return ProviderSemantics {
            provider_ty,
            kind: provider_kind_for_target_ty(db, inner),
            address_space,
            target_ty: Some(inner),
            transport: ProviderTransport::ByValue,
            evidence: ProviderLayoutEvidence::Capability,
        };
    }

    match resolve_effect_handle_layout(db, scope, assumptions, provider_ty) {
        ProviderLayoutResolution::NotHandle => ProviderSemantics {
            provider_ty,
            kind: ProviderKind::RootObject,
            address_space: Some(ProviderAddressSpace::Memory),
            target_ty: None,
            transport: ProviderTransport::ByValue,
            evidence: ProviderLayoutEvidence::NotHandle,
        },
        ProviderLayoutResolution::Resolved {
            impl_instance,
            space,
            ..
        } => {
            let target_ident = IdentId::new(db, "Target".to_string());
            let target_ty = impl_instance
                .instantiated_assoc_ty(db, target_ident)
                .map(|target| normalize_ty(db, target, scope, assumptions));
            let kind = target_ty.map_or(ProviderKind::InvalidHandle, |target| {
                provider_kind_for_target_ty(db, target)
            });
            ProviderSemantics {
                provider_ty,
                kind,
                address_space: Some(space),
                target_ty,
                transport: ProviderTransport::ByValue,
                evidence: ProviderLayoutEvidence::ResolvedHandle(impl_instance),
            }
        }
        ProviderLayoutResolution::Ambiguous => {
            invalid_provider_semantics(provider_ty, ProviderLayoutFailure::Ambiguous)
        }
        ProviderLayoutResolution::UnresolvedTarget => {
            invalid_provider_semantics(provider_ty, ProviderLayoutFailure::UnresolvedTarget)
        }
        ProviderLayoutResolution::UnresolvedSpace => {
            invalid_provider_semantics(provider_ty, ProviderLayoutFailure::UnresolvedSpace)
        }
    }
}

fn invalid_provider_semantics(
    provider_ty: TyId<'_>,
    failure: ProviderLayoutFailure,
) -> ProviderSemantics<'_> {
    ProviderSemantics {
        provider_ty,
        kind: ProviderKind::InvalidHandle,
        address_space: None,
        target_ty: None,
        transport: ProviderTransport::ByValue,
        evidence: ProviderLayoutEvidence::InvalidHandle(failure),
    }
}

pub fn provider_semantics_for_specialized_call<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    provider_ty: TyId<'db>,
    target_ty: Option<TyId<'db>>,
    address_space: Option<ProviderAddressSpace>,
    transport: ProviderTransport,
) -> ProviderSemantics<'db> {
    let mut semantics = provider_semantics(db, scope, assumptions, provider_ty);
    let root_object_refinement = matches!(semantics.evidence, ProviderLayoutEvidence::NotHandle)
        && target_ty.is_some_and(|target| {
            super::layout_shape_key(db, target) == super::layout_shape_key(db, provider_ty)
        });
    if matches!(
        semantics.evidence,
        ProviderLayoutEvidence::Capability | ProviderLayoutEvidence::ContractField
    ) || root_object_refinement
    {
        if let Some(target_ty) = target_ty {
            semantics.kind = provider_kind_for_target_ty(db, target_ty);
            semantics.target_ty = Some(target_ty);
        }
        if let Some(address_space) = address_space {
            semantics.address_space = Some(address_space);
        }
    }
    semantics.transport = transport;
    semantics
}

fn effect_space_from_resolved_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    resolved: ResolvedImplInstance<'db>,
) -> Option<ProviderAddressSpace> {
    let space_ident = IdentId::new(db, "SPACE".to_string());
    let const_ty = super::const_ty::const_ty_from_resolved_trait_const(db, resolved, space_ident)?;
    effect_space_from_const_ty(db, scope, const_ty)
}

fn effect_space_from_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    const_ty: super::const_ty::ConstTyId<'db>,
) -> Option<ProviderAddressSpace> {
    use super::const_ty::{ConstTyData, EvaluatedConstTy};

    let evaluated = const_ty.evaluate(db, None);
    let ConstTyData::Evaluated(EvaluatedConstTy::EnumVariant(variant), _) = evaluated.data(db)
    else {
        return None;
    };

    let space_enum = resolve_lib_type_path(db, scope, "core::effect_ref::AddressSpace")?;
    let space_adt = space_enum.adt_def(db)?;
    if super::adt_def::AdtRef::Enum(variant.enum_) != space_adt.adt_ref(db) {
        return None;
    }

    match variant.ident(db)?.data(db).as_str() {
        "Memory" => Some(ProviderAddressSpace::Memory),
        "Calldata" => Some(ProviderAddressSpace::Calldata),
        "Storage" => Some(ProviderAddressSpace::Storage),
        "TransientStorage" => Some(ProviderAddressSpace::Transient),
        "Code" => Some(ProviderAddressSpace::Code),
        _ => None,
    }
}

fn provider_kind_for_target_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    target_ty: TyId<'db>,
) -> ProviderKind {
    let target_ty = if let Some((_, inner)) = target_ty.as_borrow(db) {
        inner
    } else {
        target_ty
    };
    if target_ty.is_struct(db)
        || target_ty.is_array(db)
        || target_ty.is_tuple(db)
        || target_ty.as_enum(db).is_some()
    {
        ProviderKind::Handle
    } else {
        ProviderKind::RawAddress
    }
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::{
        ProviderAddressSpace, ProviderKind, ProviderLayoutEvidence, ProviderLayoutFailure,
        ProviderLayoutResolution, provider_semantics, resolve_effect_handle_layout,
    };
    use crate::{
        analysis::ty::{trait_def::ImplementorOrigin, trait_resolution::PredicateListId},
        hir_def::{ItemKind, TopLevelMod, scope_graph::ScopeId},
        test_db::HirAnalysisTestDb,
    };

    fn provider_param_ty<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: TopLevelMod<'db>,
    ) -> (ScopeId<'db>, crate::analysis::ty::TyId<'db>) {
        let func = top_mod
            .children_non_nested(db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func),
                _ => None,
            })
            .expect("missing probe function");
        let ty = func
            .params(db)
            .next()
            .expect("missing provider parameter")
            .ty(db);
        (func.scope(), ty)
    }

    #[test]
    fn provider_layout_uses_one_generic_impl_instance_for_target_and_space() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "provider_layout_uses_one_generic_impl_instance_for_target_and_space.fe",
            ),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Ptr<T, const SP: AddressSpace> { raw: u256 }

impl<T, const SP: AddressSpace> EffectHandle for Ptr<T, SP> {
    type Target = T
    const SPACE: AddressSpace = SP

    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own Ptr<u8, AddressSpace::TransientStorage>) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let (scope, provider_ty) = provider_param_ty(&db, top_mod);
        let resolution =
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty);
        let ProviderLayoutResolution::Resolved {
            impl_instance,
            target_template,
            space,
        } = resolution
        else {
            panic!("expected a unique provider layout, got {resolution:?}");
        };

        let target = impl_instance
            .instantiated_assoc_ty(&db, crate::hir_def::IdentId::new(&db, "Target"))
            .expect("missing instantiated Target");
        assert!(target_template.has_param(&db));
        assert_eq!(target.pretty_print(&db).to_string(), "u8");
        assert_eq!(impl_instance.impl_args(&db)[0], target);
        assert_eq!(space, ProviderAddressSpace::Transient);
        assert!(matches!(
            impl_instance.selected().origin(&db),
            ImplementorOrigin::Hir(_)
        ));
    }

    #[test]
    fn provider_layout_rejects_multiple_matching_impls_even_when_outputs_match() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from(
                "provider_layout_rejects_multiple_matching_impls_even_when_outputs_match.fe",
            ),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Ptr { raw: u256 }

impl EffectHandle for Ptr {
    type Target = u8
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl EffectHandle for Ptr {
    type Target = u8
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own Ptr) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let (scope, provider_ty) = provider_param_ty(&db, top_mod);
        let resolution =
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty);
        assert!(
            matches!(resolution, ProviderLayoutResolution::Ambiguous),
            "got {resolution:?}"
        );
        let semantics =
            provider_semantics(&db, scope, PredicateListId::empty_list(&db), provider_ty);
        assert_eq!(semantics.kind, ProviderKind::InvalidHandle);
        assert!(matches!(
            semantics.evidence,
            ProviderLayoutEvidence::InvalidHandle(ProviderLayoutFailure::Ambiguous)
        ));
        assert!(semantics.target_ty.is_none());
        assert!(semantics.address_space.is_none());
    }

    #[test]
    fn provider_layout_distinguishes_missing_target_and_unresolved_space() {
        let mut db = HirAnalysisTestDb::default();
        let missing_target_file = db.new_stand_alone(
            Utf8PathBuf::from("provider_layout_distinguishes_missing_target.fe"),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct MissingTarget { raw: u256 }

impl EffectHandle for MissingTarget {
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { MissingTarget { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own MissingTarget) {}
"#,
        );
        let (missing_target_mod, _) = db.top_mod(missing_target_file);
        let (scope, provider_ty) = provider_param_ty(&db, missing_target_mod);
        let resolution =
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty);
        assert!(
            matches!(resolution, ProviderLayoutResolution::UnresolvedTarget),
            "got {resolution:?}"
        );

        let unresolved_space_file = db.new_stand_alone(
            Utf8PathBuf::from("provider_layout_distinguishes_unresolved_space.fe"),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Ptr<T, const SP: AddressSpace = _> { raw: u256 }

impl<T, const SP: AddressSpace> EffectHandle for Ptr<T, SP> {
    type Target = T
    const SPACE: AddressSpace = SP
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own Ptr<u8>) {}
"#,
        );
        let (unresolved_space_mod, _) = db.top_mod(unresolved_space_file);
        let (scope, provider_ty) = provider_param_ty(&db, unresolved_space_mod);
        let resolution =
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty);
        assert!(
            matches!(resolution, ProviderLayoutResolution::UnresolvedSpace),
            "got {resolution:?}"
        );
    }

    #[test]
    fn provider_layout_rejects_a_residual_generic_target() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("provider_layout_rejects_a_residual_generic_target.fe"),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Ptr<T> { raw: u256 }

impl<T> EffectHandle for Ptr<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        let impl_trait = top_mod.all_impl_traits(&db)[0];
        assert!(matches!(
            resolve_effect_handle_layout(
                &db,
                impl_trait.scope(),
                PredicateListId::empty_list(&db),
                impl_trait.ty(&db),
            ),
            ProviderLayoutResolution::UnresolvedTarget
        ));
    }

    #[test]
    fn provider_layout_rejects_an_unreduced_projection_target() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("provider_layout_rejects_an_unreduced_projection_target.fe"),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

trait Source { type Value }

struct Missing {}
struct Ptr { raw: u256 }

impl EffectHandle for Ptr {
    type Target = <Missing as Source>::Value
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own Ptr) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let (scope, provider_ty) = provider_param_ty(&db, top_mod);
        assert!(matches!(
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty,),
            ProviderLayoutResolution::UnresolvedTarget
        ));
    }

    #[test]
    fn provider_layout_rejects_an_incomplete_adt_target() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("provider_layout_rejects_an_incomplete_adt_target.fe"),
            r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Generic<T> {}
struct Ptr { raw: u256 }

impl EffectHandle for Ptr {
    type Target = Generic
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Ptr { raw } }
    fn raw(self) -> u256 { self.raw }
}

fn probe(value: own Ptr) {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let (scope, provider_ty) = provider_param_ty(&db, top_mod);
        assert!(matches!(
            resolve_effect_handle_layout(&db, scope, PredicateListId::empty_list(&db), provider_ty,),
            ProviderLayoutResolution::UnresolvedTarget
        ));
    }
}
