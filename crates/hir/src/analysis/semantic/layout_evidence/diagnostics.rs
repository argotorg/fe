use crate::analysis::semantic::diagnostics::{
    checker_name, resolve_local_source_span, span_for_origin_from_body,
};
use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use rustc_hash::FxHashSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SemanticInstance, SemanticInstanceKey, get_or_build_semantic_instance,
            identity_semantic_instance_key,
        },
        ty::{
            CallableLayoutParamPort, LayoutBundleUnrepresentable, LayoutEvidencePathStep,
            LayoutPortKey, const_ty::CallableInputLayoutHoleOrigin, ty_check::BodyOwner,
        },
    },
    hir_def::{ItemKind, TopLevelMod},
    span::LazySpan,
};

use super::{
    LayoutEvidenceError, LayoutSignaturePort, layout_evidence_body,
    validate_layout_bundle_signature,
};

#[derive(Clone, Debug)]
pub struct LayoutEvidenceDiagnostic<'db> {
    instance: SemanticInstance<'db>,
    error: LayoutEvidenceError<'db>,
}

pub struct LayoutEvidenceAnalysisPass;

impl ModuleAnalysisPass for LayoutEvidenceAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        collect_layout_evidence_diagnostic_vouchers(db, top_mod)
    }
}

pub fn collect_layout_evidence_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    let mut seen = FxHashSet::default();
    let mut diagnostics = Vec::new();
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        for owner in BodyOwner::const_predicates_of(db, *item) {
            collect_owner(db, owner, &mut seen, &mut diagnostics);
        }
        match item {
            ItemKind::Func(func) => {
                collect_owner(db, BodyOwner::Func(*func), &mut seen, &mut diagnostics)
            }
            ItemKind::Contract(contract) => {
                collect_owner(
                    db,
                    BodyOwner::ContractInit {
                        contract: *contract,
                    },
                    &mut seen,
                    &mut diagnostics,
                );
                for (recv_idx, recv) in contract.recvs(db).data(db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(db).len() {
                        collect_owner(
                            db,
                            BodyOwner::ContractRecvArm {
                                contract: *contract,
                                recv_idx: recv_idx as u32,
                                arm_idx: arm_idx as u32,
                            },
                            &mut seen,
                            &mut diagnostics,
                        );
                    }
                }
            }
            ItemKind::Const(_)
            | ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }
    diagnostics
}

fn collect_owner<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    seen: &mut FxHashSet<SemanticInstanceKey<'db>>,
    diagnostics: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if owner.body(db).is_some() {
        let key = identity_semantic_instance_key(db, owner);
        collect_instance(
            db,
            get_or_build_semantic_instance(db, key),
            seen,
            diagnostics,
        );
        return;
    }
    // A callable without a body, such as a required trait method, is never
    // lowered, but its signature must still be representable.
    if !matches!(owner, BodyOwner::Func(_)) {
        return;
    }
    let key = identity_semantic_instance_key(db, owner);
    if seen.insert(key)
        && let Err(error) = validate_layout_bundle_signature(&key.layout_bundle_signature(db))
    {
        diagnostics.push(Box::new(LayoutEvidenceDiagnostic {
            instance: get_or_build_semantic_instance(db, key),
            error,
        }));
    }
}

fn collect_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    seen: &mut FxHashSet<SemanticInstanceKey<'db>>,
    diagnostics: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    if !seen.insert(instance.key(db)) {
        return;
    }
    if let Err(error) = layout_evidence_body(db, instance)
        && !matches!(error, LayoutEvidenceError::Blocked(_))
    {
        diagnostics.push(Box::new(LayoutEvidenceDiagnostic { instance, error }));
    }
}

impl DiagnosticVoucher for LayoutEvidenceDiagnostic<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        assert!(
            !matches!(self.error, LayoutEvidenceError::Blocked(_)),
            "blocked layout evidence must not be converted into a diagnostic"
        );
        if let LayoutEvidenceError::Normalize(diagnostic) = &self.error {
            return diagnostic.to_complete(db);
        }

        let unrepresentable = self.error.unrepresentable();
        if let Some(LayoutBundleUnrepresentable::RootArray { .. }) = unrepresentable {
            return CompleteDiagnostic::new(
                Severity::Error,
                format!(
                    "array of layout-root values in `{}`",
                    checker_name(db, self.instance)
                ),
                vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    "the elements of this array carry storage layout roots".to_string(),
                    self.primary_span(db),
                )],
                vec![
                    "every element of an array has the same type, so the elements cannot have distinct layout roots".to_string(),
                    "use separate fields, or one map whose key includes the index".to_string(),
                ],
                GlobalErrorCode::new(DiagnosticPass::SemanticLayoutEvidence, 8),
            );
        }
        let (local_code, message, internal) = match &self.error {
            _ if matches!(
                unrepresentable,
                Some(LayoutBundleUnrepresentable::NonRegularViewCycle { .. })
            ) =>
            (
                7,
                "a recursive `EffectHandle::Target` cycle changes its layout arguments and cannot be represented by a finite layout-evidence interface".to_string(),
                false,
            ),
            LayoutEvidenceError::MissingComponent { component, .. } => (
                1,
                format!(
                    "no runtime layout root is available for component {}",
                    component.0
                ),
                false,
            ),
            LayoutEvidenceError::AmbiguousComponentBinding { sources, .. } => (
                2,
                format!(
                    "multiple runtime layout roots could supply this value: {}",
                    format_sources(sources)
                ),
                false,
            ),
            LayoutEvidenceError::AmbiguousConstBinding { sources, .. } => (
                3,
                format!(
                    "this inferred slot has multiple runtime values: {}",
                    format_sources(sources)
                ),
                false,
            ),
            LayoutEvidenceError::MissingConstBinding { .. } => (
                6,
                "this inferred slot is only present inside a derived layout expression, so its original value is unavailable at runtime".to_string(),
                false,
            ),
            LayoutEvidenceError::ConflictingContextualSource { .. } => (
                5,
                "one value is required to carry two different runtime layout roots".to_string(),
                false,
            ),
            LayoutEvidenceError::Blocked(_) | LayoutEvidenceError::Normalize(_) => unreachable!(),
            error => (
                100,
                format!("layout-evidence lowering failed: {error:?}"),
                true,
            ),
        };
        let name = checker_name(db, self.instance);
        let header = if internal {
            format!("internal layout-evidence error in `{name}`")
        } else {
            format!("cannot determine inferred layout in `{name}`")
        };
        CompleteDiagnostic::new(
            Severity::Error,
            header,
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                message,
                self.primary_span(db),
            )],
            Vec::new(),
            GlobalErrorCode::new(DiagnosticPass::SemanticLayoutEvidence, local_code),
        )
    }
}

impl LayoutEvidenceDiagnostic<'_> {
    fn primary_span(&self, db: &dyn SpannedHirAnalysisDb) -> Option<common::diagnostics::Span> {
        let owner = self.instance.key(db).owner(db);
        let span = match &self.error {
            LayoutEvidenceError::AmbiguousConstBinding { origin, .. }
            | LayoutEvidenceError::MissingConstBinding { origin, .. } => {
                span_for_origin_from_body(db, owner.body(db), *origin)
            }
            LayoutEvidenceError::InvalidSignature { port, .. } => {
                signature_port_span(db, owner, *port)
            }
            LayoutEvidenceError::InvalidSchema {
                local: Some(local), ..
            }
            | LayoutEvidenceError::InvalidInterface { local, .. }
            | LayoutEvidenceError::ShapeMismatch { dst: local, .. }
            | LayoutEvidenceError::MissingComponent { local, .. }
            | LayoutEvidenceError::MissingPort { local, .. }
            | LayoutEvidenceError::ConflictingContextualSource { local, .. }
            | LayoutEvidenceError::AmbiguousComponentBinding { local, .. }
            | LayoutEvidenceError::IncompatibleComponent { dst: local, .. }
            | LayoutEvidenceError::RootTypeMismatch { dst: local, .. } => {
                resolve_local_source_span(db, self.instance, *local)
            }
            LayoutEvidenceError::Blocked(_)
            | LayoutEvidenceError::Normalize(_)
            | LayoutEvidenceError::MissingBody(_)
            | LayoutEvidenceError::TemplateLocalCountMismatch { .. }
            | LayoutEvidenceError::InvalidStatementIdentity(_)
            | LayoutEvidenceError::InvalidSchema { local: None, .. }
            | LayoutEvidenceError::DuplicateInput(_)
            | LayoutEvidenceError::InvalidPlace
            | LayoutEvidenceError::ProviderPlace
            | LayoutEvidenceError::Verify(_) => None,
        };
        span.or_else(|| {
            owner
                .body(db)
                .or_else(|| {
                    self.instance
                        .admitted_body(db)
                        .ok()
                        .and_then(|body| body.template_owner.body(db))
                })
                .and_then(|body| body.span().resolve(db))
        })
    }
}

/// The source of one signature port. Contract entry points fall back to their
/// body span.
fn signature_port_span(
    db: &dyn SpannedHirAnalysisDb,
    owner: BodyOwner<'_>,
    port: LayoutSignaturePort,
) -> Option<common::diagnostics::Span> {
    let BodyOwner::Func(func) = owner else {
        return None;
    };
    let span = func.span();
    match port {
        LayoutSignaturePort::Input(CallableInputLayoutHoleOrigin::Receiver) => {
            span.params().param(0).resolve(db)
        }
        LayoutSignaturePort::Input(CallableInputLayoutHoleOrigin::ValueParam(idx)) => {
            span.params().param(idx).ty().resolve(db)
        }
        LayoutSignaturePort::Input(CallableInputLayoutHoleOrigin::Effect(idx)) => {
            span.effects().param_idx(idx).resolve(db)
        }
        LayoutSignaturePort::Output => span.ret_ty().resolve(db),
    }
}

fn format_sources(sources: &[CallableLayoutParamPort]) -> String {
    sources
        .iter()
        .map(format_source)
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_source(source: &CallableLayoutParamPort) -> String {
    match source {
        CallableLayoutParamPort::Input(input) => {
            format!(
                "{}{}",
                format_origin(input.origin),
                format_port(&input.component)
            )
        }
        CallableLayoutParamPort::OutputWitness(port) => {
            format!("the return witness{}", format_port(port))
        }
    }
}

fn format_origin(origin: CallableInputLayoutHoleOrigin) -> String {
    match origin {
        CallableInputLayoutHoleOrigin::Receiver => "the receiver".to_string(),
        CallableInputLayoutHoleOrigin::ValueParam(index) => {
            format!("value parameter {}", index + 1)
        }
        CallableInputLayoutHoleOrigin::Effect(index) => format!("effect parameter {}", index + 1),
    }
}

fn format_port(port: &LayoutPortKey) -> String {
    let mut path = String::new();
    for step in &port.value_path {
        match step {
            LayoutEvidencePathStep::Field(index) => path.push_str(&format!(".field#{index}")),
            LayoutEvidencePathStep::Variant(index) => path.push_str(&format!("::variant#{index}")),
            LayoutEvidencePathStep::EffectTarget => path.push_str(".target"),
        }
    }
    path
}
