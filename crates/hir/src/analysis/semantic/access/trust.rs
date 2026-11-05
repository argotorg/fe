//! Use-based trust for the unsafe code of dependencies.
//!
//! An ingot's unsafe code is trusted when the ingot is the user's own code
//! (a trust root), `core` or `std`, or a dependency that a trusted ingot
//! declares with `allow_unsafe = true`. Only use counts: the semantic access
//! walk starts from the bodies of a trust root's modules, follows calls into
//! every ingot, including indexing's generated `locate` calls and the bodies
//! of the contracts a body creates, and reports an untrusted ingot's unsafe
//! code, an `unsafe` block, an `unsafe fn` or a method of an `unsafe impl`,
//! at the call in the user's code that first reaches it. An untrusted ingot's safe API stays
//! usable without trust.
//!
//! This is build policy, not typing: whether a caller builds depends on what
//! the dependency's bodies contain, but nothing it means does.
use std::collections::HashSet;

use common::{
    diagnostics::{
        CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
    },
    ingot::{Ingot, IngotKind},
};
use url::Url;

use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{SemOrigin, diagnostics::SemanticDiagnosticSpan},
        ty::ty_check::BodyOwner,
    },
    hir_def::{BlockKind, Expr, ItemKind, Partial, TopLevelMod},
};

/// The ingots whose unsafe code may run, for a walk from a trust root.
pub(super) struct UnsafeTrust {
    trusted: HashSet<Url>,
}

impl UnsafeTrust {
    /// The trust for a walk from `top_mod`, if its ingot is a trust root.
    pub(super) fn for_root(db: &dyn HirAnalysisDb, top_mod: TopLevelMod<'_>) -> Option<Self> {
        let graph = db.dependency_graph();
        graph
            .is_trust_root(db, top_mod.ingot(db).base(db))
            .then(|| Self {
                trusted: graph.trusted_ingots(db),
            })
    }

    fn allows(&self, db: &dyn HirAnalysisDb, ingot: Ingot<'_>) -> bool {
        let url = ingot.base(db);
        matches!(
            ingot.kind(db),
            IngotKind::Core | IngotKind::Std | IngotKind::StandAlone
        ) || self.trusted.contains(url)
            || !db.dependency_graph().contains_url(db, url)
    }

    /// The unsafe code `owner` runs, when its ingot is not trusted.
    pub(super) fn untrusted_site<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> Option<SemOrigin<'db>> {
        if self.allows(db, owner.scope().ingot(db)) {
            return None;
        }
        unsafe_site(db, owner)
    }
}

/// Where `owner`'s body runs unsafe code: all of it, for an `unsafe fn` or
/// a method of an `unsafe impl`, whose body is what the impl asserts correct,
/// or its first `unsafe` block.
fn unsafe_site<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Option<SemOrigin<'db>> {
    let body = owner.body(db)?;
    if let BodyOwner::Func(func, _) = owner
        && (func.is_unsafe(db)
            || matches!(
                func.scope().parent_item(db),
                Some(ItemKind::ImplTrait(impl_trait)) if impl_trait.is_unsafe(db)
            ))
    {
        return Some(SemOrigin::Body(owner));
    }
    body.exprs(db).iter().find_map(|(expr, data)| {
        matches!(data, Partial::Present(Expr::Block(_, BlockKind::Unsafe)))
            .then_some(SemOrigin::Expr(expr))
    })
}

/// A call in the user's code reaches unsafe code in a dependency that no
/// trusted ingot allows to use it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct UntrustedUnsafeUse<'db> {
    /// The trust root whose code makes the call.
    pub root: Ingot<'db>,
    /// The call in the trust root's code.
    pub call: SemanticDiagnosticSpan<'db>,
    /// The function whose body holds the unsafe code.
    pub user: BodyOwner<'db>,
    pub site: SemOrigin<'db>,
}

impl DiagnosticVoucher for UntrustedUnsafeUse<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let ingot = self.user.scope().ingot(db);
        let name = ingot_name(db, ingot.base(db));
        let site = SemanticDiagnosticSpan::Origin {
            owner: self.user,
            origin: self.site,
        };
        CompleteDiagnostic::new(
            Severity::Error,
            format!("call reaches unsafe code in `{name}`, which is not allowed to use it"),
            vec![
                SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("this call reaches unsafe code in `{name}`"),
                    self.call.resolve(db),
                ),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    format!("unsafe code in `{name}`"),
                    site.resolve(db),
                ),
            ],
            vec![self.fix(db, ingot, &name)],
            GlobalErrorCode::new(DiagnosticPass::SemanticAccess, 15),
        )
    }
}

impl UntrustedUnsafeUse<'_> {
    /// How to trust `ingot`: allow it where the root depends on it, or trust
    /// a dependent that already allows it.
    fn fix(&self, db: &dyn SpannedHirAnalysisDb, ingot: Ingot<'_>, name: &str) -> String {
        let graph = db.dependency_graph();
        let dependents = graph.dependents(db, ingot.base(db));
        let root = self.root.base(db);
        if dependents.iter().any(|(dependent, _)| dependent == root) {
            return format!(
                "to trust `{name}`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
            );
        }
        let trusted = graph.trusted_ingots(db);
        let vouchers: Vec<_> = dependents
            .iter()
            .filter(|(dependent, allows)| *allows && !trusted.contains(dependent))
            .map(|(dependent, _)| format!("`{}`", ingot_name(db, dependent)))
            .collect();
        if vouchers.is_empty() {
            format!(
                "`{name}` is trusted once an ingot you trust depends on it with `allow_unsafe = true`, \
                 or once you add it to `[dependencies]` with `allow_unsafe = true`"
            )
        } else {
            format!(
                "{} allows `{name}` to use unsafe code but is not trusted itself; \
                 trust it with `allow_unsafe = true` where you depend on it",
                vouchers.join(", ")
            )
        }
    }
}

fn ingot_name(db: &dyn HirAnalysisDb, url: &Url) -> String {
    db.workspace()
        .containing_ingot(db, url.clone())
        .and_then(|ingot| ingot.config(db).as_ref()?.metadata.name.clone())
        .map_or_else(|| url.to_string(), |name| name.to_string())
}
