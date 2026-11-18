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
//! Unsafe code is accepted only if every ingot that contributed to it is
//! trusted. Code belongs to the ingot that wrote its syntax. Generated code is
//! written by the package of the quotation that holds its text, although
//! preparation publishes it into the package that asked for it, so the package
//! that asked is not a contributor. A fragment that a quotation splices keeps
//! its writer, and each quotation around it placed the fragment into the code
//! that runs, so the writer and every placer must be trusted for an `unsafe`
//! block in it. An `unsafe impl` asserts all of its members, whoever wrote
//! them, so the ingot that wrote the `unsafe impl` is the one that must be
//! trusted for them.
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
    hir_def::{BlockKind, Expr, HirIngot, ImplTrait, ItemKind, Partial, TopLevelMod},
    span::LazySpan,
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

    /// Whether every ingot that contributed to `owner` is trusted: the one
    /// that wrote its body, and the ones that wrote or placed its unsafe code
    /// (see `untrusted_site`). A body with an untrusted contributor is not
    /// the trusted code's own: a call into it is judged, at the call.
    pub(super) fn trusts_authors_of<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> bool {
        self.allows(db, author_ingot(db, owner)) && self.untrusted_site(db, owner).is_none()
    }

    /// The first ingot of `chain` that is not trusted. A chain lists the
    /// modules of the quotations that wrote and placed code: the writer
    /// first, the outermost placer last.
    fn first_untrusted<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        chain: &[TopLevelMod<'db>],
    ) -> Option<Ingot<'db>> {
        chain
            .iter()
            .map(|top_mod| top_mod.ingot(db))
            .find(|ingot| !self.allows(db, *ingot))
    }

    /// The `unsafe impl`s of the program that no call can reach, because they
    /// have no function, and that an untrusted ingot wrote or placed. The
    /// program is the ingot `root` and the ingots it depends on, and an
    /// implementation counts wherever it is, with or without a use: it asserts
    /// a contract that trusted code may rely on without any call into it.
    pub(super) fn untrusted_unsafe_impls<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        root: Ingot<'db>,
    ) -> Vec<UntrustedUnsafeImpl<'db>> {
        let graph = db.dependency_graph();
        let root_url = root.base(db).clone();
        let urls = std::iter::once(root_url.clone()).chain(graph.dependency_urls(db, &root_url));
        let mut found = Vec::new();
        for url in urls {
            let Some(ingot) = db.workspace().containing_ingot(db, url) else {
                continue;
            };
            if matches!(ingot.kind(db), IngotKind::Core | IngotKind::Std) {
                continue;
            }
            for implementation in ingot.all_impl_traits(db) {
                if !implementation.is_unsafe(db) || implementation.methods(db).next().is_some() {
                    continue;
                }
                if let Some(author) = self.first_untrusted(db, &impl_chain(db, *implementation)) {
                    found.push(UntrustedUnsafeImpl {
                        root,
                        implementation: *implementation,
                        author,
                    });
                }
            }
        }
        found
    }

    /// The unsafe code `owner` runs and the first untrusted ingot that
    /// contributed to it:
    /// - all of the body, for a method of an `unsafe impl` that an untrusted
    ///   ingot wrote, whose body is what the impl asserts correct (even if a
    ///   provider of a trusted ingot supplied it);
    /// - all of the body, for an `unsafe fn` that an untrusted ingot wrote or
    ///   placed;
    /// - else the first `unsafe` block, in any position of the body, that an
    ///   untrusted ingot wrote or placed. Each block has its own chain: a
    ///   fragment spliced into a body keeps the writer of its text, and the
    ///   quotations around the fragment are its placers.
    pub(super) fn untrusted_site<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> Option<(SemOrigin<'db>, Ingot<'db>)> {
        let body = owner.body(db)?;
        if let Some(chain) = unsafe_impl_chain(db, owner)
            && let Some(ingot) = self.first_untrusted(db, &chain)
        {
            return Some((SemOrigin::Body(owner), ingot));
        }
        if matches!(owner, BodyOwner::Func(func, _) if func.is_unsafe(db))
            && let Some(ingot) = self.first_untrusted(db, &body.quote_chain(db))
        {
            return Some((SemOrigin::Body(owner), ingot));
        }
        body.exprs(db).iter().find_map(|(expr, data)| {
            if !matches!(data, Partial::Present(Expr::Block(_, BlockKind::Unsafe))) {
                return None;
            }
            let ingot = self.first_untrusted(db, &body.expr_quote_chain(db, expr))?;
            Some((SemOrigin::Expr(expr), ingot))
        })
    }
}

/// The ingot that wrote the syntax of `owner`: the one that holds it, except
/// for generated code, which the package of the quotation wrote.
fn author_ingot<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Ingot<'db> {
    owner.scope().syntax_top_mod(db).ingot(db)
}

/// The modules of the quotations that wrote and placed the `unsafe impl` that
/// `owner` is a method of, if it is one, writer first. A header is written by
/// the ingot that holds it, though a provider of another ingot supplies its
/// members.
fn unsafe_impl_chain<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<Vec<TopLevelMod<'db>>> {
    let BodyOwner::Func(func, _) = owner else {
        return None;
    };
    match func.scope().parent_item(db) {
        Some(ItemKind::ImplTrait(impl_trait)) if impl_trait.is_unsafe(db) => {
            Some(impl_chain(db, impl_trait))
        }
        _ => None,
    }
}

/// The modules of the quotations that wrote and placed `impl_trait`, writer
/// first and the outermost placer last; the module that holds the syntax of a
/// hand-written one.
fn impl_chain<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TopLevelMod<'db>> {
    match impl_trait.quote_chain(db) {
        chain if chain.is_empty() => vec![impl_trait.scope().syntax_top_mod(db)],
        chain => chain,
    }
}

/// A call in the user's code reaches unsafe code in a dependency that no
/// trusted ingot allows to use it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct UntrustedUnsafeUse<'db> {
    /// The trust root whose code makes the call.
    pub root: Ingot<'db>,
    /// The call in the trust root's code.
    pub call: SemanticDiagnosticSpan<'db>,
    /// Whether `call` stands for the run of a generated `#[test]` function by
    /// `fe test`, which no call in the code names.
    pub runs_as_test: bool,
    /// The function whose body holds the unsafe code.
    pub user: BodyOwner<'db>,
    pub site: SemOrigin<'db>,
    /// The untrusted ingot that wrote the unsafe code.
    pub author: Ingot<'db>,
}

impl DiagnosticVoucher for UntrustedUnsafeUse<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let ingot = self.author;
        let name = ingot_name(db, ingot.base(db));
        let site = SemanticDiagnosticSpan::Origin {
            owner: self.user,
            origin: self.site,
        };
        let (title, primary) = if self.runs_as_test {
            (
                format!(
                    "generated test runs unsafe code in `{name}`, which is not allowed to use it"
                ),
                format!("this generated test runs unsafe code in `{name}`"),
            )
        } else {
            (
                format!("call reaches unsafe code in `{name}`, which is not allowed to use it"),
                format!("this call reaches unsafe code in `{name}`"),
            )
        };
        CompleteDiagnostic::new(
            Severity::Error,
            title,
            vec![
                SubDiagnostic::new(LabelStyle::Primary, primary, self.call.resolve(db)),
                SubDiagnostic::new(
                    LabelStyle::Secondary,
                    format!("unsafe code in `{name}`"),
                    site.resolve(db),
                ),
            ],
            vec![trust_fix(db, self.root, ingot, &name)],
            GlobalErrorCode::new(DiagnosticPass::SemanticAccess, 15),
        )
    }
}

/// An `unsafe impl` with no function, which no call reaches, that an untrusted
/// ingot wrote or placed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct UntrustedUnsafeImpl<'db> {
    /// The trust root whose program contains it.
    pub root: Ingot<'db>,
    pub implementation: ImplTrait<'db>,
    /// The first untrusted ingot that wrote or placed it.
    pub author: Ingot<'db>,
}

impl DiagnosticVoucher for UntrustedUnsafeImpl<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let name = ingot_name(db, self.author.base(db));
        CompleteDiagnostic::new(
            Severity::Error,
            format!("unsafe impl from `{name}`, which is not allowed to use unsafe code"),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                format!("`unsafe impl` that `{name}` wrote or placed"),
                self.implementation.span().resolve(db),
            )],
            vec![trust_fix(db, self.root, self.author, &name)],
            GlobalErrorCode::new(DiagnosticPass::SemanticAccess, 15),
        )
    }
}

/// How to trust `ingot`: allow it where the root depends on it, or trust a
/// dependent that already allows it.
fn trust_fix(
    db: &dyn SpannedHirAnalysisDb,
    root: Ingot<'_>,
    ingot: Ingot<'_>,
    name: &str,
) -> String {
    let graph = db.dependency_graph();
    let dependents = graph.dependents(db, ingot.base(db));
    let root = root.base(db);
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

fn ingot_name(db: &dyn HirAnalysisDb, url: &Url) -> String {
    db.workspace()
        .containing_ingot(db, url.clone())
        .and_then(|ingot| ingot.config(db).as_ref()?.metadata.name.clone())
        .map_or_else(|| url.to_string(), |name| name.to_string())
}
