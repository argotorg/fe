//! This module defines the diagnostics that can be accumulated inside salsa-db
//! with span-agnostic forms. All diagnostics accumulated in salsa-db should
//! implement [`DiagnosticVoucher`] which defines the conversion into
//! [`CompleteDiagnostic`].

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::diagnostics::{ImportDiag, PathResDiag},
    ty::{
        diagnostics::{
            BodyDiag, DefConflictError, FuncBodyDiag, ImplDiag, TraitConstraintDiag,
            TraitLowerDiag, TyDiagCollection, TyLowerDiag,
        },
        trait_def::TraitInstId,
        ty_check::{EffectParamOwner, RecordLike},
        ty_def::{TyData, TyVarSort},
    },
};
use crate::{
    ParserError, SpannedHirDb,
    hir_def::{CallableDef, FieldIndex, GenericParamOwner, PathKind, Trait},
    span::LazySpan,
};
use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, Span, SpanKind,
    SubDiagnostic,
};
use either::Either;
use itertools::Itertools;
use std::cmp::Ordering;

use common::file::File;

fn primary_diag(
    severity: Severity,
    message: &str,
    label: &str,
    span: Option<Span>,
    error_code: GlobalErrorCode,
) -> CompleteDiagnostic {
    CompleteDiagnostic::new(
        severity,
        message.to_string(),
        vec![SubDiagnostic::new(
            LabelStyle::Primary,
            label.to_string(),
            span,
        )],
        vec![],
        error_code,
    )
}

fn cmp_trait_inst_by_name<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    a: &TraitInstId<'db>,
    b: &TraitInstId<'db>,
) -> Ordering {
    let a_name = a.def(db).name(db).unwrap().data(db);
    let b_name = b.def(db).name(db).unwrap().data(db);
    a_name.cmp(b_name).then_with(|| {
        let a_self = a.self_ty(db).pretty_print(db).to_string();
        let b_self = b.self_ty(db).pretty_print(db).to_string();
        a_self.cmp(&b_self)
    })
}

/// All diagnostics accumulated in salsa-db should implement
/// [`DiagnosticVoucher`] which defines the conversion into
/// [`CompleteDiagnostic`].
///
/// All types that implement `DiagnosticVoucher` must NOT have a span
/// information which invalidates cache in salsa-db. Instead of it, the all
/// information is given by [`SpannedHirDb`] to allow evaluating span lazily.
///
/// The reason why we use `DiagnosticVoucher` is that we want to evaluate span
/// lazily to avoid invalidating cache in salsa-db.
///
/// To obtain a span from HIR nodes in a lazy manner, it's recommended to use
/// `[LazySpan]`(crate::core::span::LazySpan) and types that implement `LazySpan`.
pub trait DiagnosticVoucher: Send + Sync {
    /// Makes a [`CompleteDiagnostic`].
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic;
}

impl DiagnosticVoucher for CompleteDiagnostic {
    fn to_complete(&self, _db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        self.clone()
    }
}

#[salsa::db]
pub trait SpannedHirAnalysisDb:
    salsa::Database + crate::HirDb + crate::SpannedHirDb + HirAnalysisDb
{
}

#[salsa::db]
impl<T> SpannedHirAnalysisDb for T where T: HirAnalysisDb + SpannedHirDb {}

pub fn format_diags<'a, D>(
    db: &dyn SpannedHirAnalysisDb,
    diags: impl IntoIterator<Item = &'a D>,
) -> String
where
    D: DiagnosticVoucher + 'a,
{
    use codespan_reporting::term::{
        self,
        termcolor::{BufferWriter, ColorChoice},
    };

    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    let mut completes: Vec<_> = diags.into_iter().map(|diag| diag.to_complete(db)).collect();
    completes.sort_by(|lhs, rhs| match lhs.error_code.cmp(&rhs.error_code) {
        std::cmp::Ordering::Equal => lhs.primary_span().cmp(&rhs.primary_span()),
        ord => ord,
    });

    for diag in completes {
        term::emit(
            &mut buffer,
            &config,
            &CsDbWrapper(db),
            &complete_to_cs(diag),
        )
        .expect("diagnostic render should succeed");
    }

    std::str::from_utf8(buffer.as_slice())
        .expect("diagnostic output is valid utf8")
        .to_string()
}

fn complete_to_cs(
    complete: CompleteDiagnostic,
) -> codespan_reporting::diagnostic::Diagnostic<File> {
    use codespan_reporting::diagnostic as cs_diag;

    let severity = match complete.severity {
        Severity::Error => cs_diag::Severity::Error,
        Severity::Warning => cs_diag::Severity::Warning,
        Severity::Note => cs_diag::Severity::Note,
    };
    let code = Some(complete.error_code.to_string());

    let labels = complete
        .sub_diagnostics
        .into_iter()
        .filter_map(|sub| {
            let span = sub.span?;
            let style = match sub.style {
                LabelStyle::Primary => cs_diag::LabelStyle::Primary,
                LabelStyle::Secondary => cs_diag::LabelStyle::Secondary,
            };
            Some(cs_diag::Label::new(style, span.file, span.range).with_message(sub.message))
        })
        .collect();

    cs_diag::Diagnostic {
        severity,
        code,
        message: complete.message,
        labels,
        notes: complete.notes,
    }
}

struct CsDbWrapper<'a>(pub &'a dyn SpannedHirAnalysisDb);

impl<'db> codespan_reporting::files::Files<'db> for CsDbWrapper<'db> {
    type FileId = File;
    type Name = &'db camino::Utf8Path;
    type Source = &'db str;

    fn name(
        &'db self,
        file_id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        match file_id.path(self.0) {
            Some(path) => Ok(path.as_path()),
            None => Err(codespan_reporting::files::Error::FileMissing),
        }
    }

    fn source(
        &'db self,
        file_id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(file_id.text(self.0))
    }

    fn line_index(
        &'db self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let starts: Vec<_> = codespan_reporting::files::line_starts(file_id.text(self.0)).collect();
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'db self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        let line_starts: Vec<_> =
            codespan_reporting::files::line_starts(file_id.text(self.0)).collect();

        let start =
            *line_starts
                .get(line_index)
                .ok_or(codespan_reporting::files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len().saturating_sub(1),
                })?;

        let end = if line_index == line_starts.len().saturating_sub(1) {
            file_id.text(self.0).len()
        } else {
            *line_starts.get(line_index + 1).ok_or(
                codespan_reporting::files::Error::LineTooLarge {
                    given: line_index + 1,
                    max: line_starts.len().saturating_sub(1),
                },
            )?
        };

        Ok(std::ops::Range { start, end })
    }
}

// `ParseError` has span information, but this is not a problem because the
// parsing procedure itself depends on the file content, and thus span
// information.
impl DiagnosticVoucher for ParserError {
    fn to_complete(&self, _db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::Parse, 1);
        let span = Span::new(self.file, self.error.range(), SpanKind::Original);
        CompleteDiagnostic::new(
            Severity::Error,
            self.error.msg(),
            vec![SubDiagnostic::new(
                LabelStyle::Primary,
                self.error.label(),
                Some(span),
            )],
            vec![],
            error_code,
        )
    }
}

impl DiagnosticVoucher for crate::SelectorError {
    fn to_complete(&self, _db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        use crate::SelectorErrorKind;

        let primary_span = Span::new(self.file, self.primary_range, SpanKind::Original);

        let (code, message, label, notes, secondary) = match &self.kind {
            SelectorErrorKind::Overflow => (
                1,
                format!(
                    "selector value overflows u32 for msg variant `{}`",
                    self.variant_name
                ),
                "selector value exceeds u32::MAX".to_string(),
                vec!["selector must be a u32 integer".to_string()],
                None,
            ),
            SelectorErrorKind::InvalidType => (
                2,
                format!(
                    "selector must be an integer for msg variant `{}`",
                    self.variant_name
                ),
                "expected integer literal".to_string(),
                vec!["use an integer literal like `#[selector = 0x01]`".to_string()],
                None,
            ),
            SelectorErrorKind::Missing => (
                3,
                format!("missing selector for msg variant `{}`", self.variant_name),
                "no #[selector] attribute found".to_string(),
                vec!["add a #[selector = <value>] attribute to the variant".to_string()],
                None,
            ),
            SelectorErrorKind::InvalidForm => (
                5,
                format!(
                    "invalid selector attribute form for msg variant `{}`",
                    self.variant_name
                ),
                "expected `#[selector = <value>]` form".to_string(),
                vec!["use `#[selector = 0x01]` instead of `#[selector(0x01)]`".to_string()],
                None,
            ),
            SelectorErrorKind::Duplicate {
                first_variant_name,
                selector,
            } => (
                4,
                "duplicate selector in msg block".to_string(),
                format!(
                    "`{}` has selector {:#010x} which conflicts with `{}`",
                    self.variant_name, selector, first_variant_name
                ),
                vec!["each variant in a msg block must have a unique selector".to_string()],
                self.secondary_range.map(|range| SubDiagnostic {
                    style: LabelStyle::Secondary,
                    message: format!(
                        "`{first_variant_name}` with selector {selector:#010x} declared here"
                    ),
                    span: Some(Span::new(self.file, range, SpanKind::Original)),
                }),
            ),
        };

        let error_code = GlobalErrorCode::new(DiagnosticPass::MsgLower, code);

        let mut sub_diagnostics = vec![SubDiagnostic {
            style: LabelStyle::Primary,
            message: label,
            span: Some(primary_span),
        }];
        if let Some(sec) = secondary {
            sub_diagnostics.push(sec);
        }

        CompleteDiagnostic {
            severity: Severity::Error,
            message,
            sub_diagnostics,
            notes,
            error_code,
        }
    }
}

pub trait LazyDiagnostic<'db> {
    fn to_complete(&self, db: &'db dyn SpannedHirAnalysisDb) -> CompleteDiagnostic;
}

impl DiagnosticVoucher for DefConflictError<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let mut items = self.0.iter();
        let first = items.next().unwrap();
        let name = first.name(db).unwrap().data(db);
        CompleteDiagnostic {
            severity: Severity::Error,
            message: format!("conflicting definitions of `{name}`",),
            sub_diagnostics: {
                let mut subs = vec![SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{name}` is defined here"),
                    first.name_span().unwrap().resolve(db),
                )];
                subs.extend(items.map(|item| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"`{name}` is redefined here"},
                        item.name_span().unwrap().resolve(db),
                    )
                }));
                subs
            },
            notes: vec![],
            error_code: GlobalErrorCode::new(DiagnosticPass::TypeDefinition, 100),
        }
    }
}

impl DiagnosticVoucher for FuncBodyDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        match self {
            Self::Ty(diag) => diag.to_complete(db),
            Self::Body(diag) => diag.to_complete(db),
            Self::NameRes(diag) => diag.to_complete(db),
        }
    }
}

impl DiagnosticVoucher for TyDiagCollection<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        match self {
            Self::Ty(diag) => diag.to_complete(db),
            Self::PathRes(diag) => diag.to_complete(db),
            Self::Satisfiability(diag) => diag.to_complete(db),
            Self::TraitLower(diag) => diag.to_complete(db),
            Self::Impl(diag) => diag.to_complete(db),
        }
    }
}

impl DiagnosticVoucher for PathResDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::NameResolution, self.local_code());
        let severity = Severity::Error;
        match self {
            Self::Conflict(ident, conflicts) => {
                let ident = ident.data(db);
                let mut spans: Vec<_> = conflicts
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                spans.sort_unstable();
                let mut spans = spans.into_iter();
                let mut diags = Vec::with_capacity(conflicts.len());
                diags.push(SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is defined here"),
                    spans.next(),
                ));
                for sub_span in spans {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"`{ident}` is redefined here"},
                        Some(sub_span),
                    ));
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` conflicts with other definitions"),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotFound(prim_span, ident) => {
                let ident = ident.data(db);
                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is not found"),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{ident}` is not found"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodNotFound {
                primary,
                method_name,
                receiver,
            } => {
                let (recv_name, recv_ty, recv_kind) = match receiver {
                    Either::Left(ty) => (
                        ty.pretty_print(db),
                        Some(ty),
                        RecordLike::Type(*ty).kind_name(db),
                    ),
                    Either::Right(trait_) => {
                        let name = trait_.def(db).name(db).unwrap().data(db);
                        (name, None, "trait".to_string())
                    }
                };

                let method_str = method_name.data(db);
                let message =
                    format!("no method named `{method_str}` found for {recv_kind} `{recv_name}`");

                if let Some(ty) = recv_ty
                    && let Some(field_ty) = RecordLike::Type(*ty).record_field_ty(db, *method_name)
                {
                    return CompleteDiagnostic {
                        severity: Severity::Error,
                        message,
                        sub_diagnostics: vec![SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!(
                                "field `{}` in `{}` has type `{}`",
                                method_str,
                                recv_name,
                                field_ty.pretty_print(db)
                            ),
                            span: primary.resolve(db),
                        }],
                        notes: vec![],
                        error_code,
                    };
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message,
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("method not found in `{recv_name}`"),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::Invisible(prim_span, ident, span) => {
                let ident = ident.data(db);

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is not visible"),
                    span: prim_span.resolve(db),
                }];
                if let Some(span) = span {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{ident}` is defined here"),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is not visible"),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::Ambiguous(prim_span, ident, candidates) => {
                let ident = ident.data(db);
                let mut diags = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is ambiguous"),
                    span: prim_span.resolve(db),
                }];

                let mut cand_spans: Vec<_> = candidates
                    .iter()
                    .filter_map(|(span, from_implicit)| {
                        span.resolve(db).map(|resolved| (resolved, *from_implicit))
                    })
                    .collect();
                cand_spans.sort_unstable_by(|(a, _), (b, _)| a.cmp(b));
                diags.extend(cand_spans.into_iter().enumerate().map(
                    |(i, (span, from_implicit))| {
                        let label = if from_implicit {
                            format!("candidate {} (from implicit import)", i + 1)
                        } else {
                            format!("candidate {}", i + 1)
                        };
                        SubDiagnostic::new(LabelStyle::Secondary, label, Some(span))
                    },
                ));

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is ambiguous"),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousAssociatedType {
                span,
                name,
                candidates,
            } => {
                let name = name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("associated type `{name}` is ambiguous"),
                    span: span.resolve(db),
                }];

                for (trait_inst, ty) in candidates {
                    let trait_def = trait_inst.def(db);
                    let trait_name = trait_def.name(db).unwrap().data(db);
                    let span = |t: &Trait| t.span().name().resolve(db);
                    let span = span(&trait_def);

                    let msg = match ty.data(db) {
                        TyData::AssocTy(_) | TyData::Invalid(_) | TyData::Never => {
                            format!("candidate: `{trait_name}`")
                        }
                        _ => {
                            // Render as: candidate: <Self as Trait>::Name = Ty
                            let self_ty = trait_inst.self_ty(db).pretty_print(db);
                            let ty_str = ty.pretty_print(db);
                            format!("candidate: <{self_ty} as {trait_name}>::{name} = {ty_str}")
                        }
                    };

                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: msg,
                        span,
                    });
                }

                let (inst, _) = candidates
                    .iter()
                    .min_by(|(a, _), (b, _)| cmp_trait_inst_by_name(db, a, b))
                    .unwrap();
                let trait_name = inst.def(db).name(db).unwrap().data(db);
                let self_ty = inst.self_ty(db).pretty_print(db);
                let hint = format!(
                    "hint: specify the trait explicitly: `<{self_ty} as {trait_name}>::{name}`"
                );

                CompleteDiagnostic {
                    severity,
                    message: format!("ambiguous associated type `{name}`"),
                    sub_diagnostics,
                    notes: vec![hint],
                    error_code,
                }
            }

            Self::InvalidPathSegment {
                span: prim_span,
                segment,
                defined_at,
            } => {
                let segment = *segment;
                let label = match segment.kind(db) {
                    PathKind::Ident { ident, .. } => ident
                        .to_opt()
                        .map(|id| id.data(db).to_owned())
                        .unwrap_or_else(|| segment.pretty_print(db)),
                    PathKind::QualifiedType { type_, trait_ } => {
                        let ty = type_.pretty_print(db);
                        let trait_name = trait_.pretty_print(db);
                        format!("<{ty} as {trait_name}>")
                    }
                };
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{label}` can't be used as a middle segment of a path"),
                    span: prim_span.resolve(db),
                }];

                if let Some(span) = defined_at {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{label}` is defined here"),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{label}` can't be used as a middle segment of a path"),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedType(prim_span, name, given_kind) => {
                let name = name.data(db);
                CompleteDiagnostic {
                    severity,
                    message: "expected type item here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected type here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedTrait(prim_span, name, given_kind) => {
                let name = name.data(db);
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected trait item here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected trait here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::ExpectedValue(prim_span, name, given_kind) => {
                let name = name.data(db);
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected value here".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected value here, but found {given_kind} `{name}`"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::ArgNumMismatch {
                span,
                ident,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!(
                    "incorrect number of generic arguments for `{}`; expected {expected}, given {given}",
                    ident.data(db)
                ),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} arguments, but {given} were given"),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ArgKindMismatch {
                span,
                ident,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("invalid type argument kind for `{}`", ident.data(db)),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `{expected}` kind, but `{}` has `{}` kind",
                        given.pretty_print(db),
                        given.kind(db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ArgTypeMismatch {
                span,
                ident,
                expected,
                given,
            } => {
                let (header, message) = match (expected, given) {
                    (Some(exp), Some(giv)) => (
                        format!("const type mismatch for `{}`", ident.data(db)),
                        format!(
                            "expected `{}`, given `{}`",
                            exp.pretty_print(db),
                            giv.pretty_print(db)
                        ),
                    ),

                    (Some(exp), None) => (
                        format!("const generic argument expected for `{}`", ident.data(db),),
                        format!("expected const argument of type `{}`", exp.pretty_print(db)),
                    ),
                    (None, Some(giv)) => (
                        "unexpected const generic argument".to_string(),
                        format!(
                            "expected type generic argument, given const `{}`",
                            giv.pretty_print(db)
                        ),
                    ),
                    (None, None) => (
                        "invalid const argument".to_string(),
                        "unexpected const argument".to_string(),
                    ),
                };
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: header,
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message,
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TypeMustBeKnown(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "type must be known here".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "type must be known here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::AmbiguousInherentMethod {
                primary,
                method_name,
                candidates,
            } => {
                let method_name = method_name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{method_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                for cand in candidates {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{method_name}` is defined here"),
                        span: cand.name_span().resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "ambiguous method".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousTrait {
                primary,
                method_name,
                trait_insts,
            } => {
                let method_name = method_name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{method_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                // Name-resolution flavor: sort lexicographically by printed trait name
                let mut sorted: Vec<_> = trait_insts.iter().copied().collect();
                sorted.sort_by_key(|inst| inst.pretty_print(db, false));
                for inst in sorted.into_iter().rev() {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!(
                            "candidate: `{}::{method_name}`",
                            inst.pretty_print(db, false)
                        ),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "multiple trait candidates found".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvisibleAmbiguousTrait { primary, traits } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message:
                        "consider importing one of the following traits into the scope to resolve the ambiguity"
                            .to_string(),
                    span: primary.resolve(db),
                }];

                for trait_ in traits {
                    if let Some(path) = trait_.scope().pretty_path(db) {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("`use {path}`"),
                            span: primary.resolve(db),
                        });
                    }
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "trait is not in the scope".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousAssociatedConst {
                primary,
                name,
                trait_insts,
            } => {
                let const_name = name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{const_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                // Candidate labels at the trait declarations
                for inst in trait_insts {
                    let trait_def = inst.def(db);
                    let trait_name = trait_def.name(db).unwrap().data(db);
                    let trait_name_span = trait_def.span().name().resolve(db);
                    let self_ty = inst.self_ty(db).pretty_print(db);
                    let msg = format!("candidate: `<{self_ty} as {trait_name}>::{const_name}`");
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: msg,
                        span: trait_name_span,
                    });
                }

                let inst = trait_insts
                    .iter()
                    .min_by(|a, b| cmp_trait_inst_by_name(db, a, b))
                    .unwrap();
                let trait_name = inst.def(db).name(db).unwrap().data(db);
                let self_ty = inst.self_ty(db).pretty_print(db);
                let hint = format!(
                    "hint: specify the trait explicitly: `<{self_ty} as {trait_name}>::{const_name}`"
                );

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "multiple trait candidates found".to_string(),
                    sub_diagnostics,
                    notes: vec![hint],
                    error_code,
                }
            }
        }
    }
}

impl DiagnosticVoucher for ImportDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(
            DiagnosticPass::NameResolution,
            match self {
                ImportDiag::Conflict(..) => 1,
                ImportDiag::NotFound(..) => 2,
                ImportDiag::Invisible(..) => 3,
                ImportDiag::Ambiguous(..) => 4,
                ImportDiag::InvalidPathSegment(..) => 5,
            },
        );
        let severity = Severity::Error;
        match self {
            ImportDiag::Conflict(ident, conflicts) => {
                let ident = ident.data(db);
                let mut spans: Vec<_> = conflicts
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                spans.sort_unstable();
                let mut spans = spans.into_iter();
                let mut diags = Vec::with_capacity(conflicts.len());
                diags.push(SubDiagnostic::new(
                    LabelStyle::Primary,
                    format!("`{ident}` is defined here"),
                    spans.next(),
                ));
                for sub_span in spans {
                    diags.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format! {"`{ident}` is redefined here"},
                        Some(sub_span),
                    ));
                }

                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` conflicts with other definitions"),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }
            ImportDiag::NotFound(prim_span, ident) => {
                let ident = ident.data(db);
                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is not found"),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{ident}` is not found"),
                        span: prim_span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }
            ImportDiag::Invisible(prim_span, ident, span) => {
                let ident = ident.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is not visible"),
                    span: prim_span.resolve(db),
                }];
                if let Some(span) = span {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{ident}` is defined here"),
                        span: span.resolve(db),
                    });
                }
                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is not visible"),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            ImportDiag::Ambiguous(prim_span, ident, candidates) => {
                let ident = ident.data(db);
                let mut diags = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{ident}` is ambiguous"),
                    span: prim_span.resolve(db),
                }];
                let mut cand_spans: Vec<_> = candidates
                    .iter()
                    .filter_map(|span| span.resolve(db))
                    .collect();
                cand_spans.sort_unstable();
                diags.extend(cand_spans.into_iter().enumerate().map(|(i, span)| {
                    SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("candidate {}", i + 1),
                        Some(span),
                    )
                }));
                CompleteDiagnostic {
                    severity,
                    message: format!("`{ident}` is ambiguous"),
                    sub_diagnostics: diags,
                    notes: vec![],
                    error_code,
                }
            }
            ImportDiag::InvalidPathSegment(prim_span, name, res_span) => {
                let name = name.data(db);
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{name}` can't be used as a middle segment of a path"),
                    span: prim_span.resolve(db),
                }];
                if let Some(span) = res_span {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{name}` is defined here"),
                        span: span.resolve(db),
                    });
                }
                CompleteDiagnostic {
                    severity,
                    message: format!("`{name}` can't be used as a middle segment of a path"),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }
        }
    }
}

impl DiagnosticVoucher for TyLowerDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TypeDefinition, self.local_code());
        match self {
            Self::ExpectedStarKind(span) => {
                // find expected ty name, num of generic args, etc
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected `*` kind in this context".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "expected `*` kind here".to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvalidTypeArgKind {
                span,
                given,
                expected,
            } => {
                let msg = if let Some(expected) = expected {
                    let arg_kind = given.kind(db);
                    debug_assert!(!expected.does_match(arg_kind));

                    format!(
                        "expected `{}` kind, but `{}` has `{}` kind",
                        expected,
                        given.pretty_print(db),
                        arg_kind
                    )
                } else {
                    "too many generic arguments".to_string()
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid type argument kind".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg.to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TooManyGenericArgs {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("too many generic args; expected {expected}, given {given}"),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} arguments, but {given} were given"),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            // TODO: add hint about indirection (eg *T)
            Self::RecursiveType(cycle) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "recursive type definition".to_string(),
                sub_diagnostics: {
                    let head = cycle.first().unwrap();
                    let mut subs = vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "recursive type definition here".to_string(),
                        span: head.adt.adt_ref(db).name_span(db).resolve(db),
                    }];
                    subs.extend(cycle.iter().map(|m| {
                        SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: "recursion occurs here".to_string(),
                            span: m
                                .adt
                                .variant_ty_span(db, m.field_idx as usize, m.ty_idx as usize)
                                .resolve(db),
                        }
                    }));
                    subs
                },
                notes: vec![],
                error_code,
            },
            Self::UnboundTypeAliasParam {
                span,
                alias,
                n_given_args: _,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "all type parameters of type alias must be given".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: {
                            use crate::hir_def::GenericParamOwner;
                            let n_params = GenericParamOwner::TypeAlias(*alias).params(db).count();
                            format!("expected at least {} arguments here", n_params)
                        },
                        span: span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "type alias defined here".to_string(),
                        span: alias.span().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::TypeAliasCycle { cycle } => {
                let mut cycle = cycle.clone();
                cycle.sort_by_key(|a| a.span().resolve(db));

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "type alias cycle".to_string(),
                    sub_diagnostics: {
                        let mut iter = cycle.iter();
                        let mut labels = vec![SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: "cycle happens here".to_string(),
                            span: iter.next_back().unwrap().span().ty().resolve(db),
                        }];
                        labels.extend(iter.map(|type_alias| SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: "type alias defined here".to_string(),
                            span: type_alias.span().alias().resolve(db),
                        }));
                        labels
                    },
                    notes: vec![],
                    error_code,
                }
            }

            Self::InconsistentKindBound { span, ty, bound } => {
                let msg = format!(
                    "`{}` is already declared with `{}` kind, but found `{}` kind here",
                    ty.pretty_print(db),
                    ty.kind(db),
                    bound
                );

                CompleteDiagnostic {
                    severity: Severity::Error,
                    // TODO improve message
                    message: "duplicate type bound is not allowed.".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg.to_string(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::KindBoundNotAllowed(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "kind bound is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "kind bound is not allowed here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::GenericParamAlreadyDefinedInParent {
                span,
                conflict_with,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "generic parameter is already defined in the parent item".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is already defined", name.data(db)),
                        span: span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "conflict with this generic parameter".to_string(),
                        span: conflict_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::DuplicateArgName(func, idxs) => {
                let views: Vec<_> = func.params(db).collect();
                let name = views[idxs[0] as usize]
                    .name(db)
                    .expect("param name")
                    .data(db);

                let pspan = func.span().params();
                let spans = idxs
                    .iter()
                    .map(|i| pspan.clone().param(*i as usize).name().resolve(db));

                let message = if let Some(name) = func.name(db).to_opt() {
                    format!("duplicate argument name in function `{}`", name.data(db))
                } else {
                    "duplicate argument name in function definition".into()
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message,
                    sub_diagnostics: duplicate_name_subdiags(name, spans),
                    notes: vec![],
                    error_code,
                }
            }

            Self::DuplicateFieldName(parent, idxs) => {
                let name = parent
                    .fields(db)
                    .nth(idxs[0] as usize)
                    .and_then(|v| v.name(db))
                    .expect("field not found")
                    .data(db);

                let spans = idxs
                    .iter()
                    .map(|i| parent.field_name_span(*i as usize).resolve(db));

                let kind = parent.kind_name();
                let message = if let Some(name) = parent.name(db) {
                    format!("duplicate field name in {kind} `{name}`")
                } else {
                    format!("duplicate field name in {kind} definition")
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message,
                    sub_diagnostics: duplicate_name_subdiags(name, spans),
                    notes: vec![],
                    error_code,
                }
            }

            Self::DuplicateVariantName(enum_, idxs) => {
                let message = if let Some(name) = enum_.name(db).to_opt() {
                    format!("duplicate variant name in enum `{}`", name.data(db))
                } else {
                    "duplicate variant name in enum definition".into()
                };

                let name = enum_
                    .variants(db)
                    .nth(idxs[0] as usize)
                    .and_then(|v| v.name(db))
                    .expect("variant not found")
                    .data(db);
                let spans = idxs
                    .iter()
                    .map(|i| enum_.span().variants().variant(*i as usize).resolve(db));
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message,
                    sub_diagnostics: duplicate_name_subdiags(name, spans),
                    notes: vec![],
                    error_code,
                }
            }

            Self::DuplicateGenericParamName(owner, idxs) => {
                let message = if let Some(name) = owner.name(db) {
                    format!(
                        "duplicate generic parameter name in {} `{}`",
                        owner.kind_name(),
                        name.data(db)
                    )
                } else {
                    format!(
                        "duplicate generic parameter name in {} definition",
                        owner.kind_name()
                    )
                };

                let name = owner
                    .params(db)
                    .next()
                    .map(|p| p.param.name().unwrap().data(db))
                    .expect("should be at least one generic param");

                let spans = offending_generic_param_spans(*owner, idxs.as_slice(), db);
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message,
                    sub_diagnostics: duplicate_name_subdiags(name, spans),
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvalidConstParamTy(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "invalid const parameter type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "only integer or bool types are allowed as a const parameter type"
                        .to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::RecursiveConstParamTy(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "recursive const parameter type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "recursive const parameter type is detected here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyMismatch {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "given type doesn't match the expected const type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `{}` type here, but `{}` is given",
                        expected.pretty_print(db),
                        given.pretty_print(db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyExpected { span, expected } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "expected const type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected const type of `{}` here",
                        expected.pretty_print(db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::NormalTypeExpected { span, given } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "expected a normal type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected a normal type here, but `{}` is given",
                        given.pretty_print(db)
                    ),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidConstTyExpr(span) => primary_diag(
                Severity::Error,
                "the expression is not supported in a const type context",
                "expected a literal, const, or const fn call",
                span.resolve(db),
                error_code,
            ),

            Self::ConstEvalUnsupported(span) => primary_diag(
                Severity::Error,
                "the expression cannot be evaluated at compile time",
                "unsupported in const evaluation",
                span.resolve(db),
                error_code,
            ),

            Self::ConstEvalNonConstCall(span) => primary_diag(
                Severity::Error,
                "non-const function call in const context",
                "calls in const context must be `const fn`",
                span.resolve(db),
                error_code,
            ),

            Self::ConstEvalDivisionByZero(span) => primary_diag(
                Severity::Error,
                "division by zero in const context",
                "cannot divide by zero",
                span.resolve(db),
                error_code,
            ),

            Self::ConstEvalStepLimitExceeded(span) => primary_diag(
                Severity::Error,
                "const evaluation exceeded the step limit",
                "const evaluation takes too many steps",
                span.resolve(db),
                error_code,
            ),

            Self::ConstEvalRecursionLimitExceeded(span) => primary_diag(
                Severity::Error,
                "const evaluation exceeded the recursion limit",
                "const evaluation recurses too deeply",
                span.resolve(db),
                error_code,
            ),

            Self::NonTrailingDefaultGenericParam(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "generic parameters with a default must be trailing".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "must not be followed by a parameter with no default".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::GenericDefaultForwardRef { span, name } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "cannot reference generic parameter before it is declared".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("cannot reference `{}` before it's declared", name.data(db)),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

fn offending_generic_param_spans<'db>(
    owner: crate::core::hir_def::GenericParamOwner<'db>,
    idxs: &'db [u16],
    db: &'db dyn SpannedHirAnalysisDb,
) -> impl Iterator<Item = Option<Span>> + 'db {
    let params_vec: Vec<_> = owner.params(db).collect();
    idxs.iter()
        .map(move |i| params_vec[*i as usize].span().resolve(db))
}

fn duplicate_name_subdiags<I>(name: &str, spans: I) -> Vec<SubDiagnostic>
where
    I: Iterator<Item = Option<Span>>,
{
    let mut spans = spans;
    let mut subs = vec![SubDiagnostic::new(
        LabelStyle::Primary,
        format!("`{name}` is defined here"),
        spans.next().unwrap(),
    )];
    subs.extend(spans.map(|span| {
        SubDiagnostic::new(
            LabelStyle::Secondary,
            format!("`{name}` is redefined here"),
            span,
        )
    }));
    subs
}

impl DiagnosticVoucher for BodyDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TyCheck, self.local_code());
        let severity = Severity::Error;

        match self {
            Self::TypeMismatch {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity,
                message: "type mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `{}`, but `{}` is given",
                        expected.pretty_print(db),
                        given.pretty_print(db),
                    ),
                    span: span.resolve(db),
                }],
                error_code,
                notes: vec![],
            },
            Self::InfiniteOccurrence(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "infinite sized type found".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "infinite sized type found".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedBinding {
                primary,
                conflicat_with,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("duplicate binding `{}` in pattern", name.data(db)),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is defined again here", name.data(db)),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("first definition of `{}` in this pattern", name.data(db)),
                        span: conflicat_with.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedRestPat(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "duplicate `..` in pattern".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "`..` can be used only once".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidPathDomainInPat { primary, resolved } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "expected type or enum variant here".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some(resolved) = resolved {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "this item given".to_string(),
                        span: resolved.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid item is given here".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::UnitVariantExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected unit variant here, but found {kind_name}"),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{hint}` instead"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected unit variant".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::TupleVariantExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: if let Some(kind_name) = kind_name {
                        format!("expected tuple variant here, but found {kind_name}")
                    } else {
                        "expected tuple variant here".to_string()
                    },
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{hint}` instead"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected tuple variant".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::RecordExpected {
                primary,
                kind_name,
                hint,
            } => {
                let mut labels = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: if let Some(kind_name) = kind_name {
                        format!("expected record variant or struct here, but found {kind_name}")
                    } else {
                        "expected record variant or struct here".to_string()
                    },
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    labels.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{hint}` instead"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "expected record variant or struct".to_string(),
                    sub_diagnostics: labels,
                    notes: vec![],
                    error_code,
                }
            }

            Self::MismatchedFieldCount {
                primary,
                expected,
                given,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "field count mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} fields here, but {given} given"),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::DuplicatedRecordFieldBind {
                primary,
                first_use,
                name,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "duplicated record field binding".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("duplicate field binding `{}`", name.data(db)),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("first use of `{}`", name.data(db)),
                        span: first_use.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::RecordFieldNotFound { span, label } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "specified field not found".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("field `{}` not found", label.data(db)),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ExplicitLabelExpectedInRecord { primary, hint } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "explicit label is required".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{hint}` instead"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "explicit label is required".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::MissingRecordFields {
                primary,
                missing_fields,
                hint,
            } => {
                let missing = missing_fields
                    .iter()
                    .map(|id| id.data(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("missing `{missing}`"),
                    span: primary.resolve(db),
                }];

                if let Some(hint) = hint {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("Consider using `{hint}` instead"),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "missing fields in record pattern".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::UndefinedVariable(primary, ident) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "undefined variable".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("undefined variable `{}`", ident.data(db)),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::InvalidEffectKey { owner, key, idx } => {
                let idx = *idx;
                let key_str = key.pretty_print(db);
                let span = owner.effect_param_path_span(db, idx).resolve(db);
                let effect = owner.effects(db).data(db).get(idx);
                let is_labeled = effect.and_then(|e| e.name).is_some();
                let is_contract_scoped_uses = match owner {
                    EffectParamOwner::Contract(_) => false,
                    EffectParamOwner::ContractInit { .. } => true,
                    EffectParamOwner::ContractRecvArm { .. } => true,
                    EffectParamOwner::Func(func) => matches!(
                        func.scope().parent_item(db),
                        Some(crate::hir_def::ItemKind::Contract(_))
                    ),
                };

                let message = "unresolved effect".to_string();
                let severity = Severity::Error;
                if is_contract_scoped_uses && !is_labeled {
                    CompleteDiagnostic {
                        severity,
                        message,
                        sub_diagnostics: vec![SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!("unknown effect `{}`", key_str),
                            span,
                        }],
                        notes: vec![
                            "add it to the contract `uses (...)` clause or add a matching contract field"
                                .to_string(),
                        ],
                        error_code,
                    }
                } else {
                    CompleteDiagnostic {
                        severity,
                        message,
                        sub_diagnostics: vec![SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!("cannot resolve `{}` as a type or trait", key_str),
                            span,
                        }],
                        notes: vec![
                            format!("consider defining a type or trait named `{}`", key_str),
                            "or bind the effect value with `uses (name: Type)`".to_string(),
                        ],
                        error_code,
                    }
                }
            }

            Self::ContractRootEffectTraitNotImplemented {
                owner,
                idx,
                root_ty,
                trait_req,
            } => {
                let span = owner.effect_param_path_span(db, *idx).resolve(db);
                let root = root_ty.pretty_print(db);

                CompleteDiagnostic {
                    severity,
                    message: "unsupported contract effect".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "contract root effect `{root}` does not implement `{}`",
                            trait_req.pretty_print(db, false),
                        ),
                        span,
                    }],
                    notes: vec![format!(
                        "contract-scoped trait effects must be implemented by the target `RootEffect` (`{root}`)"
                    )],
                    error_code,
                }
            }

            Self::ContractRootEffectTypeNotZeroSized {
                owner,
                key,
                idx,
                given,
            } => {
                let idx = *idx;
                let span = owner.effect_param_path_span(db, idx).resolve(db);
                let key_str = key.pretty_print(db);
                let given_str = given.pretty_print(db);

                CompleteDiagnostic {
                    severity,
                    message: "unsupported contract effect".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "contract-scoped type effects must be zero-sized, but `{key_str}` resolves to `{given_str}`"
                        ),
                        span,
                    }],
                    notes: vec![
                        "use a trait effect (implemented by the target `RootEffect`) or use a zero-sized type".to_string(),
                    ],
                    error_code,
                }
            }

            Self::MissingEffect { primary, func, key } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("missing effect `{}` required by `{}`", key_str, func_name),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "`{}` requires effect `{}` to be in scope",
                            func_name, key_str
                        ),
                        span: primary.resolve(db),
                    }],
                    notes: vec![format!(
                        "provide it with `with ({} = value)` or require it via `uses {}`",
                        key_str, key_str
                    )],
                    error_code,
                }
            }

            Self::AmbiguousEffect { primary, func, key } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "multiple effect candidates found".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "effect `{}` is ambiguous when calling `{}`",
                            key_str, func_name
                        ),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::EffectMutabilityMismatch {
                primary,
                func,
                key,
                provided_span,
            } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{}` requires `mut {}`", func_name, key_str),
                    span: primary.resolve(db),
                }];

                if let Some(span) = provided_span.as_ref().map(|s| s.resolve(db)) {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("effect `{}` is provided here", key_str),
                        span,
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "effect `{}` must be mutable when calling `{}`",
                        key_str, func_name
                    ),
                    sub_diagnostics,
                    notes: vec![
                        "use a mutable binding or pass a mutable reference in the `with` block"
                            .to_string(),
                    ],
                    error_code,
                }
            }

            Self::EffectTypeMismatch {
                primary,
                func,
                key,
                expected,
                given,
                provided_span,
            } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);
                let expected_ty = expected.pretty_print(db).to_string();
                let given_ty = given.pretty_print(db).to_string();

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `{}` for effect `{}`, found `{}`",
                        expected_ty, key_str, given_ty
                    ),
                    span: primary.resolve(db),
                }];

                if let Some(span) = provided_span.as_ref().map(|s| s.resolve(db)) {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("effect `{}` is provided here", key_str),
                        span,
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "effect `{}` provided to `{}` has type `{}`, but `{}` is required",
                        key_str, func_name, given_ty, expected_ty
                    ),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::EffectProviderMismatch {
                primary,
                func,
                key,
                expected,
                given,
                provided_span,
            } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);
                let expected_ty = expected.pretty_print(db).to_string();
                let given_ty = given.pretty_print(db).to_string();

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected effect provider `{}`, found `{}` for `{}`",
                        expected_ty, given_ty, key_str
                    ),
                    span: primary.resolve(db),
                }];

                if let Some(span) = provided_span.as_ref().map(|s| s.resolve(db)) {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("effect `{}` is provided here", key_str),
                        span,
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "effect provider mismatch for `{}` when calling `{}`",
                        key_str, func_name
                    ),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::EffectTraitUnsatisfied {
                primary,
                func,
                key,
                trait_req,
                given,
                provided_span,
            } => {
                let func_name = func
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let key_str = key.pretty_print(db);
                let trait_str = trait_req.pretty_print(db, false);
                let given_ty = given.pretty_print(db).to_string();

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "`{}` must implement `{}` for effect `{}`",
                        given_ty, trait_str, key_str
                    ),
                    span: primary.resolve(db),
                }];

                if let Some(span) = provided_span.as_ref().map(|s| s.resolve(db)) {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("effect `{}` is provided here", key_str),
                        span,
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!(
                        "effect `{}` supplied to `{}` does not satisfy `{}`",
                        key_str, func_name, trait_str
                    ),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::ReturnedTypeMismatch {
                primary,
                actual,
                expected,
                func,
            } => {
                let actual = actual.pretty_print(db);
                let expected = expected.pretty_print(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected `{expected}`, but `{actual}` is returned"),
                    span: primary.resolve(db),
                }];

                if let Some(func) = func {
                    let has_explicit = match func {
                        CallableDef::Func(f) => f.has_explicit_return_ty(db),
                        CallableDef::VariantCtor(_) => false,
                    };

                    // For explicit return types, point at the return type span;
                    // otherwise, point at the function name span (where a return
                    // type could be added).
                    let name_span = func.name_span();
                    let span = match (has_explicit, func) {
                        (true, CallableDef::Func(f)) => f.span().ret_ty().into(),
                        _ => name_span,
                    };

                    if has_explicit {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("this function expects `{expected}` to be returned"),
                            span: span.resolve(db),
                        });
                    } else {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("try adding `-> {actual}`"),
                            span: span.resolve(db),
                        });
                    }
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "returned type mismatch".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            Self::TypeMustBeKnown(span) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "type must be known".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "type must be known here".to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstValueMustBeKnown(span) => primary_diag(
                severity,
                "const value must be resolvable during type checking",
                "requires fully-resolved const value",
                span.resolve(db),
                error_code,
            ),

            Self::InvalidCast {
                primary,
                from,
                to,
                hint,
            } => {
                let notes = if let Some(hint) = hint {
                    // Use the specific hint instead of the generic downcast suggestion.
                    vec![hint.clone()]
                } else if from.is_bool(db) || to.is_bool(db) {
                    vec!["casts involving `bool` are not supported".to_string()]
                } else {
                    vec![concat!(
                        "try using `.downcast()` for checked narrowing/sign changes, ",
                        "or `.downcast_truncate()` / `.downcast_saturate()` / `.downcast_unchecked()`"
                    )
                    .to_string()]
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "cast is not provably lossless".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "cannot cast `{}` to `{}` with `as`",
                            from.pretty_print(db),
                            to.pretty_print(db),
                        ),
                        span: primary.resolve(db),
                    }],
                    notes,
                    error_code,
                }
            }
            Self::AccessedFieldNotFound {
                primary,
                given_ty,
                index,
            } => {
                let message = match index {
                    FieldIndex::Ident(ident) => format!(
                        "field `{}` is not found in `{}`",
                        ident.data(db),
                        given_ty.pretty_print(db)
                    ),
                    FieldIndex::Index(index) => format!(
                        "field `{}` is not found in `{}`",
                        index.data(db),
                        given_ty.pretty_print(db)
                    ),
                };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "invalid field index".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message,
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::OpsTraitNotImplemented {
                span,
                ty,
                op,
                trait_path,
            } => {
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` can't be applied to `{}`", op.data(db), ty),
                        span: span.resolve(db),
                    },
                    // TODO move to hint
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!(
                            "Try implementing `{}` for `{}`",
                            trait_path.pretty_print(db),
                            ty
                        ),
                        span: span.resolve(db),
                    },
                ];

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{}` trait is not implemented", trait_path.pretty_print(db)),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            Self::UnsupportedUnaryPlus(primary) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "unary `+` is not supported".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "remove the unary `+`".to_string(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::NonAssignableExpr(primary) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "not assignable left-hand side of assignment".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "cant assign to this expression".to_string(),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ImmutableAssignment { primary, binding } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "immutable assignment".to_string(),
                    span: primary.resolve(db),
                }];

                if let Some((name, span)) = binding {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("try changing to `mut {}`", name.data(db)),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "left-hand side of assignment is immutable".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::LoopControlOutsideOfLoop { primary, is_break } => {
                let stmt = if *is_break { "break" } else { "continue" };

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{stmt}` is not allowed outside of a loop"),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{stmt}` is not allowed here"),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TraitNotImplemented {
                primary,
                ty,
                trait_name,
            } => {
                let trait_name = trait_name.data(db);

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("`{trait_name}` needs to be implemented for {ty}"),
                    sub_diagnostics: vec![
                        SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!("`{trait_name}` needs to be implemented for `{ty}`"),
                            span: primary.resolve(db),
                        },
                        SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("consider implementing `{trait_name}` for `{ty}`"),
                            span: primary.resolve(db),
                        },
                    ],
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotCallable(primary, ty) => {
                let ty = ty.pretty_print(db);
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: format!("expected function, found `{ty}`"),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "call expression requires function; `{ty}` is not callable"
                        ),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::CallGenericArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "given generic argument number mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected {expected} generic arguments, but {given} given"
                        ),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "function defined here".to_string(),
                        span: def_span.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CallArgNumMismatch {
                primary,
                def_span,
                given,
                expected,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "argument number mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected {expected} arguments, but {given} given"),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "function defined here".to_string(),
                        span: def_span.resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CallArgLabelMismatch {
                primary,
                def_span,
                given,
                expected,
            } => {
                let mut sub_diagnostics = if let Some(given) = given {
                    vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected `{}` label, but `{}` given",
                            expected.data(db),
                            given.data(db)
                        ),
                        span: primary.resolve(db),
                    }]
                } else {
                    vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("expected `{}` label", expected.data(db)),
                        span: primary.resolve(db),
                    }]
                };

                sub_diagnostics.push(SubDiagnostic {
                    style: LabelStyle::Secondary,
                    message: "function defined here".to_string(),
                    span: def_span.resolve(db),
                });

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "argument label mismatch".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotAMethod {
                span,
                receiver_ty,
                func_name,
                func_ty,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: format!("`{}` is not a method", func_name.data(db)),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "`{}` is an associated function, not a method",
                            func_name.data(db),
                        ),
                        span: span.clone().method_name().resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "help: use associated function syntax instead: `{}::{}`",
                            receiver_ty.pretty_print(db),
                            func_name.data(db)
                        ),
                        span: span.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "function defined here".to_string(),
                        span: func_ty.name_span(db).unwrap().resolve(db),
                    },
                ],
                notes: vec![
                    "note: to be used as a method, a function must have a `self` parameter"
                        .to_string(),
                ],
                error_code,
            },

            Self::AmbiguousInherentMethodCall {
                primary,
                method_name,
                candidates,
            } => {
                let method_name = method_name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{method_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                for cand in candidates {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{method_name}` is defined here"),
                        span: cand.name_span().resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "ambiguous method call".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousTrait {
                primary,
                method_name,
                traits,
            } => {
                let method_name = method_name.data(db);
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{method_name}` is ambiguous"),
                    span: primary.resolve(db),
                }];

                // Body-diag flavor: prefer lexical trait name asc; if candidates share the
                // same trait and have generic args, order by first arg desc (u32 before i32).
                let mut sorted: Vec<_> = traits.iter().copied().collect();
                let names: Vec<String> = sorted
                    .iter()
                    .map(|t| {
                        t.pretty_print(db, false)
                            .split('<')
                            .next()
                            .unwrap_or("")
                            .to_string()
                    })
                    .collect();
                let all_same_trait = names.iter().all(|n| *n == names[0]);
                let has_first_arg = sorted.iter().any(|t| t.args(db).get(1).is_some());
                if all_same_trait && has_first_arg {
                    sorted.sort_by(|a, b| {
                        let sa: String = a
                            .args(db)
                            .get(1)
                            .map_or(String::new(), |t| t.pretty_print(db).to_string());
                        let sb: String = b
                            .args(db)
                            .get(1)
                            .map_or(String::new(), |t| t.pretty_print(db).to_string());
                        sb.cmp(&sa)
                    });
                } else {
                    // Sort by trait name lexicographically
                    sorted.sort_by(|a, b| {
                        let na: String = a
                            .def(db)
                            .name(db)
                            .to_opt()
                            .map(|id| id.data(db).to_string())
                            .unwrap_or_else(|| {
                                a.pretty_print(db, false)
                                    .split('<')
                                    .next()
                                    .unwrap_or("")
                                    .to_string()
                            });
                        let nb: String = b
                            .def(db)
                            .name(db)
                            .to_opt()
                            .map(|id| id.data(db).to_string())
                            .unwrap_or_else(|| {
                                b.pretty_print(db, false)
                                    .split('<')
                                    .next()
                                    .unwrap_or("")
                                    .to_string()
                            });
                        na.cmp(&nb)
                    });
                }
                for trait_ in sorted.into_iter().rev() {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!(
                            "candidate: `{}::{method_name}`",
                            trait_.pretty_print(db, false)
                        ),
                        span: primary.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "multiple trait candidates found".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::AmbiguousTraitInst { primary, cands } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "multiple implementations are found".to_string(),
                    span: primary.resolve(db),
                }];

                let mut sorted: Vec<_> = cands.iter().copied().collect();
                sorted.sort_by_key(|a| a.pretty_print(db, false));
                for cand in sorted {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("candidate: {}", cand.pretty_print(db, false)),
                        span: primary.resolve(db), // TODO cand span??
                    });
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "ambiguous trait implementation".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvisibleAmbiguousTrait { primary, traits } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "consider importing one of the following traits into the scope to resolve the ambiguity".to_string(),
                    span: primary.resolve(db),
                }];

                for trait_ in traits {
                    if let Some(path) = trait_.scope().pretty_path(db) {
                        sub_diagnostics.push(SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: format!("`use {path}`"),
                            span: primary.resolve(db),
                        });
                    }
                }

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "trait is not in the scope".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::NotValue { primary, given } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "value is expected".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "`{}` cannot be used as a value",
                        match given {
                            Either::Left(item) => item.kind_name(),
                            Either::Right(_) => "type",
                        }
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::TypeAnnotationNeeded { span: primary, ty } => {
                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "type annotation is needed".to_string(),
                    span: primary.resolve(db),
                }];

                let sub_diag_msg = match ty.base_ty(db).data(db) {
                    TyData::TyVar(var) if var.sort == TyVarSort::Integral => {
                        "no default type is provided for an integer type. consider giving integer type".to_string()
                    }
                    TyData::TyVar(_) => "consider giving `: Type` here".to_string(),
                    _ => format!("consider giving `: {}` here", ty.pretty_print(db)),
                };

                sub_diagnostics.push(SubDiagnostic {
                    style: LabelStyle::Secondary,
                    message: sub_diag_msg,
                    span: primary.resolve(db),
                });

                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "type annotation is needed".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::NonExhaustiveMatch {
                primary,
                scrutinee_ty,
                missing_patterns,
            } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "match expression does not cover all possible values".to_string(),
                    span: primary.resolve(db),
                }];
                let notes = if !missing_patterns.is_empty() {
                    let message = if missing_patterns.len() == 1 {
                        format!("Not covered: `{}`", missing_patterns[0])
                    } else {
                        format!("Not covered: `{}`", missing_patterns.join("`, `"))
                    };
                    vec![message]
                } else {
                    vec![]
                };
                CompleteDiagnostic {
                    severity,
                    message: format!(
                        "non-exhaustive patterns: type `{}` is not covered",
                        scrutinee_ty.pretty_print(db)
                    ),
                    sub_diagnostics,
                    notes,
                    error_code,
                }
            }
            BodyDiag::UnreachablePattern { primary } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "this pattern is unreachable".to_string(),
                    span: primary.resolve(db),
                }];
                let notes = vec!["previous patterns already cover all possible values".to_string()];
                CompleteDiagnostic {
                    severity,
                    message: "unreachable pattern".to_string(),
                    sub_diagnostics,
                    notes,
                    error_code,
                }
            }
            BodyDiag::RecvExpectedMsgType { primary, given } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected `msg` type, but `{}` is given",
                        given.pretty_print(db)
                    ),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "recv block expects a msg type".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvArmNotMsgVariant { primary, msg_name } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected variant of `{}`", msg_name.data(db)),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "recv arm pattern is not a variant of the msg type".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvArmRetTypeMissing { primary, expected } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "return type must be annotated as `{}` to match the msg variant",
                        expected.pretty_print(db)
                    ),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "recv arm return type annotation required".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvArmDuplicateVariant {
                primary,
                first_use,
                variant,
            } => {
                let name = variant.data(db);
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("duplicate handling of `{name}`"),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "first handled here".to_string(),
                        span: first_use.resolve(db),
                    },
                ];
                CompleteDiagnostic {
                    severity,
                    message: "duplicate msg variant in recv block".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvMissingMsgVariants { primary, variants } => {
                let missing = variants
                    .iter()
                    .map(|ident| ident.data(db).to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("missing variants: {missing}"),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "recv block missing msg variants".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvDuplicateMsgBlock {
                primary,
                first_use,
                msg_name,
            } => {
                let msg = msg_name.data(db);
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("duplicate recv block for `{msg}`"),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "first declared here".to_string(),
                        span: first_use.resolve(db),
                    },
                ];
                CompleteDiagnostic {
                    severity,
                    message: "duplicate recv block".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvDuplicateSelector {
                primary,
                first_use,
                selector,
                first_variant,
                second_variant,
            } => {
                let first = first_variant.data(db);
                let second = second_variant.data(db);
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "`{second}` has selector 0x{selector:08x} which conflicts with `{first}`"
                        ),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{first}` with selector 0x{selector:08x} declared here"),
                        span: first_use.resolve(db),
                    },
                ];
                CompleteDiagnostic {
                    severity,
                    message: "duplicate selector across recv blocks".to_string(),
                    sub_diagnostics,
                    notes: vec![
                        "each msg variant in a contract must have a unique selector".to_string(),
                    ],
                    error_code,
                }
            }
            BodyDiag::RecvArmNotVariantOfMsg {
                primary,
                variant_ty,
                msg_name,
            } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "`{}` is not a variant of `{}`",
                        variant_ty.pretty_print(db),
                        msg_name.data(db)
                    ),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "type is not a variant of the specified msg".to_string(),
                    sub_diagnostics,
                    notes: vec![
                        "in a named recv block, only variants defined in that `msg` block are allowed".to_string(),
                    ],
                    error_code,
                }
            }
            BodyDiag::RecvArmNotMsgVariantTrait { primary, given_ty } => {
                let sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "`{}` does not implement `MsgVariant`",
                        given_ty.pretty_print(db)
                    ),
                    span: primary.resolve(db),
                }];
                CompleteDiagnostic {
                    severity,
                    message: "type does not implement MsgVariant trait".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }
            BodyDiag::RecvDuplicateHandler {
                primary,
                first_use,
                handler_ty,
            } => {
                let ty_str = handler_ty.pretty_print(db).to_string();
                let sub_diagnostics = vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("`{}` is already handled", ty_str),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: format!("`{}` first handled here", ty_str),
                        span: first_use.resolve(db),
                    },
                ];
                CompleteDiagnostic {
                    severity,
                    message: "duplicate message handler".to_string(),
                    sub_diagnostics,
                    notes: vec![
                        "each message type can only be handled once in a contract".to_string(),
                    ],
                    error_code,
                }
            }

            BodyDiag::ConstFnEffectsNotAllowed(primary) => primary_diag(
                severity,
                "effects are not allowed in a `const fn`",
                "remove the `uses (...)` clause",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnWithNotAllowed(primary) => primary_diag(
                severity,
                "`with` expressions are not allowed in a `const fn`",
                "`with` is not supported in const evaluation",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnLoopNotAllowed(primary) => primary_diag(
                severity,
                "loops are not allowed in a `const fn`",
                "loops are not supported in const evaluation (MVP)",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnMatchNotAllowed(primary) => primary_diag(
                severity,
                "`match` is not allowed in a `const fn`",
                "`match` is not supported in const evaluation (MVP)",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnAssignmentNotAllowed(primary) => primary_diag(
                severity,
                "assignment is not allowed in a `const fn`",
                "mutation is not supported in const evaluation (MVP)",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnAggregateNotAllowed(primary) => primary_diag(
                severity,
                "aggregate operations are not allowed in a `const fn`",
                "aggregates are not supported in const evaluation (MVP)",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnMutableBindingNotAllowed(primary) => primary_diag(
                severity,
                "`mut` bindings are not allowed in a `const fn`",
                "mutation is not supported in const evaluation (MVP)",
                primary.resolve(db),
                error_code,
            ),

            BodyDiag::ConstFnNonConstCall { primary, callee } => {
                let name = callee
                    .name(db)
                    .map(|n| n.data(db).as_str())
                    .unwrap_or("<unknown>");
                CompleteDiagnostic::new(
                    severity,
                    "non-const call in `const fn`".to_string(),
                    vec![
                        SubDiagnostic::new(
                            LabelStyle::Primary,
                            format!("`{name}` is not a `const fn`"),
                            primary.resolve(db),
                        ),
                        SubDiagnostic::new(
                            LabelStyle::Secondary,
                            "callee defined here".to_string(),
                            callee.name_span().resolve(db),
                        ),
                    ],
                    vec![],
                    error_code,
                )
            }

            BodyDiag::ConstFnEffectfulCall { primary, callee } => {
                let name = callee
                    .name(db)
                    .map(|n| n.data(db).as_str())
                    .unwrap_or("<unknown>");
                CompleteDiagnostic::new(
                    severity,
                    "effectful call in `const fn`".to_string(),
                    vec![
                        SubDiagnostic::new(
                            LabelStyle::Primary,
                            format!("`{name}` requires effects"),
                            primary.resolve(db),
                        ),
                        SubDiagnostic::new(
                            LabelStyle::Secondary,
                            "callee defined here".to_string(),
                            callee.name_span().resolve(db),
                        ),
                    ],
                    vec![],
                    error_code,
                )
            }
        }
    }
}

impl DiagnosticVoucher for TraitLowerDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code =
            GlobalErrorCode::new(DiagnosticPass::ImplTraitDefinition, self.local_code());
        match self {
            Self::ExternalTraitForExternalType(impl_trait) => CompleteDiagnostic {
                severity: Severity::Error,
                message: "external trait cannot be implemented for external type".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: "external trait cannot be implemented for external type".to_string(),
                    span: impl_trait.span().resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConflictTraitImpl {
                primary,
                conflict_with,
            } => CompleteDiagnostic {
                severity: Severity::Error,
                message: "conflicting trait implementations".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "this trait implementation".to_string(),
                        span: primary.span().ty().resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "conflicts with this trait implementation".to_string(),
                        span: conflict_with.span().ty().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::CyclicSuperTraits(traits) => {
                let span = |t: &Trait| t.span().name().resolve(db);
                CompleteDiagnostic {
                    severity: Severity::Error,
                    message: "cyclic trait bounds are not allowed".to_string(),
                    sub_diagnostics: {
                        let mut subs = vec![SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: "trait cycle detected here".to_string(),
                            span: span(traits.first().unwrap()),
                        }];
                        subs.extend(traits.iter().skip(1).map(|t| SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: "cycle continues here".to_string(),
                            span: span(t),
                        }));
                        subs
                    },
                    notes: vec![],
                    error_code,
                }
            }
        }
    }
}

impl DiagnosticVoucher for TraitConstraintDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code());
        let severity = Severity::Error;
        match self {
            Self::KindMismatch { primary, trait_def } => CompleteDiagnostic {
                severity,
                message: "type doesn't satisfy required kind bound".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "type doesn't satisfy required kind bound here".to_string(),
                        span: primary.resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "trait is defined here".to_string(),
                        span: trait_def.span().name().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::TraitArgNumMismatch {
                span,
                expected,
                given,
            } => CompleteDiagnostic {
                severity,
                message: "given trait argument number mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("expected {expected} arguments here, but {given} given"),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::TraitArgKindMismatch {
                span,
                expected,
                actual,
            } => {
                let actual_kind = actual.kind(db);
                let ty_display = actual.pretty_print(db);

                CompleteDiagnostic {
                    severity,
                    message: "given trait argument kind mismatch".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected `{expected}` kind, but `{ty_display}` has `{actual_kind}` kind",
                        ),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::TraitBoundNotSat {
                span,
                primary_goal,
                unsat_subgoal,
            } => {
                let msg = format!(
                    "`{}` doesn't implement `{}`",
                    primary_goal.self_ty(db).pretty_print(db),
                    primary_goal.pretty_print(db, false)
                );

                let unsat_subgoal = unsat_subgoal.map(|unsat| {
                    format!(
                        "trait bound `{}` is not satisfied",
                        unsat.pretty_print(db, true)
                    )
                });

                let mut sub_diagnostics = vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }];

                if let Some(subgoal) = unsat_subgoal {
                    sub_diagnostics.push(SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: subgoal.to_string(),
                        span: span.resolve(db),
                    });
                }

                CompleteDiagnostic {
                    severity,
                    message: "trait bound is not satisfied".to_string(),
                    sub_diagnostics,
                    notes: vec![],
                    error_code,
                }
            }

            Self::InfiniteBoundRecursion(span, msg) => CompleteDiagnostic {
                severity,
                message: "infinite trait bound recursion".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: msg.to_string(),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConcreteTypeBound(span, ty) => CompleteDiagnostic {
                severity,
                message: "trait bound for concrete type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{}` is a concrete type", ty.pretty_print(db)),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstTyBound(span, ty) => CompleteDiagnostic {
                severity,
                message: "trait bound for const type is not allowed".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!("`{}` is a const type", ty.pretty_print(db)),
                    span: span.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}

impl DiagnosticVoucher for ImplDiag<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let error_code = GlobalErrorCode::new(DiagnosticPass::TraitSatisfaction, self.local_code());
        let severity = Severity::Error;

        match self {
            Self::ConflictMethodImpl {
                primary,
                conflict_with,
            } => CompleteDiagnostic {
                severity,
                message: "conflicting method implementations".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "".into(),
                        span: primary.name_span().resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: "".into(),
                        span: conflict_with.name_span().resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::MethodNotDefinedInTrait {
                primary,
                trait_,
                method_name,
            } => CompleteDiagnostic {
                severity,
                message: "method not defined in trait".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "method `{}` is not defined in trait `{}`",
                        method_name.data(db),
                        trait_.name(db).unwrap().data(db)
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::NotAllTraitItemsImplemented {
                primary,
                not_implemented,
            } => {
                let missing = not_implemented
                    .iter()
                    .map(|id| id.data(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                CompleteDiagnostic {
                    severity,
                    message: "not all trait methods are implemented".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!("missing implementations: {missing}"),
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodTypeParamNumMismatch { trait_m, impl_m } => {
                let impl_params = impl_m.explicit_params(db);
                let trait_params = trait_m.explicit_params(db);

                CompleteDiagnostic {
                    severity,
                    message: "method type parameter count mismatch".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected {} type parameters, but {} given",
                            trait_params.len(),
                            impl_params.len(),
                        ),
                        span: impl_m.name_span().resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodTypeParamKindMismatch {
                trait_m,
                impl_m,
                param_idx,
            } => {
                let message = format!(
                    "expected `{}` kind, but the given type has `{}` kind",
                    trait_m.explicit_params(db)[*param_idx].kind(db),
                    impl_m.explicit_params(db)[*param_idx].kind(db),
                );

                // Prefer to highlight the specific generic type parameter that
                // mismatches, falling back to the whole parameter list if we
                // cannot resolve it (e.g., for variant constructors).
                let span = match impl_m {
                    CallableDef::Func(func) => {
                        // Map from "explicit param index" back to the original
                        // index in the owner's generic parameter list.
                        let offset = impl_m.offset_to_explicit_params_position(db);
                        let original_idx = offset + *param_idx;
                        let owner = GenericParamOwner::Func(*func);

                        owner
                            .params(db)
                            .nth(original_idx)
                            .map(|p| p.name_span().resolve(db))
                            .unwrap_or_else(|| impl_m.param_list_span().resolve(db))
                    }
                    _ => impl_m.param_list_span().resolve(db),
                };

                CompleteDiagnostic {
                    severity,
                    message: "method type parameter kind mismatch".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message,
                        span,
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodArgNumMismatch { trait_m, impl_m } => CompleteDiagnostic {
                severity,
                message: "method argument count mismatch".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "expected {} arguments, but {} given",
                        trait_m.arg_tys(db).len(),
                        impl_m.arg_tys(db).len(),
                    ),
                    span: impl_m.param_list_span().resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MethodArgLabelMismatch {
                trait_m,
                impl_m,
                param_idx,
            } => CompleteDiagnostic {
                severity,
                message: "method argument label mismatch".to_string(),
                sub_diagnostics: vec![
                    SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: format!(
                            "expected `{}` label, but the given label is `{}`",
                            trait_m
                                .param_label_or_name(db, *param_idx)
                                .unwrap()
                                .pretty_print(db),
                            impl_m
                                .param_label_or_name(db, *param_idx)
                                .unwrap()
                                .pretty_print(db),
                        ),
                        span: impl_m.param_span(*param_idx).resolve(db),
                    },
                    SubDiagnostic {
                        style: LabelStyle::Secondary,
                        message: "argument label defined here".to_string(),
                        span: trait_m.param_span(*param_idx).resolve(db),
                    },
                ],
                notes: vec![],
                error_code,
            },

            Self::MethodArgTyMismatch {
                trait_m,
                impl_m,
                trait_m_ty,
                impl_m_ty,
                param_idx,
            } => {
                let method_name = impl_m.name(db).expect("methods have names").data(db);

                CompleteDiagnostic {
                    severity,
                    message: format!("method `{method_name}` has incompatible argument type"),
                    sub_diagnostics: vec![
                        SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!(
                                "expected `{}`, found `{}`",
                                trait_m_ty.pretty_print(db),
                                impl_m_ty.pretty_print(db)
                            ),
                            span: impl_m.param_span(*param_idx).resolve(db),
                        },
                        SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: "trait requires this type".to_string(),
                            span: trait_m.param_span(*param_idx).resolve(db),
                        },
                    ],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodRetTyMismatch {
                trait_m,
                impl_m,
                trait_ty,
                impl_ty,
            } => {
                let method_name = impl_m.name(db).expect("methods have names").data(db);

                CompleteDiagnostic {
                    severity,
                    message: format!("method `{method_name}` has incompatible return type"),
                    sub_diagnostics: vec![
                        SubDiagnostic {
                            style: LabelStyle::Primary,
                            message: format!(
                                "expected `{}`, found `{}`",
                                trait_ty.pretty_print(db),
                                impl_ty.pretty_print(db),
                            ),
                            span: impl_m.name_span().resolve(db),
                        },
                        SubDiagnostic {
                            style: LabelStyle::Secondary,
                            message: "trait requires this return type".to_string(),
                            span: trait_m.name_span().resolve(db),
                        },
                    ],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MethodStricterBound {
                span,
                stricter_bounds,
            } => {
                // TODO sort!
                // unsatisfied_goals.sort_by_key(|goal| goal.self_ty(db).pretty_print(db));

                let message = format!(
                    "method has stricter bounds than the declared method in the trait: {}",
                    stricter_bounds
                        .iter()
                        .map(|pred| format!("`{}`", pred.pretty_print(db, true)))
                        .join(", ")
                );
                CompleteDiagnostic {
                    severity,
                    message: "method has stricter bounds than trait".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: message.clone(),
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::InvalidSelfType {
                span,
                expected,
                given,
            } => {
                let message = if expected.is_trait_self(db) {
                    format!(
                        "type of `self` must start with `Self`, but the given type is `{}`",
                        given.pretty_print(db),
                    )
                } else {
                    format!(
                        "type of `self` must start with `Self` or `{}`, but the given type is `{}`",
                        expected.pretty_print(db),
                        given.pretty_print(db),
                    )
                };

                CompleteDiagnostic {
                    severity,
                    message: "invalid type for `self` parameter".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message,
                        span: span.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::InherentImplIsNotAllowed {
                primary,
                ty,
                is_nominal,
            } => {
                let msg = if *is_nominal {
                    format!("inherent impl is not allowed for foreign type `{ty}`")
                } else {
                    "inherent impl is not allowed for non nominal type".to_string()
                };

                CompleteDiagnostic {
                    severity,
                    message: "invalid inherent implementation".to_string(),
                    sub_diagnostics: vec![SubDiagnostic {
                        style: LabelStyle::Primary,
                        message: msg,
                        span: primary.resolve(db),
                    }],
                    notes: vec![],
                    error_code,
                }
            }

            Self::MissingAssociatedType {
                primary,
                type_name,
                trait_,
            } => CompleteDiagnostic {
                severity,
                message: "missing associated type in trait implementation".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "missing associated type `{}` from trait `{}`",
                        type_name.data(db),
                        trait_.name(db).unwrap().data(db)
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MissingAssociatedConstValue {
                primary,
                const_name,
                trait_,
            } => CompleteDiagnostic {
                severity,
                message: "missing associated const value in trait implementation".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "missing value for associated const `{}` from trait `{}`",
                        const_name.data(db),
                        trait_.name(db).unwrap().data(db)
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::ConstNotDefinedInTrait {
                primary,
                trait_,
                const_name,
            } => CompleteDiagnostic {
                severity,
                message: "associated const not defined in trait".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "associated const `{}` is not defined in trait `{}`",
                        const_name.data(db),
                        trait_.name(db).unwrap().data(db)
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },

            Self::MissingAssociatedConst {
                primary,
                const_name,
                trait_,
            } => CompleteDiagnostic {
                severity,
                message: "missing associated const in trait implementation".to_string(),
                sub_diagnostics: vec![SubDiagnostic {
                    style: LabelStyle::Primary,
                    message: format!(
                        "missing associated const `{}` from trait `{}`",
                        const_name.data(db),
                        trait_.name(db).unwrap().data(db)
                    ),
                    span: primary.resolve(db),
                }],
                notes: vec![],
                error_code,
            },
        }
    }
}
