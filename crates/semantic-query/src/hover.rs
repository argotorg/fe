use hir_analysis::diagnostics::SpannedHirAnalysisDb;

use hir::hir_def::scope_graph::ScopeId;
use hir::source_index::OccurrencePayload;
use hir::span::DynLazySpan;

#[derive(Debug, Clone)]
pub struct HoverSemantics<'db> {
    pub span: DynLazySpan<'db>,
    pub signature: Option<String>,
    pub documentation: Option<String>,
    pub kind: &'static str,
}

pub fn hover_for_occurrence<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    occ: &OccurrencePayload<'db>,
    top_mod: hir::hir_def::TopLevelMod<'db>,
) -> Option<HoverSemantics<'db>> {
    // Use the canonical occurrence interpreter to get the symbol target
    let target = crate::identity::occurrence_symbol_target(db, top_mod, occ)?;
    let symbol_key = crate::occ_target_to_symbol_key(target)?;
    
    // Get the span from the occurrence
    let span = get_span_from_occurrence(occ);
    
    // Convert symbol key to hover data
    hover_data_from_symbol_key(db, symbol_key, span)
}

fn get_span_from_occurrence<'db>(occ: &OccurrencePayload<'db>) -> DynLazySpan<'db> {
    match occ {
        OccurrencePayload::PathSeg { span, .. }
        | OccurrencePayload::UsePathSeg { span, .. }
        | OccurrencePayload::UseAliasName { span, .. }
        | OccurrencePayload::MethodName { span, .. }
        | OccurrencePayload::FieldAccessName { span, .. }
        | OccurrencePayload::PatternLabelName { span, .. }
        | OccurrencePayload::PathExprSeg { span, .. }
        | OccurrencePayload::PathPatSeg { span, .. }
        | OccurrencePayload::ItemHeaderName { span, .. } => span.clone(),
    }
}

fn hover_data_from_symbol_key<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    symbol_key: crate::SymbolKey<'db>,
    span: DynLazySpan<'db>,
) -> Option<HoverSemantics<'db>> {
    match symbol_key {
        crate::SymbolKey::Scope(sc) => {
            let signature = sc.pretty_path(db);
            let documentation = get_docstring(db, sc);
            let kind = sc.kind_name();
            Some(HoverSemantics { span, signature, documentation, kind })
        }
        crate::SymbolKey::Method(fd) => {
            let meth = fd.name(db).data(db).to_string();
            let signature = Some(format!("method: {}", meth));
            let documentation = get_docstring(db, fd.scope(db));
            Some(HoverSemantics { span, signature, documentation, kind: "method" })
        }
        crate::SymbolKey::Local(_func, bkey) => {
            let signature = Some(format!("local binding: {:?}", bkey));
            Some(HoverSemantics { span, signature, documentation: None, kind: "local" })
        }
        crate::SymbolKey::FuncParam(item, idx) => {
            let signature = Some(format!("parameter {} of {:?}", idx, item));
            Some(HoverSemantics { span, signature, documentation: None, kind: "parameter" })
        }
        crate::SymbolKey::EnumVariant(v) => {
            let sc = v.scope();
            let signature = sc.pretty_path(db);
            let documentation = get_docstring(db, sc);
            Some(HoverSemantics { span, signature, documentation, kind: "enum_variant" })
        }
    }
}

fn get_docstring(db: &dyn hir::HirDb, scope: ScopeId) -> Option<String> {
    use hir::hir_def::Attr;
    scope
        .attrs(db)?
        .data(db)
        .iter()
        .filter_map(|attr| match attr { Attr::DocComment(doc) => Some(doc.text.data(db).clone()), _ => None })
        .reduce(|a, b| a + "\n" + &b)
}
