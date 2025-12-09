use hir::{
    HirDb, SpannedHirDb,
    hir_def::{Attr, ItemKind, scope_graph::ScopeId},
    span::LazySpan,
};

pub fn get_docstring(db: &dyn HirDb, scope: ScopeId) -> Option<String> {
    scope
        .attrs(db)?
        .data(db)
        .iter()
        .filter_map(|attr| {
            if let Attr::DocComment(doc) = attr {
                Some(doc.text.data(db).clone())
            } else {
                None
            }
        })
        .reduce(|a, b| a + "\n" + &b)
}

pub fn get_item_path_markdown(db: &dyn HirDb, item: ItemKind) -> Option<String> {
    item.scope()
        .pretty_path(db)
        .map(|path| format!("```fe\n{path}\n```"))
}

pub fn get_item_definition_markdown(db: &dyn SpannedHirDb, item: ItemKind) -> Option<String> {
    // TODO: use pending AST features to get the definition without all this text manipulation
    let span = item.span().resolve(db)?;

    let mut start: usize = span.range.start().into();
    let mut end: usize = span.range.end().into();

    // if the item has a body or children, cut that stuff out
    let body_start = match item {
        ItemKind::Func(func) => Some(func.body(db)?.span().resolve(db)?.range.start()),
        ItemKind::Mod(module) => Some(module.scope().name_span(db)?.resolve(db)?.range.end()),
        // TODO: handle other item types
        _ => None,
    };
    if let Some(body_start) = body_start {
        end = body_start.into();
    }

    // let's start at the beginning of the line where the name is defined
    let name_span = item.name_span()?.resolve(db);
    if let Some(name_span) = name_span {
        let mut name_line_start = name_span.range.start().into();
        let file_text = span.file.text(db).as_str();
        while name_line_start > 0 && file_text.chars().nth(name_line_start - 1).unwrap() != '\n' {
            name_line_start -= 1;
        }
        start = name_line_start;
    }

    let item_definition = span.file.text(db).as_str()[start..end].to_string();
    Some(format!("```fe\n{}\n```", item_definition.trim()))
}
