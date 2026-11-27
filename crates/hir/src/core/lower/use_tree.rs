use parser::ast::{self, prelude::*};

use super::{FileLowerCtxt, path::quote_name_is_bound};
use crate::{
    hir_def::{AttrListId, AuthoredScope, IdentId, Partial, TrackedItemVariant, Use, use_tree::*},
    span::{HirOrigin, UseDesugared},
};

impl<'db> Use<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Use) -> Vec<Self> {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = super::lower_visibility(&ast);

        let Some(use_tree) = ast.use_tree() else {
            let id = ctxt.joined_id(TrackedItemVariant::Use(Partial::Absent));
            ctxt.enter_item_scope(id, false);
            let path = Partial::Absent;
            let alias = None;
            let top_mod = ctxt.top_mod();
            let origin = HirOrigin::raw(&ast);
            let use_ = Self::new(ctxt.db(), id, attributes, path, alias, vis, top_mod, origin);
            ctxt.leave_item_scope(use_);
            return vec![use_];
        };

        if ctxt.quote_names_resolve_at_producer() {
            refuse_glob_imports(ctxt, &use_tree);
        }

        // If the use tree has no subtree, then there is no need to decompose it.
        if !use_tree.has_subtree() {
            let path = UsePathId::lower_ast_partial(ctxt, use_tree.path(), &ast);
            let id = ctxt.joined_id(TrackedItemVariant::Use(path));
            ctxt.enter_item_scope(id, false);
            let alias = use_tree
                .alias()
                .map(|alias| UseAlias::lower_ast_partial(ctxt, alias));
            let top_mod = ctxt.top_mod();
            let origin = HirOrigin::raw(&ast);
            let use_ = Self::new(ctxt.db(), id, attributes, path, alias, vis, top_mod, origin);
            ctxt.leave_item_scope(use_);
            return vec![use_];
        }

        let decomposed_paths = decompose_tree(ctxt, ast, use_tree);
        decomposed_paths
            .into_iter()
            .map(|(path, alias, origin)| {
                let id = ctxt.joined_id(TrackedItemVariant::Use(path));
                ctxt.enter_item_scope(id, false);
                let top_mod = ctxt.top_mod();
                let origin = HirOrigin::desugared(origin);
                let use_ = Self::new(ctxt.db(), id, attributes, path, alias, vis, top_mod, origin);
                ctxt.leave_item_scope(use_)
            })
            .collect()
    }
}

/// Refuses a glob import in a quotation, at any depth of the use tree. The
/// names that a glob brings in are not known when the quotation is lowered, so
/// a name that only the glob provides would be looked up in the producer's
/// scope, where it can mean another item or nothing.
fn refuse_glob_imports(ctxt: &mut FileLowerCtxt<'_>, tree: &ast::UseTree) {
    if let Some(segment) = tree.path().and_then(|path| path.into_iter().last())
        && matches!(segment.kind(), Some(ast::UsePathSegmentKind::Glob(_)))
    {
        ctxt.record_generation_error(
            segment.syntax().text_range(),
            "a glob import is not supported inside a quotation; list the names to import, as in `use m::{A, B}`",
        );
    }
    for child in tree.children().into_iter().flatten() {
        refuse_glob_imports(ctxt, &child);
    }
}

impl<'db> UsePathId<'db> {
    fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::UsePath>,
        import: &ast::Use,
    ) -> Partial<Self> {
        let Some(ast) = ast else {
            return Partial::Absent;
        };
        Self::from_segments(ctxt, ast.into_iter().collect(), import)
    }

    fn from_segments(
        ctxt: &mut FileLowerCtxt<'db>,
        ast_segs: Vec<ast::UsePathSegment>,
        import: &ast::Use,
    ) -> Partial<Self> {
        let Some(first) = ast_segs.first() else {
            return Partial::Absent;
        };
        let authored_scope = quoted_import_scope(ctxt, first, import);
        let quote_origin = ctxt.quote_frames.last().map(|frame| frame.path_origin());
        let segments: Vec<_> = ast_segs
            .into_iter()
            .map(|segment| UsePathSegment::lower_ast_partial(ctxt, segment))
            .collect();
        Partial::Present(Self::new(ctxt.db(), segments, authored_scope, quote_origin))
    }
}

/// The path's free names are looked up at the quotation's producer, while its
/// import binding is introduced into the generated block.
fn quoted_import_scope<'db>(
    ctxt: &FileLowerCtxt<'db>,
    first: &ast::UsePathSegment,
    import: &ast::Use,
) -> Option<AuthoredScope<'db>> {
    let frame = ctxt.quote_frames.last()?;
    match first.kind()? {
        ast::UsePathSegmentKind::Ingot(_) => ctxt.quote_producer_in_other_ingot(),
        ast::UsePathSegmentKind::Super(_) if ctxt.quote_names_resolve_at_producer() => {
            Some(frame.producer)
        }
        ast::UsePathSegmentKind::Ident(name)
            if ctxt.quote_names_resolve_at_producer()
                && !quote_name_is_bound(
                    ctxt,
                    first.syntax(),
                    name.text(),
                    Some(import.syntax()),
                ) =>
        {
            Some(frame.producer)
        }
        _ => None,
    }
}

impl<'db> UsePathSegment<'db> {
    fn lower_ast_partial(ctxt: &mut FileLowerCtxt<'db>, ast: ast::UsePathSegment) -> Partial<Self> {
        let db = ctxt.db();

        ast.kind()
            .map(|kind| match kind {
                ast::UsePathSegmentKind::Ingot(_) => Self::Ident(IdentId::make_ingot(db)),
                ast::UsePathSegmentKind::Super(_) => Self::Ident(IdentId::make_super(db)),
                ast::UsePathSegmentKind::Ident(ident) => {
                    Self::Ident(IdentId::lower_token(ctxt, ident))
                }
                ast::UsePathSegmentKind::Self_(_) => Self::Ident(IdentId::make_self(db)),
                ast::UsePathSegmentKind::Glob(_) => Self::Glob,
            })
            .into()
    }
}

impl<'db> UseAlias<'db> {
    pub(super) fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: ast::UseAlias,
    ) -> Partial<Self> {
        if let Some(ident) = ast.ident() {
            Some(Self::Ident(IdentId::lower_token(ctxt, ident)))
        } else if ast.underscore().is_some() {
            Some(Self::Underscore)
        } else {
            None
        }
        .into()
    }
}

fn decompose_tree<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: ast::Use,
    use_tree: ast::UseTree,
) -> Vec<(
    Partial<UsePathId<'db>>,
    Option<Partial<UseAlias<'db>>>,
    UseDesugared,
)> {
    let use_desugared = UseDesugared::new(&ast);
    decompose_subtree(ctxt, use_tree, (vec![], use_desugared))
        .into_iter()
        .map(|(ast_segs, alias, desugared)| {
            let path = UsePathId::from_segments(ctxt, ast_segs, &ast);
            (path, alias, desugared)
        })
        .collect()
}

fn decompose_subtree<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    subtree: ast::UseTree,
    succ: (Vec<ast::UsePathSegment>, UseDesugared),
) -> Vec<(
    Vec<ast::UsePathSegment>,
    Option<Partial<UseAlias<'db>>>,
    UseDesugared,
)> {
    let (mut succ_path, mut succ_desugared) = succ;
    if let Some(path) = subtree.path() {
        let segs: Vec<ast::UsePathSegment> = path.into_iter().collect();

        let is_self_only = segs.len() == 1
            && segs[0]
                .kind()
                .is_some_and(|k| matches!(k, ast::UsePathSegmentKind::Self_(_)));

        for seg in segs {
            succ_desugared.push_seg(&seg);
            if !is_self_only {
                succ_path.push(seg.clone());
            }
        }
    }

    if let Some(alias) = subtree.alias() {
        succ_desugared.add_alias(&alias);
        let alias = UseAlias::lower_ast_partial(ctxt, alias);
        assert!(subtree.children().is_none());
        return vec![(succ_path, Some(alias), succ_desugared)];
    }

    let Some(children) = subtree.children() else {
        return vec![(succ_path, None, succ_desugared)];
    };

    children
        .into_iter()
        .flat_map(|subtree| {
            decompose_subtree(ctxt, subtree, (succ_path.clone(), succ_desugared.clone()))
        })
        .collect()
}
