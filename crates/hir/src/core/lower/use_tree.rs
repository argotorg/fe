use parser::ast::{self, prelude::*};

use super::FileLowerCtxt;
use crate::{
    hir_def::{AttrListId, IdentId, Partial, TrackedItemVariant, Use, use_tree::*},
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

        // If the use tree has no subtree, then there is no need to decompose it.
        if !use_tree.has_subtree() {
            let path = UsePathId::lower_ast_partial(ctxt, use_tree.path());
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

impl<'db> UsePathId<'db> {
    fn lower_ast_partial(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::UsePath>,
    ) -> Partial<Self> {
        let Some(ast) = ast else {
            return Partial::Absent;
        };

        let segments = ast
            .into_iter()
            .map(|ast| UsePathSegment::lower_ast_partial(ctxt, ast))
            .collect::<Vec<_>>();
        Some(Self::new(ctxt.db(), segments)).into()
    }

    fn from_segments(
        ctxt: &mut FileLowerCtxt<'db>,
        ast_segs: Vec<ast::UsePathSegment>,
    ) -> Partial<Self> {
        if ast_segs.is_empty() {
            Partial::Absent
        } else {
            let segs = ast_segs
                .into_iter()
                .map(|seg| UsePathSegment::lower_ast_partial(ctxt, seg))
                .collect::<Vec<_>>();
            Partial::Present(Self::new(ctxt.db(), segs))
        }
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
            let path = UsePathId::from_segments(ctxt, ast_segs);
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
