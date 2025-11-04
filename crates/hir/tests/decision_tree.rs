mod test_db;
use std::path::Path;

use ascii_tree::{Tree, write_tree};
use dir_test::{Fixture, dir_test};
use fe_hir::analysis::ty::{
    decision_tree::{DecisionTree, Occurrence, build_decision_tree},
    pattern_analysis::PatternMatrix,
    simplified_pattern::ConstructorKind,
    ty_check::{TypedBody, check_func_body},
    ty_def::{TyData, TyId},
};
use fe_hir::hir_def::LitKind;
use fe_hir::{
    hir_def::{Expr, ExprId},
    visitor::prelude::*,
};
use test_db::{HirAnalysisTestDb, HirPropertyFormatter};
use test_utils::snap_test;

fn render_decision_tree<'db>(
    db: &'db dyn fe_hir::analysis::HirAnalysisDb,
    tree: &DecisionTree<'db>,
) -> String {
    let ascii_tree = convert_to_ascii_tree(db, tree);
    let mut output = String::new();
    write_tree(&mut output, &ascii_tree).unwrap();
    output
}

fn convert_to_ascii_tree<'db>(
    db: &'db dyn fe_hir::analysis::HirAnalysisDb,
    tree: &DecisionTree<'db>,
) -> Tree {
    match tree {
        DecisionTree::Leaf(leaf_node) => {
            let mut lines = vec![];

            // Add arm content with simple, robust format
            lines.push(format!("Execute arm #{}", leaf_node.arm_index));

            // Add bindings if present
            for ((name, _idx), occurrence) in &leaf_node.bindings {
                lines.push(format!(
                    "  {} ← {}",
                    name.data(db),
                    render_occurrence(occurrence)
                ));
            }

            Tree::Leaf(lines)
        }

        DecisionTree::Switch(switch_node) => {
            let mut children = Vec::new();

            for (case, subtree) in &switch_node.arms {
                let label = match case {
                    fe_hir::analysis::ty::decision_tree::Case::Constructor(ctor) => {
                        format!("{} =>", render_constructor(db, ctor))
                    }
                    fe_hir::analysis::ty::decision_tree::Case::Default => "_ =>".to_string(),
                };
                children.push(Tree::Node(label, vec![convert_to_ascii_tree(db, subtree)]));
            }

            Tree::Node(
                format!("Switch on {}", render_occurrence(&switch_node.occurrence)),
                children,
            )
        }
    }
}

fn render_occurrence(occurrence: &Occurrence) -> String {
    if occurrence.0.is_empty() {
        "expr".to_string()
    } else {
        let mut result = "expr".to_string();
        for &index in &occurrence.0 {
            use std::fmt::Write;
            write!(&mut result, ".{index}").unwrap();
        }
        result
    }
}

fn render_constructor<'db>(
    db: &'db dyn fe_hir::analysis::HirAnalysisDb,
    ctor: &ConstructorKind<'db>,
) -> String {
    match ctor {
        ConstructorKind::Variant(variant_kind, _) => {
            let variant_name = variant_kind.name(db).unwrap_or("unknown");
            variant_name.to_string()
        }
        ConstructorKind::Type(ty) => {
            if ty.is_tuple(db) {
                "tuple()".to_string()
            } else {
                "record{}".to_string()
            }
        }
        ConstructorKind::Literal(lit, _) => match lit {
            LitKind::Bool(b) => b.to_string(),
            LitKind::Int(int_id) => int_id.data(db).to_string(),
            LitKind::String(string_id) => format!("\"{}\"", string_id.data(db)),
        },
    }
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/test_files/decision_trees",
    glob: "*.fe"
)]
fn decision_tree_generation(fixture: Fixture<&str>) {
    let mut db = HirAnalysisTestDb::default();
    let path = Path::new(fixture.path());
    let file_name = path.file_name().and_then(|file| file.to_str()).unwrap();
    let file = db.new_stand_alone(file_name.into(), fixture.content());
    let (top_mod, mut prop_formatter) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let mut ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
    DecisionTreeVisitor {
        db: &db,
        top_mod,
        prop_formatter: &mut prop_formatter,
        current_func: None,
        typed_body: None,
    }
    .visit_top_mod(&mut ctxt, top_mod);

    let res = prop_formatter.finish(&db);
    snap_test!(res, fixture.path());
}

struct DecisionTreeVisitor<'db, 'a> {
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    prop_formatter: &'a mut HirPropertyFormatter<'db>,
    current_func: Option<String>,
    typed_body: Option<TypedBody<'db>>,
}

impl<'db> Visitor<'db> for DecisionTreeVisitor<'db, '_> {
    fn visit_func(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, fe_hir::span::item::LazyFuncSpan<'db>>,
        func: fe_hir::hir_def::Func<'db>,
    ) {
        self.current_func = func
            .name(self.db)
            .to_opt()
            .map(|name| name.data(self.db).to_string());

        // Get the typed body for this function
        let (_diags, typed_body) = check_func_body(self.db, func);
        self.typed_body = Some(typed_body.clone());

        walk_func(self, ctxt, func);

        // Clear typed body and current func after processing function
        self.typed_body = None;
        self.current_func = None;
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, fe_hir::span::expr::LazyExprSpan<'db>>,
        expr_id: ExprId,
        expr: &Expr<'db>,
    ) {
        if let Expr::Match(_scrutinee, arms) = expr
            && let Some(arms) = arms.clone().to_opt()
        {
            let body = ctxt.body();
            let patterns: Vec<_> = arms
                .iter()
                .filter_map(|arm| arm.pat.data(self.db, body).clone().to_opt())
                .collect();

            if !patterns.is_empty() {
                // Get the actual scrutinee type from the typed body
                let scrutinee_ty = if let Some(ref typed_body) = self.typed_body {
                    typed_body.expr_ty(self.db, *_scrutinee)
                } else {
                    TyId::new(self.db, TyData::Never)
                };

                let matrix = PatternMatrix::from_hir_patterns(
                    self.db,
                    &patterns,
                    body,
                    body.scope(),
                    scrutinee_ty,
                );

                let tree = build_decision_tree(self.db, &matrix);
                let visualization = render_decision_tree(self.db, &tree);

                let func_name = self.current_func.as_deref().unwrap_or("unknown");
                let prop = format!("Decision Tree for {func_name}:\n{visualization}");

                if let Some(span) = ctxt.span() {
                    self.prop_formatter
                        .push_prop(self.top_mod, span.into(), prop);
                }
            }
        }

        walk_expr(self, ctxt, expr_id);
    }
}
