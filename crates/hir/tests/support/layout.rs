macro_rules! parse_module {
    (trusted $db:ident, $top_mod:ident, $source:expr $(,)?) => {
        let mut $db = fe_hir::test_db::HirAnalysisTestDb::default();
        let file = $db.new_trusted_effect_handle_module(
            camino::Utf8PathBuf::from(concat!(module_path!(), "_", line!(), ".fe")),
            $source,
        );
        let ($top_mod, _) = $db.top_mod(file);
    };
    ($db:ident, $top_mod:ident, $source:expr $(,)?) => {
        let mut $db = fe_hir::test_db::HirAnalysisTestDb::default();
        let file = $db.new_stand_alone(
            camino::Utf8PathBuf::from(concat!(module_path!(), "_", line!(), ".fe")),
            $source,
        );
        let ($top_mod, _) = $db.top_mod(file);
    };
}

pub(crate) use parse_module;

macro_rules! parse_ok {
    (trusted $db:ident, $top_mod:ident, $source:expr $(,)?) => {
        $crate::layout_test_support::parse_module!(trusted $db, $top_mod, $source);
        $db.assert_no_diags($top_mod);
    };
    ($db:ident, $top_mod:ident, $source:expr $(,)?) => {
        $crate::layout_test_support::parse_module!($db, $top_mod, $source);
        $db.assert_no_diags($top_mod);
    };
}

pub(crate) use parse_ok;
