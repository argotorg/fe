//! Normalization limits are errors of the program, never types. Whether a
//! type reaches a limit depends only on the type, not on the order in which
//! its parts are normalized.

use camino::Utf8PathBuf;
use common::diagnostics::CompleteDiagnostic;
use fe_hir::{
    analysis::ty::{
        normalize::{NormalizationLimit, normalize_ty},
        trait_def::TraitInstId,
        trait_resolution::PredicateListId,
        ty_def::{TyData, TyId},
    },
    test_db::{HirAnalysisTestDb, find_func, initialize_test_analysis_pass},
};

const PRELUDE: &str = r#"
trait Nest {
    type Out
}

struct N<T> {
    t: T,
}

impl<T: Nest> Nest for N<T> {
    type Out = T::Out
}

impl Nest for u8 {
    type Out = u8
}

trait Tr {
    type Out
}

struct W<T> {
    t: T,
}

impl<T> Tr for W<T> {
    type Out = <W<(T, T)> as Tr>::Out
}

trait Keep<T> {
    type Out
}

struct K {}

impl<T> Keep<T> for K {
    type Out = u8
}

trait Cy {
    type Out
}

struct A {}

struct B {}

impl Cy for A {
    type Out = <B as Cy>::Out
}

impl Cy for B {
    type Out = <A as Cy>::Out
}
"#;

/// A projection that needs `depth` levels of nesting.
fn chain(depth: usize) -> String {
    format!(
        "<{}u8{} as Nest>::Out",
        "N<".repeat(depth),
        ">".repeat(depth)
    )
}

/// A projection whose argument is `inner`, which it ignores.
fn keep(inner: &str) -> String {
    format!("<K as Keep<{inner}>>::Out")
}

fn permutations(items: &[String]) -> Vec<Vec<String>> {
    if items.len() <= 1 {
        return vec![items.to_vec()];
    }
    let mut out = Vec::new();
    for i in 0..items.len() {
        let mut rest = items.to_vec();
        let first = rest.remove(i);
        for mut perm in permutations(&rest) {
            perm.insert(0, first.clone());
            out.push(perm);
        }
    }
    out
}

/// The diagnostics for `src`, each with its line. Identical diagnostics from
/// different passes are shown once, as the driver does.
fn diagnostics(db: &mut HirAnalysisTestDb, src: &str) -> Vec<(CompleteDiagnostic, usize)> {
    let file = db.new_stand_alone("normalization_limits.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    let mut diags: Vec<CompleteDiagnostic> = manager
        .run_on_module(db, top_mod)
        .into_iter()
        .map(|diag| diag.to_complete(db))
        .collect();
    let mut seen = Vec::new();
    diags.retain(|diag| {
        let new = !seen.contains(diag);
        if new {
            seen.push(diag.clone());
        }
        new
    });
    diags
        .into_iter()
        .map(|diag| {
            let start = diag
                .sub_diagnostics
                .iter()
                .find_map(|sub| sub.span.as_ref())
                .map_or(0, |span| usize::from(span.range.start()));
            let line = src[..start].matches('\n').count();
            (diag, line)
        })
        .collect()
}

/// Every permutation of independent tuple components reaches a limit or
/// none does.
#[test]
fn reaching_a_limit_does_not_depend_on_the_order_of_components() {
    // Each case lists independent components and whether a limit is reached.
    let cases: Vec<(Vec<String>, bool)> = vec![
        (vec![chain(40), chain(50), chain(60)], false),
        // `chain(70)` reaches `chain(40)` 30 levels down: a component computed
        // first must not make that deeper use of it pass.
        (vec![chain(40), chain(70)], true),
        (vec![chain(40), keep(&chain(70)), chain(50)], true),
        (vec![keep(&chain(40)), chain(60)], false),
        (vec![chain(30), keep("<W<u8> as Tr>::Out"), chain(20)], true),
        (
            vec![
                chain(40),
                format!("({}, {})", chain(50), keep(&chain(70))),
                chain(10),
            ],
            true,
        ),
        // Two projections defined through each other: the cycle is found
        // whichever is met first, and next to whatever else.
        (
            vec![chain(60), "<A as Cy>::Out".to_string(), keep(&chain(20))],
            true,
        ),
    ];

    let mut src = PRELUDE.to_string();
    // The line of each function, with its case.
    let mut functions = Vec::new();
    for (case, (components, _)) in cases.iter().enumerate() {
        for (perm_idx, perm) in permutations(components).into_iter().enumerate() {
            let line = src.matches('\n').count();
            src.push_str(&format!(
                "fn case{case}_{perm_idx}(_ x: ({})) {{}}\n",
                perm.join(", ")
            ));
            functions.push((line, case));
        }
    }

    let mut db = HirAnalysisTestDb::default();
    let diags = diagnostics(&mut db, &src);
    for (diag, _) in &diags {
        assert!(
            [
                "type normalization limit exceeded",
                "cycle detected while resolving this type"
            ]
            .contains(&diag.message.as_str()),
            "unexpected diagnostic: {diag:#?}"
        );
    }
    for (case, (components, expected)) in cases.iter().enumerate() {
        let reported: Vec<usize> = functions
            .iter()
            .filter(|(_, c)| *c == case)
            .map(|(line, _)| diags.iter().filter(|(_, l)| l == line).count())
            .collect();
        assert!(
            reported
                .iter()
                .all(|&count| count == usize::from(*expected)),
            "case {case} {components:?}: expected {} report(s) in every order, got {reported:?}",
            usize::from(*expected)
        );
    }
}

/// The depth limit applies to the result of each use of an associated type,
/// wherever the use is written, and a cached result counts as deep inside
/// another use as one computed there: whichever part of a type is resolved
/// first, the type reaches the depth limit or not.
#[test]
fn the_depth_limit_applies_to_each_use_wherever_it_is_placed() {
    let src = format!(
        "struct W<T> {{ t: T }}\nstruct Z {{}}\nstruct S<T> {{ t: T }}\n\
         trait Deep {{ type Out }}\nimpl Deep for Z {{ type Out = u8 }}\n\
         impl<T: Deep> Deep for S<T> {{ type Out = {}T::Out{} }}\n\
         fn parts(_ w: own W<u8>, _ s: own S<Z>, _ p: own <Z as Deep>::Out, _ t: own (u8, u8)) {{}}\n",
        "W<".repeat(100),
        ">".repeat(100)
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(Utf8PathBuf::from("cached_depth.fe"), &src);
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "parts");
    let arg = |idx: usize| func.arg_tys(&db)[idx].instantiate_identity();
    // The types are built here rather than written: lowering a type written
    // 600 levels deep is slow for reasons that have nothing to do with this.
    let w = arg(0).decompose_ty_app(&db).0;
    let s = arg(1).decompose_ty_app(&db).0;
    let z = arg(1).decompose_ty_app(&db).1[0];
    let TyData::AssocTy(projection) = arg(2).data(&db) else {
        panic!("expected a projection");
    };
    let (deep_trait, out) = (projection.trait_.def(&db), projection.name);
    // About `100 * steps` levels deep once resolved, under `levels` wrappers.
    let mut wraps = Vec::new();
    for (steps, levels) in [(6, 0), (6, 600), (11, 0)] {
        let self_ty = (0..steps).fold(z, |ty, _| TyId::app(&db, s, ty));
        let inst = TraitInstId::new_simple(&db, deep_trait, vec![self_ty]);
        let deep = TyId::assoc_ty(&db, inst.trait_ref(&db), out);
        wraps.push((0..levels).fold(deep, |ty, _| TyId::app(&db, w, ty)));
    }
    let tuple = arg(3).decompose_ty_app(&db).0;
    let pair = |first, second| TyId::app(&db, TyId::app(&db, tuple, first), second);
    // About 600 levels under 600 written ones: each use is within the limit,
    // in both orders. About 1,100 levels in one use, which reuses the
    // 600-level result inside its own: past the limit, in both orders.
    let cases = [
        ("cached, then wrapped", pair(wraps[0], wraps[1]), false),
        ("wrapped, then cached", pair(wraps[1], wraps[0]), false),
        ("part first", pair(wraps[0], wraps[2]), true),
        ("part last", pair(wraps[2], wraps[0]), true),
    ];
    for (name, ty, too_deep) in cases {
        let result = normalize_ty(&db, ty, func.scope(), PredicateListId::empty_list(&db));
        if too_deep {
            assert_eq!(result, Err(NormalizationLimit::Depth), "{name}");
        } else {
            assert!(result.is_ok(), "{name}: {result:?}");
        }
    }
}

/// An impl whose where clause names the associated type it defines. Its
/// uses are listed in every order: normalization and trait solving call each
/// other here, and the outcome must not depend on which is asked first.
fn where_clause_naming_its_own_type(definition: &str, order: &[String]) -> String {
    format!(
        "trait Show {{}}\n\
         impl Show for u8 {{}}\n\
         trait Tr {{\n    type Out\n}}\n\
         pub struct S {{}}\n\
         impl Tr for S\nwhere\n    <S as Tr>::Out: Show,\n{{\n    type Out = {definition}\n}}\n\
         fn need<X: Tr>() {{}}\n\
         {}",
        order.concat()
    )
}

fn uses_of_own_type(result: &str) -> Vec<String> {
    vec![
        "pub fn goal() {\n    need<S>()\n}\n".to_string(),
        "pub fn body() {\n    let _x: Option<<S as Tr>::Out> = Option::None\n}\n".to_string(),
        format!("pub fn signature(_ x: <S as Tr>::Out) -> {result} {{\n    x\n}}\n"),
        "pub fn unrelated() {}\n".to_string(),
    ]
}

/// The error codes of `src`'s diagnostics, sorted.
fn codes(db: &mut HirAnalysisTestDb, src: &str) -> Vec<String> {
    let mut codes: Vec<String> = diagnostics(db, src)
        .into_iter()
        .map(|(diag, _)| diag.error_code.to_string())
        .collect();
    codes.sort();
    codes
}

#[test]
fn a_where_clause_naming_its_own_type_holds_in_every_order() {
    // The definition satisfies the where clause: accepted, as the
    // assumption-free reading of `S: Tr` gives, in every order.
    for order in permutations(&uses_of_own_type("u8")) {
        let mut db = HirAnalysisTestDb::default();
        let src = where_clause_naming_its_own_type("u8", &order);
        assert_eq!(codes(&mut db, &src), Vec::<String>::new(), "{src}");
    }
}

#[test]
fn a_where_clause_naming_its_own_type_fails_alike_in_every_order() {
    // The definition does not satisfy the where clause: the same errors in
    // every order.
    let mut expected = None;
    for order in permutations(&uses_of_own_type("u16")) {
        let mut db = HirAnalysisTestDb::default();
        let src = where_clause_naming_its_own_type("u16", &order);
        let found = codes(&mut db, &src);
        assert!(!found.is_empty(), "{src}");
        let expected = expected.get_or_insert_with(|| found.clone());
        assert_eq!(&found, expected, "{src}");
    }
}

/// An impl with other trait arguments cannot contribute constraints to this
/// projection, even when its self type matches. Check both declaration orders.
#[test]
fn unrelated_trait_arguments_cannot_limit_a_projection() {
    let prefix = "trait Nest { type Out }\nstruct N<T> { t: T }\n\
        impl<T: Nest> Nest for N<T> { type Out = T::Out }\n\
        impl Nest for u8 { type Out = u8 }\n\
        trait Show {}\nimpl Show for u8 {}\n\
        struct W<T> { t: T }\ntrait Tr<X> { type Out }\n";
    let unrelated = "impl<T: Nest> Tr<u8> for W<T> where T::Out: Show { type Out = u8 }\n";
    let selected = "impl<T> Tr<bool> for W<T> { type Out = bool }\n";
    for order in [
        format!("{unrelated}{selected}"),
        format!("{selected}{unrelated}"),
        selected.to_owned(),
    ] {
        for depth in [3, 70] {
            let arg = format!("{}u8{}", "N<".repeat(depth), ">".repeat(depth));
            let source = format!(
                "{prefix}{order}fn roundtrip(x: <W<{arg}> as Tr<bool>>::Out) -> bool {{ x }}\n"
            );
            let mut db = HirAnalysisTestDb::default();
            let diags = diagnostics(&mut db, &source);
            assert!(diags.is_empty(), "{source}\n{diags:#?}");
        }
    }
}

#[test]
fn complete_headers_bind_impl_parameters_from_trait_arguments() {
    let source = "trait Show {}\nimpl Show for u8 {}\nstruct S {}\n\
        trait Tr<X> { type Out }\n\
        impl<T: Show> Tr<T> for S { type Out = T }\n\
        fn roundtrip(x: <S as Tr<u8>>::Out) -> u8 { x }\n";
    let mut db = HirAnalysisTestDb::default();
    assert!(diagnostics(&mut db, source).is_empty());
}

/// Filtering incompatible headers must not hide limits when two complete
/// headers really can apply. Query directly: overlap diagnostics are separate.
#[test]
fn matching_header_constraints_still_report_limits() {
    let source = format!(
        "trait Nest {{ type Out }}\nstruct N<T> {{ t: T }}\n\
         impl<T: Nest> Nest for N<T> {{ type Out = T::Out }}\n\
         impl Nest for u8 {{ type Out = u8 }}\n\
         trait Show {{}}\nimpl Show for u8 {{}}\n\
         struct W<T> {{ t: T }}\ntrait Tr<X> {{ type Out }}\n\
         impl<T: Nest> Tr<bool> for W<T> where T::Out: Show {{ type Out = u8 }}\n\
         impl<T> Tr<bool> for W<T> {{ type Out = bool }}\n\
         fn projected(x: <W<{}u8{}> as Tr<bool>>::Out) {{}}\n",
        "N<".repeat(70),
        ">".repeat(70)
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("matching_headers.fe".into(), &source);
    let (top, _) = db.top_mod(file);
    let function = find_func(&db, top, "projected");
    let ty = function.arg_tys(&db)[0].instantiate_identity();
    assert_eq!(
        normalize_ty(&db, ty, function.scope(), PredicateListId::empty_list(&db)),
        Err(NormalizationLimit::Nesting)
    );
}

#[test]
fn a_complete_header_cannot_assign_the_callers_inference_variable() {
    use fe_hir::analysis::ty::{
        ty_def::{Kind, TyVarSort},
        unify::{InferenceKey, UnificationTableBase},
    };
    let source = "trait Tr<X> { type Out }\nstruct S {}\n\
        impl Tr<u8> for S { type Out = bool }\n\
        fn projected(x: own S) {}\n";
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("header_inference.fe".into(), source);
    let (top, _) = db.top_mod(file);
    db.assert_no_diags(top);
    let function = find_func(&db, top, "projected");
    let self_ty = function.arg_tys(&db)[0].instantiate_identity();
    assert!(self_ty.as_view(&db).is_none());
    assert!(!self_ty.has_invalid(&db));
    let trait_ = top.all_traits(&db)[0];
    let mut table = UnificationTableBase::<ena::unify::InPlace<InferenceKey<'_>>>::new(&db);
    let variable = table.new_var(TyVarSort::General, &Kind::Star);
    let inst = TraitInstId::new_simple(&db, trait_, vec![self_ty, variable]);
    let concrete_inst = TraitInstId::new_simple(&db, trait_, vec![self_ty, TyId::u8(&db)]);
    let concrete_projection = TyId::assoc_ty(
        &db,
        concrete_inst.trait_ref(&db),
        fe_hir::hir_def::IdentId::new(&db, "Out"),
    );
    assert_eq!(
        normalize_ty(
            &db,
            concrete_projection,
            function.scope(),
            PredicateListId::empty_list(&db)
        ),
        Ok(TyId::bool(&db))
    );
    let projection = TyId::assoc_ty(
        &db,
        inst.trait_ref(&db),
        fe_hir::hir_def::IdentId::new(&db, "Out"),
    );
    assert_eq!(
        normalize_ty(
            &db,
            projection,
            function.scope(),
            PredicateListId::empty_list(&db)
        ),
        Ok(projection)
    );
}

#[test]
fn complete_header_filtering_keeps_the_type_ingots_impls() {
    use common::{
        InputDb,
        stdlib::{HasBuiltinCore, HasBuiltinStd},
    };
    use url::Url;
    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    let mut touch = |path: &str, content: &str| {
        db.workspace()
            .touch(&mut db, Url::parse(path).unwrap(), Some(content.to_owned()))
    };
    touch(
        "file:///normalization-cross-ingot/traits/fe.toml",
        "[ingot]\nname = \"traits\"\nversion = \"0.1.0\"\n",
    );
    touch(
        "file:///normalization-cross-ingot/traits/src/lib.fe",
        "pub trait Tr<X> { type Out }\n",
    );
    touch(
        "file:///normalization-cross-ingot/types/fe.toml",
        "[ingot]\nname = \"types\"\nversion = \"0.1.0\"\n[dependencies]\ntraits = { path = \"../traits\" }\n",
    );
    let types = touch(
        "file:///normalization-cross-ingot/types/src/lib.fe",
        "use traits::Tr\npub struct S {}\nimpl Tr<u8> for S { type Out = u8 }\nimpl Tr<bool> for S { type Out = bool }\n",
    );
    touch(
        "file:///normalization-cross-ingot/app/fe.toml",
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n[dependencies]\ntraits = { path = \"../traits\" }\ntypes = { path = \"../types\" }\n",
    );
    let app = touch(
        "file:///normalization-cross-ingot/app/src/lib.fe",
        "use traits::Tr\nuse types::S\nfn byte(x: <S as Tr<u8>>::Out) -> u8 { x }\nfn boolean(x: <S as Tr<bool>>::Out) -> bool { x }\n",
    );
    let (types, _) = db.top_mod(types);
    let (app, _) = db.top_mod(app);
    db.assert_no_diags(types);
    db.assert_no_diags(app);
}

/// The impl parameter is fixed by a trait argument, not the self type. Its
/// constraint must see that binding before it selects among matching headers.
#[test]
fn matching_headers_prove_constraints_with_bound_trait_arguments() {
    let constrained = "impl<T: Show> Tr<T> for S { type Out = bool }\n";
    let unconditional = "impl<T> Tr<T> for S { type Out = u8 }\n";
    for order in [
        format!("{constrained}{unconditional}"),
        format!("{unconditional}{constrained}"),
    ] {
        let source = format!(
            "trait Show {{}}\nimpl Show for u8 {{}}\nstruct S {{}}\n\
             trait Tr<X> {{ type Out }}\n{order}\
             fn projected(x: <S as Tr<bool>>::Out, expected: u8) {{}}\n"
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("bound_trait_arguments.fe".into(), &source);
        let (top, _) = db.top_mod(file);
        let function = find_func(&db, top, "projected");
        let projection = function.arg_tys(&db)[0].instantiate_identity();
        let expected = function.arg_tys(&db)[1].instantiate_identity();
        assert_eq!(
            normalize_ty(
                &db,
                projection,
                function.scope(),
                PredicateListId::empty_list(&db)
            ),
            Ok(expected),
            "{source}"
        );
    }
}

/// Run the complete diagnostic pipeline: an isolated query can stop at an
/// earlier incomplete trait proof and miss the offending fallback entirely.
#[test]
fn an_unmatched_trait_header_cannot_contribute_a_normalization_limit() {
    let deep = include_str!(
        "../../../uitest/fixtures/ty_check/normalization_no_matching_trait_arguments.fe"
    );
    let nested = format!("{}u8{}", "N<".repeat(70), ">".repeat(70));
    let shallow = deep.replace(&nested, "N<N<N<u8>>>");
    let no_unrelated = deep
        .lines()
        .filter(|line| !line.starts_with("impl<T: Nest> Tr<u8>"))
        .collect::<Vec<_>>()
        .join("\n");
    for source in [&shallow, &no_unrelated, deep] {
        let mut db = HirAnalysisTestDb::default();
        let diags = diagnostics(&mut db, source);
        assert!(
            diags
                .iter()
                .any(|(diag, _)| diag.message == "trait bound is not satisfied"),
            "missing ordinary no-impl diagnostic: {diags:#?}"
        );
        assert!(
            !diags
                .iter()
                .any(|(diag, _)| diag.message == "type normalization limit exceeded"),
            "unrelated impl contributed a limit: {diags:#?}"
        );
    }
}

#[test]
fn targeted_fallback_preserves_contextual_and_supertrait_bindings() {
    let source = "trait Base<X> { type Out }\n\
        trait Sub: Base<u8, Out = bool> {\n\
            fn identity(x: <Self as Base<u8>>::Out) -> bool { x }\n\
        }\n\
        fn contextual<T: Sub>(x: <T as Base<u8>>::Out) -> bool { x }\n";
    let mut db = HirAnalysisTestDb::default();
    let diags = diagnostics(&mut db, source);
    assert!(diags.is_empty(), "{diags:#?}");
}

#[test]
fn targeted_fallback_ignores_other_traits_but_checks_matching_requirements() {
    let original = include_str!(
        "../../../uitest/fixtures/ty_check/normalization_no_matching_trait_arguments.fe"
    );
    let other_trait = original.replace("impl<T: Nest> Tr<u8>", "impl<T: Nest> Other");
    let other_trait = format!("trait Other {{ type Out }}\n{other_trait}");
    let false_matching = original.replace(
        "impl<T: Nest> Tr<u8> for W<T> where T::Out: Show { type Out = u8 }",
        "impl<T: Missing> Tr<bool> for W<T> { type Out = bool }",
    );
    let false_matching = format!("trait Missing {{}}\n{false_matching}");
    for source in [other_trait, false_matching] {
        let mut db = HirAnalysisTestDb::default();
        let diags = diagnostics(&mut db, &source);
        assert!(
            diags
                .iter()
                .any(|(diag, _)| diag.message == "trait bound is not satisfied"),
            "{diags:#?}"
        );
        assert!(
            !diags
                .iter()
                .any(|(diag, _)| diag.message == "type normalization limit exceeded"),
            "{diags:#?}"
        );
    }
}

/// Inside one associated type's result the first limit met is named, so the
/// name may differ between orders; acceptance may not.
#[test]
fn inner_component_limits_preserve_acceptance_in_both_orders() {
    let tree = format!("{}u8{}", "D<".repeat(14), ">".repeat(14));
    let nested = format!("{}u8{}", "N<".repeat(70), ">".repeat(70));
    let mut outcomes = Vec::new();
    for result in ["(A::Out, B::Out)", "(B::Out, A::Out)"] {
        let source = format!(
            "{PRELUDE}\ntrait Tree {{ type Out }}\nstruct D<T> {{ t: T }}\n\
             impl<T: Tree> Tree for D<T> {{ type Out = (T::Out, T::Out) }}\n\
             impl Tree for u8 {{ type Out = u8 }}\n\
             trait Pack<A, B> {{ type Out }}\n\
             impl<A: Tree, B: Nest> Pack<A, B> for K {{ type Out = {result} }}\n\
             fn projected(x: <K as Pack<{tree}, {nested}>>::Out) {{}}\n"
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("inner_limit_priority.fe".into(), &source);
        let (top, _) = db.top_mod(file);
        let function = find_func(&db, top, "projected");
        let ty = function.arg_tys(&db)[0].instantiate_identity();
        outcomes.push(
            normalize_ty(&db, ty, function.scope(), PredicateListId::empty_list(&db)).map(|_| ()),
        );
    }
    assert_eq!(outcomes[0].is_ok(), outcomes[1].is_ok());
    assert!(outcomes.iter().all(|outcome| matches!(
        outcome,
        Err(NormalizationLimit::Work | NormalizationLimit::Nesting)
    )));
}

#[test]
fn inner_cycle_and_nesting_limits_reject_in_both_orders() {
    let nested = format!("{}u8{}", "N<".repeat(70), ">".repeat(70));
    let mut outcomes = Vec::new();
    for result in ["(A::Out, B::Out)", "(B::Out, A::Out)"] {
        let source = format!(
            "{PRELUDE}\ntrait Cyc {{ type Out\n type Mid }}\nstruct S {{}}\n\
             impl Cyc for S {{ type Out = <S as Cyc>::Mid\n type Mid = <S as Cyc>::Out }}\n\
             trait Pack<A, B> {{ type Out }}\n\
             impl<A: Cyc, B: Nest> Pack<A, B> for K {{ type Out = {result} }}\n\
             fn projected(x: <K as Pack<S, {nested}>>::Out) {{}}\n"
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("inner_cycle_priority.fe".into(), &source);
        let (top, _) = db.top_mod(file);
        let function = find_func(&db, top, "projected");
        let ty = function.arg_tys(&db)[0].instantiate_identity();
        outcomes.push(
            normalize_ty(&db, ty, function.scope(), PredicateListId::empty_list(&db)).map(|_| ()),
        );
    }
    assert_eq!(outcomes[0].is_ok(), outcomes[1].is_ok());
    assert!(outcomes.iter().all(|outcome| matches!(
        outcome,
        Err(NormalizationLimit::Cycle | NormalizationLimit::Nesting)
    )));
}

/// A lookup of an associated item considers only the impls whose trait
/// declares it. An unrelated impl for the same type cannot make the lookup hit
/// a limit while proving its where clauses.
#[test]
fn unrelated_impls_cannot_limit_an_associated_item_lookup() {
    let prefix = "trait Nest { type Out }\nstruct N<T> { t: T }\n\
        impl<T: Nest> Nest for N<T> { type Out = T::Out }\n\
        impl Nest for u8 { type Out = u8 }\n\
        trait Show {}\nimpl Show for u8 {}\n\
        struct W<T> { t: T }\n\
        trait HasValue { const VALUE: u8\n type Item }\n\
        impl<T> HasValue for W<T> { const VALUE: u8 = 1\n type Item = u8 }\n\
        trait Other { const OTHER: u8\n type Thing }\n\
        impl<T: Nest> Other for W<T> where T::Out: Show { const OTHER: u8 = 2\n type Thing = u8 }\n";
    for depth in [3, 70] {
        let arg = format!("{}u8{}", "N<".repeat(depth), ">".repeat(depth));
        for body in [
            format!("fn constant() -> u8 {{ W<{arg}>::VALUE }}\n"),
            format!("fn item(x: W<{arg}>::Item) -> u8 {{ x }}\n"),
        ] {
            let source = format!("{prefix}{body}");
            let mut db = HirAnalysisTestDb::default();
            let diags = diagnostics(&mut db, &source);
            assert!(diags.is_empty(), "{source}\n{diags:#?}");
        }
    }
}

#[test]
fn instantiated_unused_effect_keys_report_normalization_limits() {
    // A type key, a trait key's argument and a trait key's binding.
    for (declarations, key, caller_key) in [
        ("", "T::Out", "u8"),
        ("trait Need<X> {}\n", "Need<T::Out>", "Need<u8>"),
        (
            "trait Need { type Item }\n",
            "Need<Item = T::Out>",
            "Need<Item = u8>",
        ),
    ] {
        for depth in [2, 65] {
            let argument = format!("{}u8{}", "N<".repeat(depth), ">".repeat(depth));
            // Only the chain: the prelude's other impls define types that
            // reach limits, which are reported where they are defined.
            let chain_only = PRELUDE.split("trait Tr {").next().unwrap();
            let src = format!(
                "{chain_only}\n{declarations}fn hidden<T: Nest>() uses (x: {key}) {{}}\n\
                 fn run() uses (x: {caller_key}) {{ hidden<{argument}>() }}\n"
            );
            let mut db = HirAnalysisTestDb::default();
            let expected = if depth == 2 { vec![] } else { vec!["3-0058"] };
            assert_eq!(codes(&mut db, &src), expected, "{key}, depth {depth}");
        }
    }
}

#[test]
fn nested_families_report_limits_without_expanding_shared_normal_arguments() {
    for depth in [4, 6, 8, 12] {
        let mut receiver = "Base".to_owned();
        for _ in 0..depth {
            receiver = format!("Wrap<{receiver}>");
        }
        let source = format!(
            "trait Tr {{ type Out<T> }}\n\
             struct Base {{}}\nstruct Wrap<U> {{}}\n\
             impl Tr for Base {{ type Out<T> = T }}\n\
             impl<U: Tr> Tr for Wrap<U> {{ type Out<T> = <U as Tr>::Out<(T, T)> }}\n\
             type W = {receiver}\n\
             fn nested(_ x: <W as Tr>::Out<<W as Tr>::Out<u8>>) {{}}\n"
        );
        let mut db = HirAnalysisTestDb::default();
        let actual = codes(&mut db, &source);
        if depth < 8 {
            assert!(actual.is_empty(), "depth {depth}: {actual:?}");
        } else {
            assert_eq!(actual, ["3-0058"], "depth {depth}");
        }
    }
}
