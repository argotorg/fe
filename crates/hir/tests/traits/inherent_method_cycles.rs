use fe_hir::test_db::HirAnalysisTestDb;

fn check(source: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("inherent_method_cycles.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}

#[test]
fn enum_projection_and_inherent_header_lookup_terminate_in_both_orders() {
    let declarations = "trait Factory { type Out }\nstruct Provider {}\nimpl Factory for Provider { type Out = u256 }\nstruct W1<T> {}\n";
    for (first, second) in [
        (
            "enum Payload { A(Provider::Out) }",
            "impl W1<Provider::Out> {}",
        ),
        (
            "impl W1<Provider::Out> {}",
            "enum Payload { A(Provider::Out) }",
        ),
    ] {
        check(&format!("{declarations}\n{first}\n{second}"));
    }
}

#[test]
fn stabilized_projection_headers_preserve_inherent_method_lookup() {
    let declarations = "trait Factory { type Out }\nstruct Provider {}\nimpl Factory for Provider { type Out = u256 }\nstruct W1<T> {}\n";
    let projected = "impl W1<Provider::Out> { fn value(self) -> u256 { 7 } }";
    let concrete = "impl W1<bool> { fn value(self) -> bool { true } }";
    for (first, second) in [(projected, concrete), (concrete, projected)] {
        check(&format!(
            "{declarations}\nenum Payload {{ A(Provider::Out) }}\n{first}\n{second}\nfn number(w: W1<u256>) -> u256 {{ w.value() }}\nfn boolean(w: W1<bool>) -> bool {{ w.value() }}"
        ));
    }
}

#[test]
fn enum_family_application_and_inherent_header_lookup_terminate_in_both_orders() {
    let declarations = "trait Factory { type Out<T> }\nstruct Provider {}\nimpl Factory for Provider { type Out<T> = T }\nstruct W1<T> {}\n";
    for (first, second) in [
        (
            "enum Payload { A(Provider::Out<u8>) }",
            "impl W1<Provider::Out<u8>> {}",
        ),
        (
            "impl W1<Provider::Out<u8>> {}",
            "enum Payload { A(Provider::Out<u8>) }",
        ),
    ] {
        check(&format!("{declarations}\n{first}\n{second}"));
    }
}
