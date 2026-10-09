use fe_hir::analysis::ty::{
    LayoutBundleSchema, LayoutBundleSchemaError,
    LayoutEvidencePathStep::{EffectTarget, Field},
    LayoutViewAlias,
};

#[test]
fn later_self_alias_is_rejected_before_any_target_is_followed() {
    let schema = LayoutBundleSchema {
        view_aliases: vec![
            LayoutViewAlias {
                alias: vec![Field(0), EffectTarget],
                canonical: vec![Field(0)],
            },
            LayoutViewAlias {
                alias: vec![Field(0)],
                canonical: vec![Field(0)],
            },
        ],
        ..Default::default()
    };
    assert_eq!(
        schema.validate(),
        Err(LayoutBundleSchemaError::InvalidViewAlias { alias: 1 })
    );
    assert_eq!(
        schema.canonicalize_view_path(&[Field(0), EffectTarget]),
        Err(LayoutBundleSchemaError::InvalidViewAlias { alias: 1 })
    );
}

#[test]
fn malformed_aliases_fail_their_local_prerequisite() {
    for rule in [
        LayoutViewAlias {
            alias: vec![Field(0)],
            canonical: vec![Field(0), EffectTarget],
        },
        LayoutViewAlias {
            alias: vec![Field(0), EffectTarget],
            canonical: vec![Field(1)],
        },
        LayoutViewAlias {
            alias: vec![],
            canonical: vec![],
        },
    ] {
        let path = rule.alias.clone();
        let schema = LayoutBundleSchema {
            view_aliases: vec![rule],
            ..Default::default()
        };
        let expected = Err(LayoutBundleSchemaError::InvalidViewAlias { alias: 0 });
        assert_eq!(schema.validate(), expected);
        assert_eq!(
            schema.canonicalize_view_path(&path),
            Err(LayoutBundleSchemaError::InvalidViewAlias { alias: 0 })
        );
    }
}

#[test]
fn duplicate_and_overlapping_aliases_are_rejected_before_canonical_checks() {
    let first = LayoutViewAlias {
        alias: vec![Field(0), EffectTarget],
        canonical: vec![Field(0)],
    };
    for (second, expected) in [
        (
            first.clone(),
            LayoutBundleSchemaError::DuplicateViewAlias {
                first: 0,
                second: 1,
            },
        ),
        (
            LayoutViewAlias {
                alias: vec![Field(0), EffectTarget, EffectTarget],
                canonical: vec![Field(0)],
            },
            LayoutBundleSchemaError::OverlappingViewAlias {
                first: 0,
                second: 1,
            },
        ),
    ] {
        let schema = LayoutBundleSchema {
            view_aliases: vec![first.clone(), second],
            ..Default::default()
        };
        assert_eq!(schema.validate(), Err(expected));
    }
}

#[test]
fn decreasing_rewrites_reach_an_idempotent_normal_form() {
    // The prefix-disjoint rules alternate after each rewrite; one pass over
    // the table is insufficient even though both targets are canonical.
    let schema = LayoutBundleSchema {
        view_aliases: vec![
            LayoutViewAlias {
                alias: vec![Field(2), Field(0)],
                canonical: vec![Field(2)],
            },
            LayoutViewAlias {
                alias: vec![Field(2), EffectTarget],
                canonical: vec![Field(2)],
            },
        ],
        ..Default::default()
    };
    assert_eq!(schema.validate(), Ok(()));
    let mut path = vec![Field(2)];
    path.extend([Field(0), EffectTarget].repeat(2048));
    path.push(Field(1));
    let canonical = schema.canonicalize_view_path(&path).unwrap();
    assert_eq!(canonical, [Field(2), Field(1)]);
    assert_eq!(schema.canonicalize_view_path(&canonical), Ok(canonical));
}
