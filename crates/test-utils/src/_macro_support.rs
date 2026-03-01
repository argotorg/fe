#[doc(hidden)]
pub use insta as _insta;
use std::path::Path;

fn add_literal_filter(settings: &mut _insta::Settings, literal: &str, replacement: &str) {
    if literal.is_empty() {
        return;
    }
    settings.add_filter(&regex::escape(literal), replacement);
}

#[doc(hidden)]
pub fn configure_snapshot_filters(settings: &mut _insta::Settings, project_root: &Path) {
    let project_root_native = project_root.to_string_lossy().to_string();
    let project_root_forward = project_root_native.replace('\\', "/");

    add_literal_filter(settings, "\r\n", "\n");
    add_literal_filter(settings, "\r", "\n");
    add_literal_filter(
        settings,
        &format!("file:///{project_root_forward}/"),
        "file://<project>/",
    );
    add_literal_filter(
        settings,
        &format!("file:///{project_root_forward}"),
        "file://<project>",
    );
    add_literal_filter(settings, &project_root_native, "<project>");
    add_literal_filter(settings, &project_root_forward, "<project>");
    add_literal_filter(settings, "<project>\\", "<project>/");
}

/// A macro to assert that a value matches a snapshot.
/// If the snapshot does not exist, it will be created in the same directory as
/// the test file.
#[macro_export]
macro_rules! snap_test {
    ($value:expr, $fixture_path: expr) => {
        let mut settings = $crate::_macro_support::_insta::Settings::new();
        let normalized_value = $crate::normalize::normalize_newlines($value.as_str()).into_owned();
        let fixture_path = ::std::path::Path::new($fixture_path);
        let fixture_dir = fixture_path.parent().unwrap();
        let fixture_name = fixture_path.file_stem().unwrap().to_str().unwrap();
        let manifest_dir = ::std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let project_root = manifest_dir
            .parent()
            .and_then(::std::path::Path::parent)
            .unwrap_or(manifest_dir);

        settings.set_snapshot_path(fixture_dir);
        settings.set_input_file($fixture_path);
        settings.set_prepend_module_to_snapshot(false);
        $crate::_macro_support::configure_snapshot_filters(&mut settings, project_root);
        settings.bind(|| {
            $crate::_macro_support::_insta::_macro_support::assert_snapshot(
                (
                    $crate::_macro_support::_insta::_macro_support::AutoName,
                    normalized_value.as_str(),
                )
                    .into(),
                std::path::Path::new(env!("CARGO_MANIFEST_DIR")),
                fixture_name,
                module_path!(),
                file!(),
                line!(),
                stringify!($value),
            )
            .unwrap()
        })
    };
}
