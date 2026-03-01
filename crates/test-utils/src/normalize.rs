use std::borrow::Cow;
use std::path::Path;

pub fn normalize_newlines(input: &str) -> Cow<'_, str> {
    if !input.contains('\r') {
        return Cow::Borrowed(input);
    }

    let mut normalized = input.replace("\r\n", "\n");
    normalized = normalized.replace('\r', "\n");
    Cow::Owned(normalized)
}

pub fn normalize_path_separators(input: &str) -> String {
    input.replace('\\', "/")
}

pub fn replace_path_token(text: &str, path: &Path, token: &str) -> String {
    fn file_url_prefix(path: &str) -> String {
        format!("file:///{}", path.trim_start_matches('/'))
    }

    let native_path = path.to_string_lossy();
    let normalized_path = normalize_path_separators(native_path.as_ref());
    let file_url = file_url_prefix(&normalized_path);
    let file_url_token = format!("file://{token}");
    let mut output = text.replace(&format!("{file_url}/"), &format!("{file_url_token}/"));
    output = output.replace(&file_url, &file_url_token);

    output = output.replace(native_path.as_ref(), token);
    output = output.replace(&normalized_path, token);

    output
}
