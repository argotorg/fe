use std::borrow::Cow;
use std::path::Path;

use common::paths::normalize_slashes;

pub fn normalize_newlines(input: &str) -> Cow<'_, str> {
    if !input.contains('\r') {
        return Cow::Borrowed(input);
    }

    let mut normalized = input.replace("\r\n", "\n");
    normalized = normalized.replace('\r', "\n");
    Cow::Owned(normalized)
}

pub fn normalize_path_separators(input: &str) -> String {
    let mut normalized = String::with_capacity(input.len());
    let mut token_start = None;

    for (idx, ch) in input.char_indices() {
        if ch.is_whitespace() {
            if let Some(start) = token_start.take() {
                push_normalized_token(&mut normalized, &input[start..idx]);
            }
            normalized.push(ch);
        } else if token_start.is_none() {
            token_start = Some(idx);
        }
    }

    if let Some(start) = token_start {
        push_normalized_token(&mut normalized, &input[start..]);
    }

    normalized
}

pub fn replace_path_token(text: &str, path: &Path, token: &str) -> String {
    fn file_url_prefix(path: &str) -> String {
        format!("file:///{}", path.trim_start_matches('/'))
    }

    let native_path = path.to_string_lossy();
    let normalized_path = normalize_slashes(native_path.as_ref());
    let file_url = file_url_prefix(&normalized_path);
    let file_url_token = format!("file://{token}");
    let mut output = text.replace(&format!("{file_url}/"), &format!("{file_url_token}/"));
    output = output.replace(&file_url, &file_url_token);

    output = output.replace(native_path.as_ref(), token);
    output = output.replace(&normalized_path, token);

    output
}

fn push_normalized_token(output: &mut String, token: &str) {
    if token.contains('\\') {
        output.push_str(&normalize_slashes(token));
    } else {
        output.push_str(token);
    }
}
