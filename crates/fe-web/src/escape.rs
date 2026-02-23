//! HTML and script-context escaping utilities.

/// Escape for embedding in HTML attribute values and general code content.
///
/// Escapes: `& < > " '`
pub fn escape_html(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#x27;")
}

/// Escape for embedding in HTML element content (e.g., `<title>`).
///
/// Only escapes `& < >` — quotes are safe in element text.
pub fn escape_html_text(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Escape content for safe embedding inside a `<script>` tag.
///
/// The only dangerous sequence is `</` which can close the script element.
/// We replace `</` with `<\/` which is valid in JS string literals and JSON.
pub fn escape_script_content(s: &str) -> String {
    s.replace("</", r"<\/")
}

/// Encode bytes as standard base64 (no external dependency).
pub fn base64_encode(data: &[u8]) -> String {
    const CHARS: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    let mut out = String::with_capacity((data.len() + 2) / 3 * 4);
    let mut i = 0;
    while i + 2 < data.len() {
        let n = ((data[i] as u32) << 16) | ((data[i + 1] as u32) << 8) | (data[i + 2] as u32);
        out.push(CHARS[(n >> 18 & 0x3f) as usize] as char);
        out.push(CHARS[(n >> 12 & 0x3f) as usize] as char);
        out.push(CHARS[(n >> 6 & 0x3f) as usize] as char);
        out.push(CHARS[(n & 0x3f) as usize] as char);
        i += 3;
    }
    match data.len() - i {
        1 => {
            let n = (data[i] as u32) << 16;
            out.push(CHARS[(n >> 18 & 0x3f) as usize] as char);
            out.push(CHARS[(n >> 12 & 0x3f) as usize] as char);
            out.push('=');
            out.push('=');
        }
        2 => {
            let n = ((data[i] as u32) << 16) | ((data[i + 1] as u32) << 8);
            out.push(CHARS[(n >> 18 & 0x3f) as usize] as char);
            out.push(CHARS[(n >> 12 & 0x3f) as usize] as char);
            out.push(CHARS[(n >> 6 & 0x3f) as usize] as char);
            out.push('=');
        }
        _ => {}
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_html_all_chars() {
        assert_eq!(
            escape_html("a<b>c&d\"e'f"),
            "a&lt;b&gt;c&amp;d&quot;e&#x27;f"
        );
    }

    #[test]
    fn escape_html_text_no_quotes() {
        assert_eq!(escape_html_text("a<b>c&d\"e'f"), "a&lt;b&gt;c&amp;d\"e'f");
    }

    #[test]
    fn escape_script_content_closes_tag() {
        assert_eq!(escape_script_content("</script>"), r"<\/script>");
        assert_eq!(escape_script_content("hello world"), "hello world");
    }

    #[test]
    fn base64_encode_standard_vectors() {
        // RFC 4648 test vectors
        assert_eq!(base64_encode(b""), "");
        assert_eq!(base64_encode(b"f"), "Zg==");
        assert_eq!(base64_encode(b"fo"), "Zm8=");
        assert_eq!(base64_encode(b"foo"), "Zm9v");
        assert_eq!(base64_encode(b"foob"), "Zm9vYg==");
        assert_eq!(base64_encode(b"fooba"), "Zm9vYmE=");
        assert_eq!(base64_encode(b"foobar"), "Zm9vYmFy");
    }

    #[test]
    fn base64_encode_binary() {
        assert_eq!(base64_encode(&[0, 1, 2, 255]), "AAEC/w==");
    }
}
