use std::collections::HashSet;

use regex::Regex;

fn group(choices: &[&str]) -> String {
    ["(", &choices.join("|"), ")"].concat()
}

fn maybe(choices: &[&str]) -> String {
    [&group(choices), "?"].concat()
}

pub const WHITESPACE: &str = r"[ \f\t]*";
pub const COMMENT: &str = r"#[^\r\n]*";

pub const NAME: &str = r"\w+";

pub const HEXNUMBER: &str = r"0[xX](?:_?[0-9a-fA-F])+";
pub const BINNUMBER: &str = r"0[bB](?:_?[01])+";
pub const OCTNUMBER: &str = r"0[oO](?:_?[0-7])+";
pub const DECNUMBER: &str = r"[0-9](?:_?[0-9])*";

/// INTNUMBER = group(Hexnumber, Binnumber, Octnumber, Decnumber)
pub fn get_intnumber_pattern() -> String {
    group(&[HEXNUMBER, BINNUMBER, OCTNUMBER, DECNUMBER])
}

pub const EXPONENT: &str = r"[eE][-+]?[0-9](?:_?[0-9])*";

/// POINTFLOAT = group(
///     r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
///     r"\.[0-9](?:_?[0-9])*",
/// ) + maybe(EXPONENT)
pub fn get_pointfloat_pattern() -> String {
    [
        group(&[
            r"[0-9](?:_?[0-9])*\.(?:[0-9](?:_?[0-9])*)?",
            r"\.[0-9](?:_?[0-9])*",
        ]),
        maybe(&[EXPONENT]),
    ]
    .concat()
}

/// EXPFLOAT = r"[0-9](?:_?[0-9])*" + EXPONENT
pub fn get_expfloat_pattern() -> String {
    [r"[0-9](?:_?[0-9])*", EXPONENT].concat()
}

/// FLOATNUMBER = group(POINTFLOAT, EXPFLOAT)
pub fn get_floatnumber_pattern() -> String {
    group(&[&get_pointfloat_pattern(), &get_expfloat_pattern()])
}

/// IMAGNUMBER = group(r"[0-9](?:_?[0-9])*[jJ]", FLOATNUMBER + r"[jJ]")
pub fn get_imagnumber_pattern() -> String {
    group(&[
        r"[0-9](?:_?[0-9])*[jJ]",
        &[&get_floatnumber_pattern(), r"[jJ]"].concat(),
    ])
}

/// NUMBER = group(IMAGNUMBER, FLOATNUMBER, INTNUMBER)
pub fn get_number_pattern() -> String {
    group(&[
        &get_imagnumber_pattern(),
        &get_floatnumber_pattern(),
        &get_intnumber_pattern(),
    ])
}

pub const VALID_STRING_PREFIXES: &[&str] = &[
    "b", "B", "r", "R", "u", "U", "f", "F", "br", "BR", "bR", "Br", "fr", "FR", "fR", "Fr", "rb",
    "RB", "Rb", "rB", "rf", "RF", "Rf", "rF",
];

/// Get a hash set containing all possible initial single-quoted string
/// delimiters.
pub fn get_single_quote_set() -> HashSet<String> {
    let mut set = HashSet::new();

    set.insert("\"".to_string());
    set.insert("'".to_string());
    for prefix in VALID_STRING_PREFIXES {
        set.insert([*prefix, "\""].concat());
        set.insert([*prefix, "'"].concat());
    }

    set
}

/// Get a hash set containing all possible initial triple-quoted string
/// delimiters.
pub fn get_triple_quote_set() -> HashSet<String> {
    let mut set = HashSet::new();

    set.insert("\"\"\"".to_string());
    set.insert("'''".to_string());
    for prefix in VALID_STRING_PREFIXES {
        set.insert([*prefix, "\"\"\""].concat());
        set.insert([*prefix, "'''"].concat());
    }

    set
}

/// STRINGPREFIX = group(VALID_STRING_PREFIXES)
pub fn get_stringprefix_pattern() -> String {
    [&group(VALID_STRING_PREFIXES), "?"].concat()
}

// Tail end of ' string
pub const SINGLE: &str = "'";
// Tail end of " string
pub const DOUBLE: &str = "\"";
// Tail end of ''' string
pub const SINGLE3: &str = "'''";
// Tail end of """ string
pub const DOUBLE3: &str = "\"\"\"";

/// TRIPLE = group(STRINGPREFIX + "'''", STRINGPREFIX + '"""')
pub fn get_triple_pattern() -> String {
    let stringprefix = &get_stringprefix_pattern();
    group(&[
        &[stringprefix, "'''"].concat(),
        &[stringprefix, "\"\"\""].concat(),
    ])
}

/// Because of leftmost-then-longest match semantics, be sure to put the longest
/// operators first (e.g., if = came before ==, == would get recognized as two
/// instances of =).
///
/// OPERATOR = group(r"\*\*=?", r">>=?", r"<<=?", r"!=",
///                  r"//=?", r"->",
///                  r"[+\-*/%&@|^=<>]=?",
///                  r"~")
pub fn get_operator_pattern() -> String {
    group(&[
        r"\*\*=?",
        r">>=?",
        r"<<=?",
        r"!=",
        r"//=?",
        r"->",
        r"[+\-*/%&@|^=<>]=?",
        r"~",
    ])
}

pub const BRACKET: &str = r"[\[\](){}]";

/// SPECIAL = group(r'\r?\n', r'\.\.\.', r'[:;.,@]')
pub fn get_special_pattern() -> String {
    group(&[r"\r?\n", r"\.\.\.", r"[:;.,@]"])
}

/// FUNNY = group(OPERATOR, BRACKET, SPECIAL)
pub fn get_funny_pattern() -> String {
    group(&[&get_operator_pattern(), BRACKET, &get_special_pattern()])
}

/// First (or only) line of ' or " string.
/// ContStr = group(StringPrefix + r"'[^\n'\\]*(?:\\.[^\n'\\]*)*" +
///                 group("'", r'\\\r?\n'),
///                 StringPrefix + r'"[^\n"\\]*(?:\\.[^\n"\\]*)*' +
///                 group('"', r'\\\r?\n'))
pub fn get_contstr_pattern() -> String {
    let stringprefix_pattern = &get_stringprefix_pattern();
    group(&[
        &[
            stringprefix_pattern,
            r###"'[^\n'\\]*(?:\\.[^\n'\\]*)*"###,
            &group(&["'", r"\\\r?\n"]),
        ]
        .concat(),
        &[
            stringprefix_pattern,
            r###""[^\n"\\]*(?:\\.[^\n"\\]*)*"###,
            &group(&["\"", r"\\\r?\n"]),
        ]
        .concat(),
    ])
}
/// PSEUDOEXTRAS = group(r'\\\r?\n|\z', COMMENT, TRIPLE)
pub fn get_pseudoextras_pattern() -> String {
    group(&[r"\\\r?\n|\z", COMMENT, &get_triple_pattern()])
}

/// PSEUDOTOKEN = WHITESPACE + group(PSEUDOEXTRAS, NUMBER, FUNNY, CONTSTR, NAME)
pub fn get_pseudotoken_pattern() -> String {
    [
        WHITESPACE,
        &group(&[
            &get_pseudoextras_pattern(),
            &get_number_pattern(),
            &get_funny_pattern(),
            &get_contstr_pattern(),
            NAME,
        ]),
    ]
    .concat()
}

/// Compile the given regex with the "beginning of text" anchor prepended.  This
/// forces the regex to match at the *beginning of a string* (as in python's
/// re.match method) instead of anywhere in the string.
pub fn compile_anchored(re: &str) -> Regex {
    Regex::new(&[r"\A", re].concat()).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    use regex::Regex;

    #[test]
    fn test_get_regex_patterns() {
        assert_eq!(
            get_intnumber_pattern(),
            "(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|[0-9](?:_?[0-9])*)",
        );
        assert_eq!(
            get_pointfloat_pattern(),
            "([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?",
        );
        assert_eq!(
            get_expfloat_pattern(),
            "[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*",
        );
        assert_eq!(
            get_floatnumber_pattern(),
            "(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)",
        );
        assert_eq!(
            get_imagnumber_pattern(),
            "([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])",
        );
        assert_eq!(
            get_number_pattern(),
            "(([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|[0-9](?:_?[0-9])*))",
        );
        assert_eq!(
            get_stringprefix_pattern(),
            "(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?",
        );
        assert_eq!(
            get_triple_pattern(),
            "((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\"\"\")",
        );
        assert_eq!(
            get_operator_pattern(),
            "(\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)",
        );
        assert_eq!(get_special_pattern(), "(\\r?\\n|\\.\\.\\.|[:;.,@])",);
        assert_eq!(
            get_funny_pattern(),
            "((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[\\[\\](){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))",
        );
        assert_eq!(
            get_contstr_pattern(),
            "((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*(\'|\\\\\\r?\\n)|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*(\"|\\\\\\r?\\n))",
        );
        assert_eq!(
            get_pseudoextras_pattern(),
            "(\\\\\\r?\\n|\\z|#[^\\r\\n]*|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\"\"\"))",
        );
        assert_eq!(
            get_pseudotoken_pattern(),
            "[ \\f\\t]*((\\\\\\r?\\n|\\z|#[^\\r\\n]*|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\'\'\'|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\"\"\"))|(([0-9](?:_?[0-9])*[jJ]|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)[jJ])|(([0-9](?:_?[0-9])*\\.(?:[0-9](?:_?[0-9])*)?|\\.[0-9](?:_?[0-9])*)([eE][-+]?[0-9](?:_?[0-9])*)?|[0-9](?:_?[0-9])*[eE][-+]?[0-9](?:_?[0-9])*)|(0[xX](?:_?[0-9a-fA-F])+|0[bB](?:_?[01])+|0[oO](?:_?[0-7])+|[0-9](?:_?[0-9])*))|((\\*\\*=?|>>=?|<<=?|!=|//=?|->|[+\\-*/%&@|^=<>]=?|~)|[\\[\\](){}]|(\\r?\\n|\\.\\.\\.|[:;.,@]))|((b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\'[^\\n\'\\\\]*(?:\\\\.[^\\n\'\\\\]*)*(\'|\\\\\\r?\\n)|(b|B|r|R|u|U|f|F|br|BR|bR|Br|fr|FR|fR|Fr|rb|RB|Rb|rB|rf|RF|Rf|rF)?\"[^\\n\"\\\\]*(?:\\\\.[^\\n\"\\\\]*)*(\"|\\\\\\r?\\n))|\\w+)",
        );
    }

    #[test]
    fn test_parse_regex_patterns() {
        let patterns = &[
            ("get_intnumber_pattern", get_intnumber_pattern()),
            ("get_pointfloat_pattern", get_pointfloat_pattern()),
            ("get_expfloat_pattern", get_expfloat_pattern()),
            ("get_floatnumber_pattern", get_floatnumber_pattern()),
            ("get_imagnumber_pattern", get_imagnumber_pattern()),
            ("get_number_pattern", get_number_pattern()),
            ("get_stringprefix_pattern", get_stringprefix_pattern()),
            ("get_triple_pattern", get_triple_pattern()),
            ("get_operator_pattern", get_operator_pattern()),
            ("get_special_pattern", get_special_pattern()),
            ("get_funny_pattern", get_funny_pattern()),
            ("get_contstr_pattern", get_contstr_pattern()),
            ("get_pseudoextras_pattern", get_pseudoextras_pattern()),
            ("get_pseudotoken_pattern", get_pseudotoken_pattern()),
        ];

        for (name, pattern) in patterns {
            Regex::new(&pattern).expect(name);
        }
    }
}
