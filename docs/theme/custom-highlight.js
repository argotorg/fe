function highlight_fe(hljs) {
  const KEYWORDS = [
    "and",
    "break",
    "const",
    "continue",
    "contract",
    "ingot",
    "else",
    "emit",
    "enum",
    "event",
    "false",
    "fn",
    "for",
    "idx",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mut",
    "or",
    "not",
    "pragma",
    "pub",
    "return",
    "revert",
    "self",
    "Self",
    "struct",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while"
  ];

  const TYPES = [
    "address",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "i256",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "u256",
    "bool",
    "Context",
    "Option",
    "Result",
    "String",
    "Array",
    "Map"
  ];

  return {
    name: "Fe",
    keywords: {
      keyword: KEYWORDS.join(' '),
      built_in: TYPES.join(' '),
      literal: "false true",
    },
    contains: [
      hljs.QUOTE_STRING_MODE,
      hljs.C_NUMBER_MODE,
      {
        scope: "string",
        begin: '"',
        end: '"',
        contains: [{ begin: "\\\\." }],
      },
      hljs.COMMENT("//", "\n"),
    ],
  }
}

hljs.registerLanguage("fe", highlight_fe);
hljs.initHighlightingOnLoad();
