# String literals

String literals are enclosed in double quotes. Ordinary text is UTF-8, and
literal newlines are allowed. The following escape sequences are supported:

| Source | Decoded character |
| --- | --- |
| `\"` | Double quote |
| `\\` | Backslash |
| `\n` | Line feed |
| `\r` | Carriage return |
| `\t` | Tab |

Other backslash escapes are rejected. Escape decoding happens before literal
typing and constant evaluation, consistently for EVM and native targets. Lengths
and capacities count decoded UTF-8 bytes: `"é\n"` occupies three bytes, and
`"\\n"` occupies two bytes (a backslash followed by the letter `n`).

The syntax tree retains the original source spelling for diagnostics and tools.
