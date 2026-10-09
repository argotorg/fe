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

`String<N>::as_bytes()` returns an owned `PackedBytes<N>` with all `N` bytes,
including leading zero padding. Bytes are indexed from the most significant
byte; bits above the string's capacity are ignored. `len()` counts bytes after
leading zero padding, including any interior zero bytes.

`AsBytes` also supports `u256`, ordinary byte arrays, packed bytes and tuples
whose components implement the trait. Tuples concatenate component encodings in
order into one checked destination. Custom implementations declare `N` and
implement `append_to<const M: usize>(self, _ out: mut ByteWriter<M>)`; the writer
checks that each component appends exactly its declared width. The provided
`as_bytes()` method builds the owned result in constant evaluation and at runtime.

Packed bytes store numeric words with a zero-filled tail. They support indexing,
equality and explicit ordinary-array conversion with `PackedBytes::from_array`
and `.to_array()`. `String<N>::from_bytes()` accepts `PackedBytes<N>`; for an
ordinary array, use `String<N>::from_bytes(PackedBytes::from_array(array))`.
Packed bytes expose no mutable byte references. `core::keccak(value)` hashes the
exact fixed-width encoding of an `AsBytes` value at compile time or on EVM.
