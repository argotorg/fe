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

## Literal types

A literal takes the type its context expects:

- `String<N>`, a fixed-capacity string packed into one word, for `N` up to 31.
  A literal fits when it has at most `N` bytes; shorter text is padded.
- `DynString` (`std::abi::DynString`, also `std::abi::Text`), a string of any
  length in memory.
- `[u8; N]`, the literal's bytes, when the context expects a byte array.

When the context leaves the type open, a literal of up to 31 bytes is a
`String<N>` of its own length and a longer one is a `DynString`. So is a
literal that begins with a zero byte, since `String<N>` drops leading zero bytes
from its length; a context that needs a `String<N>` still gives it one.
Literals that must have one type, such as the branches of an `if`, take the type
of the longest.

An operator's operand takes the type the operator's impl needs, on either side:
in `"x" == s`, the literal is a `String<N>` when `s` is one and a `DynString`
when `s` is one. When impls for both types fit, the default above decides.

A method call on a literal follows the same rule. A trait method is that
trait's method on the literal, as `Width::width("ab")` is for the trait below:
its arguments, effects and result come from the trait, and the context, or
else the default, decides the literal. When several traits give the method
name, the argument types and the literal's type choose among them once the
whole body is checked, as for a receiver of a known type. An inherent method
only one string type has makes the literal that type. `len` and `byte_at`,
which both types have, run on the type the literal takes: the default when
nothing else decides it, so `"ab".len()` uses `String<2>`. A result whose type
depends on the literal's type, such as an associated type that differs between
the two, cannot be cast, called, indexed, matched, iterated, negated or used in
an operator or a field access until the literal's type is decided; annotate the
literal. Whether the function a literal is passed to is `const` does not change
its type.

```fe
trait Width {
    fn width(self) -> usize
}
impl<const N: usize> Width for String<N> {
    fn width(self) -> usize { N }
}
impl Width for DynString {
    fn width(self) -> usize { 1000 }
}

let w = "ab".width()                  // 2: the default, String<2>
let d: DynString = "ab"
let v = d.width()                     // 1000
```

```fe
let short = "hello"                 // String<5>
let long = "a literal longer than 31 bytes of text"   // DynString
let bytes: [u8; 5] = "hello"
```

On the EVM, a `DynString` literal of 96 bytes or more is copied into memory
from the contract code where it is evaluated; a shorter one is stored a word at
a time.

A `DynString` owns its bytes. It moves rather than copies, `clone()` makes a second
string, and dropping it frees the memory on native targets. A parameter without a
mode views the caller's string, so passing a string to one does not move it.
`into_bytes()` turns a string into the `Bytes` that hold the same allocation.

## Bytes

`String<N>::as_bytes()` returns an owned `PackedBytes<N>` with all `N` bytes,
including leading zero padding. Bytes are indexed from the most significant
byte; bits above the string's capacity are ignored. `len()` counts bytes after
leading zero padding, including any interior zero bytes.

A byte span (`MemSpan`) equals a `String<N>` whose `len()` bytes it holds and a
`DynString` whose bytes it holds, so `arg.span() == "serve"` compares bytes with
a literal of any length. Strings compare with spans too, so the literal may
stand on the left: `"serve" == arg.span()`. A `MemBuffer` and a process argument
(`std::env::Arg`) compare with strings the same way: `args.get(1) == "serve"`.
Spans (`MemSlice<T>`) compare with each other element by element.

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

## Byte literals

A byte literal, `b'x'`, is a `u8`: the byte of one ASCII character, or of an
escape. It accepts the string escapes and `\'`, `\0` for a zero byte and `\xNN`
for any byte, such as `b'\xff'`. In a pattern, a byte literal matches its value.

```fe
fn digit(_ c: u8) -> u8 {
    c - b'0'
}
```
