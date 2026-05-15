# Known Issues in optimization-consolidated Branch

## 1. Method generics on default-type-param structs

When a struct has a default type parameter and a method introduces its own
generic with a ByteInput bound, the compiler incorrectly unifies the method
generic with the struct's defaulted type:

```fe
pub struct ArrayView<T, const N: usize, I = ()> { ... }

impl<T, const N: usize> ArrayView<T, N> {
    // This method's I is independent of the struct's I
    pub fn over<I: ByteInput>(self, _ input: I) -> ArrayView<T, N, I> { ... }
}

// This fails: "() doesn't implement ByteInput"
let bound = unbound.over(CallData::new())
```

Free functions with the same signature work correctly:
```fe
fn bind_view<T, const N: usize, Source: ByteInput>(
    _ view: ArrayView<T, N>, _ input: Source
) -> ArrayView<T, N, Source> { ... }

// This works
let bound = bind_view(unbound, CallData::new())
```

Affects: ArrayView.over(), BytesView.over(), StringView.over()

Workaround: construct the bound view directly:
```fe
let view: ArrayView<u256, 32, CallData> = ArrayView {
    input: CallData::new(),
    start: proof.start,
}
```

Root cause: method selection in HIR type checker unifies the method-level
generic with the struct's defaulted type parameter instead of treating them
as independent. Likely in crates/hir/src/analysis/ty/ (trait resolution or
method selection).
