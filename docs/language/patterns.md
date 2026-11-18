# Patterns with alternatives

A pattern `p | q` matches what either alternative matches, trying `p` first.
The alternatives may bind names. Each binds the same names, each name with
one type and bound the same way (by value, `var`, `ref` or `mut`), and what
follows sees one binding per name: the part the matching alternative bound.

```fe
match change.kind {
    ChangeKind::Account { existed, .. } | ChangeKind::Sender { existed, .. } => !existed,
    ChangeKind::Storage { .. } => false,
}

if let Side::L(x) | Side::R(x) = side { total += x }
let Side::L(x) | Side::R(x) = side
for Side::L(x) | Side::R(x) in sides { total += x }
```

`|` nests, as in `Some(Side::L(x) | Side::R(x))`, and alternatives may
overlap: the first that matches binds, so `(x, 0) | (0, x)` binds the first
`0` of `(0, 0)`. It is an error for a name to be missing from an alternative,
to have two types, or to be bound two ways, as in `E::A(mut x) | E::B(x)`.

## Accesses

In a `match`, each alternative may bind an access at a part of its own:
the arm runs with the part the matching alternative reached.

```fe
match e {
    E::A(mut x, _) | E::B(mut x) => x += 1,
    E::C => {}
}
```

Anywhere else (`if let`, `while let`, `let`, `let ... else` and `for`), the
alternatives meet before the code that follows, where an access reaches one
place. An access the alternatives bind there must reach the same part in each,
or it is an error; write a `match` instead. Values have no such limit, so `v`
below may come from either side while the access `n` reaches one place:

```fe
if let E::A(mut x, _) | E::B(mut x) = e { x += 1 }  // error: use a `match`

if let Some((Side::L(v) | Side::R(v), mut n)) = h.parts() {
    n += v
}
```
