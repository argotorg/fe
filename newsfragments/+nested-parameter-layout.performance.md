Checking a function whose parameter type nests generics deeply, such as `fn f(x: W<W<W<u8>>>)`, no longer takes time exponential in the nesting depth.
