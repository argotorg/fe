// Implements `host_calls.fe`'s `host_apply` with its exported `twice`.
#include <stdint.h>
int64_t twice(int64_t x);
int64_t host_apply(int64_t x) { return twice(x); }
