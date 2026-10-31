// Calls the functions `exports.fe` exports before its `main` runs.
#include <stdint.h>
#include <stdlib.h>
int64_t twice(int64_t x);
void store(int64_t *p, int64_t value);
__attribute__((constructor)) static void call_exports(void) {
    int64_t stored = 0;
    store(&stored, 1);
    if (twice(21) + stored != 43) abort();
}
