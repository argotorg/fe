// Calls `library.fe`'s exported functions. With an argument, overflows `add`,
// whose panic aborts the process.
#include <stdint.h>
#include <stdio.h>
int64_t add(int64_t a, int64_t b);
void fill(uint8_t *data, size_t len, uint32_t byte);
int main(int argc, char **argv) {
    (void)argv;
    uint8_t text[4] = {0};
    fill(text, 3, 'z');
    printf("%lld %s\n", (long long)add(40, 2), (char *)text);
    if (argc > 1) add(INT64_MAX, 1);
    return 0;
}
