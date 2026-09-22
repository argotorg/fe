// Relinked only into the byte-buffer regression executable. The watched sizes
// distinguish its data allocations from the compiler's reference descriptors.
#define _DEFAULT_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

static unsigned allocations;
static unsigned releases;
static unsigned failures;

typedef union {
    max_align_t alignment;
    struct { size_t extent; int watched; } info;
} Header;

void *malloc(size_t size) {
    if (size == 4096) {
        failures++;
        return NULL;
    }
    if (size > SIZE_MAX - sizeof(Header)) return NULL;
    size_t extent = sizeof(Header) + size;
    Header *header = mmap(NULL, extent, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (header == MAP_FAILED) return NULL;
    header->info.extent = extent;
    header->info.watched = size == 1024 || size == 2048;
    allocations += header->info.watched;
    // Fresh mmap bytes are zero; poison them so the test checks Fe's zeroing.
    memset(header + 1, 0xa5, size);
    return header + 1;
}

void free(void *pointer) {
    if (pointer == NULL) return;
    Header *header = (Header *)pointer - 1;
    releases += header->info.watched;
    munmap(header, header->info.extent);
}

__attribute__((destructor)) static void check_releases(void) {
    if (allocations != 2 || releases != 2 || failures != 2) _Exit(101);
}
