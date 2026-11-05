// Relinked only into the fallible-allocation regression executable. It refuses
// 4096-byte requests and counts the 1024-byte block the test owns. The test's
// empty allocation must not reach the host at all.
#define _DEFAULT_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

static unsigned allocations;
static unsigned releases;
static unsigned failures;
static unsigned empties;

typedef union {
    max_align_t alignment;
    struct { size_t extent; int watched; } info;
} Header;

void *malloc(size_t size) {
    empties += size <= 1;
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
    header->info.watched = size == 1024;
    allocations += header->info.watched;
    return header + 1;
}

void free(void *pointer) {
    if (pointer == NULL) return;
    Header *header = (Header *)pointer - 1;
    releases += header->info.watched;
    munmap(header, header->info.extent);
}

__attribute__((destructor)) static void check_releases(void) {
    if (allocations != 1 || releases != 1 || failures != 1 || empties != 0) _Exit(101);
}
