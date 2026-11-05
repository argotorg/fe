// Every allocation must be freed exactly once. Allocations are poisoned when
// made and unmapped when freed, so a use after free faults. Requests of
// `REFUSE_FROM` bytes or more fail.
#define _DEFAULT_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#ifndef REFUSE_FROM
#define REFUSE_FROM SIZE_MAX
#endif
static unsigned allocations, releases;
typedef union {
    max_align_t alignment;
    struct { size_t extent; } info;
} Header;
void *malloc(size_t size) {
    if (size >= REFUSE_FROM || size > SIZE_MAX - sizeof(Header)) return NULL;
    size_t extent = sizeof(Header) + size;
    Header *header = mmap(NULL, extent, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (header == MAP_FAILED) return NULL;
    header->info.extent = extent;
    allocations++;
    memset(header + 1, 0xa5, size);
    return header + 1;
}
void free(void *pointer) {
    if (!pointer) return;
    Header *header = (Header *)pointer - 1;
    releases++;
    munmap(header, header->info.extent);
}
// Unbuffered output keeps stdio from allocating a buffer it never frees.
__attribute__((constructor)) static void unbuffered(void) {
    setvbuf(stdout, NULL, _IONBF, 0);
}
__attribute__((destructor)) static void check_releases(void) {
    fprintf(stderr, "allocations=%u releases=%u\n", allocations, releases);
    if (allocations != releases) _Exit(101);
}
