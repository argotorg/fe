// Every allocation must be freed exactly once. Allocations are poisoned when
// made and unmapped when freed, so a use after free faults. A resize always
// moves the block, so a use of the old address faults too. Requests of
// `REFUSE_FROM` bytes or more fail, and a refused resize leaves the block as
// it was.
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
static unsigned allocations, releases, resizes;
typedef union {
    max_align_t alignment;
    struct { size_t extent, size; } info;
} Header;
static void *map_block(size_t size) {
    if (size >= REFUSE_FROM || size > SIZE_MAX - sizeof(Header)) return NULL;
    size_t extent = sizeof(Header) + size;
    Header *header = mmap(NULL, extent, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (header == MAP_FAILED) return NULL;
    header->info.extent = extent;
    header->info.size = size;
    memset(header + 1, 0xa5, size);
    return header + 1;
}
static void unmap_block(void *pointer) {
    Header *header = (Header *)pointer - 1;
    munmap(header, header->info.extent);
}
void *malloc(size_t size) {
    void *pointer = map_block(size);
    allocations += pointer != NULL;
    return pointer;
}
void free(void *pointer) {
    if (!pointer) return;
    releases++;
    unmap_block(pointer);
}
void *realloc(void *pointer, size_t size) {
    if (!pointer) return malloc(size);
    void *resized = map_block(size);
    if (!resized) return NULL;
    size_t old = ((Header *)pointer - 1)->info.size;
    memcpy(resized, pointer, old < size ? old : size);
    unmap_block(pointer);
    resizes++;
    return resized;
}
// Unbuffered output keeps stdio from allocating a buffer it never frees.
__attribute__((constructor)) static void unbuffered(void) {
    setvbuf(stdout, NULL, _IONBF, 0);
}
__attribute__((destructor)) static void check_releases(void) {
    fprintf(stderr, "allocations=%u releases=%u resizes=%u\n", allocations, releases,
            resizes);
    if (allocations != releases) _Exit(101);
}
