// Every block ends as near to an inaccessible page as malloc's 16-byte
// alignment allows, so a read 16 or more bytes past a block faults. Blocks are
// never released.
#define _DEFAULT_SOURCE
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

enum { ALIGN = 16 };

void *malloc(size_t size) {
    size_t page = (size_t)sysconf(_SC_PAGESIZE);
    if (size > SIZE_MAX - ALIGN - 2 * page) return NULL;
    size_t extent = (size + ALIGN + page - 1) / page * page;
    char *base = mmap(NULL, extent + page, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED || mprotect(base + extent, page, PROT_NONE) != 0) {
        return NULL;
    }
    return (void *)((uintptr_t)(base + extent - size) & ~(uintptr_t)(ALIGN - 1));
}

void free(void *pointer) {
    (void)pointer;
}
