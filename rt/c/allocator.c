#include "allocator.h"

#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void* brad_allocator_malloc_new();
void brad_allocator_malloc_drop(void* data);
void brad_allocator_malloc_free(void* data, void* ptr);
void* brad_allocator_malloc_alloc(void* data, size_t size, size_t alignment);
void* brad_allocator_malloc_realloc(void* data, void* ptr, size_t size, size_t alignment);

struct brad_general_allocator_fns brad_malloc_allocator = {
    .base =
        {
            .new = brad_allocator_malloc_new,
            .drop = brad_allocator_malloc_drop,
            .free = brad_allocator_malloc_free,
            .alloc = brad_allocator_malloc_alloc,
        },
    .realloc = brad_allocator_malloc_realloc,
};

void* brad_allocator_malloc_new() { return NULL; }

void brad_allocator_malloc_drop(
    void* data
) {
    // Nothing to do
}

void brad_allocator_malloc_free(
    void* data,
    void* ptr
) {
    free(ptr);
}

void* brad_allocator_malloc_alloc(
    void* data,
    const size_t size,
    const size_t alignment
) {
    void* ptr = NULL;
    if (alignment == 0) {
        ptr = malloc(size);
    } else {
        posix_memalign(&ptr, alignment, size);
    }
    return ptr;
}

void* brad_allocator_malloc_realloc(
    void* data,
    void* ptr,
    const size_t size,
    const size_t alignment
) {
    void* new_ptr = realloc(ptr, size);

    if (alignment == 0 || ((uintptr_t)new_ptr % alignment) == 0) {
        return new_ptr;
    }

    // Allocate new pointer so alignment holds
    void* aligned_ptr = brad_allocator_malloc_alloc(data, size, alignment);

    if (aligned_ptr == NULL) {
        return NULL;
    }

    memcpy(aligned_ptr, new_ptr, size);
    free(new_ptr);
    return aligned_ptr;
}