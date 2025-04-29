#include "allocator.h"

#include "debug.h"

#include <locale.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void* brad_allocator_malloc_new();
void brad_allocator_malloc_drop(void* data);
void* brad_allocator_malloc_alloc(void* data, size_t size, size_t alignment);
void* brad_allocator_malloc_realloc(
    void* data,
    void* ptr,
    size_t old_sz,
    size_t new_sz,
    size_t alignment
);
void brad_allocator_malloc_free(void* data, void* ptr);

struct brad_allocator_ops brad_malloc_allocator = {
    .new = brad_allocator_malloc_new,
    .drop = brad_allocator_malloc_drop,
    .free = brad_allocator_malloc_free,
    .alloc = brad_allocator_malloc_alloc,
    .realloc = brad_allocator_malloc_realloc,
};

void* brad_allocator_malloc_new() { return NULL; }

void brad_allocator_malloc_drop(
    void* data
) {
    DEBUG_MESSAGE_SINGLE("[WARN] Cannot drop malloc-allocator, as it is a global allocator.\n");
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
    const size_t old_sz,
    const size_t new_sz,
    const size_t alignment
) {
    // Always try realloc, we could not do this to prevent 2x memcpy but aligned reallocs are an
    // edge case.
    void* new_ptr = realloc(ptr, new_sz);

    if (new_ptr == nullptr) {
        return nullptr;
    }

    // Check if the new pointer is aligned
    if (alignment == 0 || ((uintptr_t)new_ptr % alignment) == 0) {
        return new_ptr;
    }

    // Allocate new pointer so alignment holds
    void* aligned_ptr = brad_allocator_malloc_alloc(data, new_sz, alignment);

    if (aligned_ptr == NULL) {
        return NULL;
    }

    memcpy(aligned_ptr, new_ptr, old_sz);
    free(new_ptr);

    return aligned_ptr;
}

void brad_allocator_malloc_free(
    void* data,
    void* ptr
) {
    free(ptr);
}