#pragma once

#include "debug.h"

#include <stdlib.h>
#include <string.h>

/**
 * Allocator interface for general purpose memory management.
 */
struct brad_allocator_ops {
    void* (*new)();
    void (*drop)(void*);
    // size, alignment
    void* (*alloc)(void*, size_t, size_t);
    // ptr, old_sz, new_sz, alignment
    void* (*realloc)(void*, void*, size_t, size_t, size_t);
    // ptr
    void (*free)(void*, void*);
};

struct brad_allocator {
    const struct brad_allocator_ops* ops;
    void* data;
};

INLINE struct brad_allocator brad_allocator_new(
    const struct brad_allocator_ops* ops
) {
    return (struct brad_allocator){.ops = ops, .data = ops->new()};
}

INLINE void brad_allocator_drop(
    const struct brad_allocator allocator
) {
    allocator.ops->drop(allocator.data);
}

INLINE void brad_allocator_free(
    const struct brad_allocator allocator,
    void* ptr
) {
    allocator.ops->free(allocator.data, ptr);
}

INLINE void* brad_allocator_try_alloc(
    const struct brad_allocator allocator,
    const size_t size,
    const size_t alignment
) {
    return allocator.ops->alloc(allocator.data, size, alignment);
}

INLINE void* brad_allocator_alloc(
    const struct brad_allocator allocator,
    const size_t size,
    const size_t alignment
) {
    const auto ptr = allocator.ops->alloc(allocator.data, size, alignment);

    if (ptr == NULL) {
        abort();
    }

    return ptr;
}

INLINE void* brad_allocator_try_calloc(
    const struct brad_allocator allocator,
    const size_t nmemb,
    const size_t size,
    const size_t alignment
) {
    void* ptr = allocator.ops->alloc(allocator.data, nmemb * size, alignment);

    if (ptr != NULL) {
        memset(ptr, 0, nmemb * size);
    }

    return ptr;
}

INLINE void* brad_allocator_calloc(
    const struct brad_allocator allocator,
    const size_t nmemb,
    const size_t size,
    const size_t alignment
) {
    void* ptr = allocator.ops->alloc(allocator.data, nmemb * size, alignment);

    if (ptr != NULL) {
        memset(ptr, 0, nmemb * size);
    } else {
        abort();
    }

    return ptr;
}

INLINE void* brad_allocator_realloc(
    const struct brad_allocator allocator,
    void* ptr,
    const size_t old_sz,
    const size_t new_sz,
    const size_t alignment
) {
    return allocator.ops->realloc(allocator.data, ptr, old_sz, new_sz, alignment);
}

extern struct brad_allocator_ops brad_malloc_allocator;

// Global allocator.
static const struct brad_allocator brad_global_allocator = {
    .ops = &brad_malloc_allocator,
    .data = NULL,
};
