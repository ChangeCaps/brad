#pragma once

#include <stddef.h>

// Base allocator

struct brad_allocator_fns {
    void* (*new)();
    void (*drop)(void*);
    void (*free)(void*, void*);
    void* (*alloc)(void*, size_t, size_t); // size, count, alignment
};

struct brad_allocator {
    struct brad_allocator_fns* fns;
    void* data;
};

inline struct brad_allocator brad_allocator_new(
    struct brad_allocator_fns* fns
) {
    return (struct brad_allocator){.fns = fns, .data = fns->new()};
}

// General allocator (with realloc support)

struct brad_general_allocator_fns {
    struct brad_allocator_fns base;
    void* (*realloc)(void*, void*, size_t, size_t);
};

struct brad_general_allocator {
    struct brad_general_allocator_fns* fns;
    void* data;
};

inline struct brad_general_allocator brad_general_allocator_new(
    struct brad_general_allocator_fns* fns
) {
    return (struct brad_general_allocator){.fns = fns, .data = fns->base.new()};
}

// Convenience functions

inline void brad_allocator_drop(
    const struct brad_allocator allocator
) {
    allocator.fns->drop(allocator.data);
}

inline void brad_allocator_free(
    const struct brad_allocator allocator,
    void* ptr
) {
    allocator.fns->free(allocator.data, ptr);
}

inline void* brad_allocator_alloc(
    const struct brad_allocator allocator,
    const size_t size,
    const size_t count,
    const size_t alignment
) {
    return allocator.fns->alloc(allocator.data, size * count, alignment);
}

inline void* brad_general_allocator_realloc(
    const struct brad_general_allocator allocator,
    void* ptr,
    const size_t size,
    const size_t count,
    const size_t alignment
) {
    return allocator.fns->realloc(allocator.data, ptr, size * count, alignment);
}

extern struct brad_general_allocator_fns brad_malloc_allocator;