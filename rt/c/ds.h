#pragma once

#include <stddef.h>

// Check struct fields.
#define __bds_csf(a, b, field)                                                                     \
    static_assert(                                                                                 \
        offsetof(a, field) == offsetof(b, field),                                                  \
        "Types " #a " and " #b " must have the same offset for " #field                            \
    )

#define __bds_vec_def(name, T)                                                                     \
    struct name {                                                                                  \
        size_t len;                                                                                \
        size_t cap;                                                                                \
        T* data;                                                                                   \
    };

__bds_vec_def(bds_vec_t, void);

#define bds_vec_def(name, T)                                                                       \
    __bds_vec_def(name, T);                                                                        \
    __bds_csf(struct name, struct bds_vec_t, len);                                                 \
    __bds_csf(struct name, struct bds_vec_t, cap);                                                 \
    __bds_csf(struct name, struct bds_vec_t, data);

void bds_vec_drop(const struct bds_vec_t* vec);

void bds_vec_push(struct bds_vec_t* vec, const void* value, size_t elem_sz);

void bds_vec_pop(struct bds_vec_t* vec, void** value, size_t elem_sz);

void* bds_vec_get(const struct bds_vec_t* vec, size_t index, size_t elem_sz);

void bds_vec_set(
    const struct bds_vec_t* vec,
    size_t index,
    const void* value,
    size_t elem_sz
);

void* bds_vec_get_last(const struct bds_vec_t* vec, size_t elem_sz);

void bds_vec_clear(struct bds_vec_t* vec);

void bds_vec_copy(struct bds_vec_t* dst, const struct bds_vec_t* src, size_t elem_sz);

void bds_vec_reserve(struct bds_vec_t* vec, size_t additional, size_t elem_sz);

void bds_vec_shrink(struct bds_vec_t* vec, size_t capacity, size_t elem_sz);

#define vec_drop(vec) bds_vec_drop((struct bds_vec_t*)(vec))
#define vec_push(vec, value)                                                                       \
    bds_vec_push((struct bds_vec_t*)(vec), (void*)(value), sizeof((vec)->data[0]))
#define vec_pop(vec, vdptr)                                                                        \
    bds_vec_pop((struct bds_vec_t*)(vec), (void**)vdptr, sizeof((vec)->data[0]))
#define vec_get(vec, index)                                                                        \
    ((typeof((vec)->data[0]                                                                        \
    )*)bds_vec_get((struct bds_vec_t*)(vec), (index), sizeof((vec)->data[0])))
#define vec_set(vec, index, value)                                                                 \
    bds_vec_set((struct bds_vec_t*)(vec), (index), (void*)(value), sizeof((vec)->data[0]))
#define vec_get_last(vec) bds_vec_get_last((struct bds_vec_t*)(vec), sizeof((vec)->data[0]))
#define vec_clear(vec)    bds_vec_clear((struct bds_vec_t*)(vec))
#define vec_copy(dst, src)                                                                         \
    bds_vec_copy((struct bds_vec_t*)(dst), (struct bds_vec_t*)(src), sizeof((dst)->data[0]))
#define vec_reserve(vec, additional)                                                               \
    bds_vec_reserve((struct bds_vec_t*)(vec), (additional), sizeof((vec)->data[0]))
#define vec_shrink(vec, capacity)                                                                  \
    bds_vec_shrink((struct bds_vec_t*)(vec), (capacity), sizeof((vec)->data[0]))