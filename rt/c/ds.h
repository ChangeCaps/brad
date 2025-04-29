#pragma once

#include <stddef.h>

struct bds_vec_t {
    size_t len;
    size_t cap;
    void* data;
};

#define bds_vec_def(name, T)                                                                       \
    struct name {                                                                                  \
        size_t len;                                                                                \
        size_t cap;                                                                                \
        T* data;                                                                                   \
    };                                                                                             \
    static_assert(                                                                                 \
        offsetof(struct name, len) == offsetof(struct bds_vec_t, len),                             \
        "bds_vec_t and " #name " must have the same offset for len"                                \
    );                                                                                             \
    static_assert(                                                                                 \
        offsetof(struct name, cap) == offsetof(struct bds_vec_t, cap),                             \
        "bds_vec_t and " #name " must have the same offset for cap"                                \
    );                                                                                             \
    static_assert(                                                                                 \
        offsetof(struct name, data) == offsetof(struct bds_vec_t, data),                           \
        "bds_vec_t and " #name " must have the same offset for data"                               \
    );                                                                                             \
    static_assert(                                                                                 \
        sizeof(struct name) == sizeof(struct bds_vec_t),                                           \
        "bds_vec_t and " #name " must have the same size"                                          \
    );

inline void bds_vec_drop(const struct bds_vec_t* vec);

inline void bds_vec_push(struct bds_vec_t* vec, const void* value, size_t elem_sz);

inline void bds_vec_pop(struct bds_vec_t* vec, void** value, size_t elem_sz);

inline void* bds_vec_get(const struct bds_vec_t* vec, size_t index, size_t elem_sz);

inline void bds_vec_set(
    const struct bds_vec_t* vec,
    size_t index,
    const void* value,
    size_t elem_sz
);

inline void* bds_vec_get_last(const struct bds_vec_t* vec, size_t elem_sz);

inline void bds_vec_clear(struct bds_vec_t* vec);

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
