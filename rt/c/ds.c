#include "ds.h"
#include "allocator.h"
#include "misc.h"

#include <assert.h>

void bds_vec_maybe_grow(struct bds_vec_t* vec, size_t cap, size_t elem_sz);

void bds_vec_maybe_grow(
    struct bds_vec_t* vec,
    const size_t cap,
    const size_t elem_sz
) {
    // No need to grow
    if (unlikely(vec->cap >= cap)) {
        return;
    }

    void* new_ptr;

    if (vec->data == NULL) {
        new_ptr = brad_allocator_try_alloc(brad_global_allocator, elem_sz * cap, 0);
    } else {
        new_ptr = brad_allocator_realloc(
            brad_global_allocator,
            vec->data,
            elem_sz * vec->cap,
            elem_sz * cap,
            0
        );
    }

    if (new_ptr == NULL) {
        abort();
    }

    vec->cap = cap;
    vec->data = new_ptr;
}

void bds_vec_drop(
    const struct bds_vec_t* vec
) {
    if (vec->data != NULL) {
        brad_allocator_free(brad_global_allocator, vec->data);
    }
}

void bds_vec_push(
    struct bds_vec_t* vec,
    const void* value,
    const size_t elem_sz
) {
    if (vec->len == vec->cap) {
        bds_vec_maybe_grow(vec, vec->cap == 0 ? 1 : vec->cap * 2, elem_sz);
    }

    memcpy((char*)vec->data + (elem_sz * vec->len++), value, elem_sz);
}

void bds_vec_pop(
    struct bds_vec_t* vec,
    void** value,
    const size_t elem_sz
) {
    if (vec->len == 0) {
        *value = NULL;
        return;
    }

    memcpy(value, (char*)vec->data + (elem_sz * --vec->len), elem_sz);
}

void* bds_vec_get(
    const struct bds_vec_t* vec,
    const size_t index,
    const size_t elem_sz
) {
    if (index >= vec->len) {
        return NULL;
    }
    return (char*)vec->data + (elem_sz * index);
}

void bds_vec_set(
    const struct bds_vec_t* vec,
    const size_t index,
    const void* value,
    const size_t elem_sz
) {
    if (index >= vec->len) {
        return;
    }
    memcpy((char*)vec->data + (elem_sz * index), value, elem_sz);
}

void* bds_vec_get_last(
    const struct bds_vec_t* vec,
    const size_t elem_sz
) {
    if (vec->len == 0) {
        return NULL;
    }
    return (char*)vec->data + (elem_sz * (vec->len - 1));
}

void bds_vec_clear(
    struct bds_vec_t* vec
) {
    vec->len = 0;
}

void bds_vec_copy(
    struct bds_vec_t* dst,
    const struct bds_vec_t* src,
    const size_t elem_sz
) {
    bds_vec_clear(dst);
    bds_vec_reserve(dst, src->len, elem_sz);
    memcpy(dst->data, src->data, elem_sz * src->len);
    dst->len = src->len;
}

void bds_vec_reserve(
    struct bds_vec_t* vec,
    const size_t additional,
    const size_t elem_sz
) {
    const size_t new_cap = vec->len + additional;
    bds_vec_maybe_grow(vec, new_cap, elem_sz);
}

void bds_vec_shrink(
    struct bds_vec_t* vec,
    const size_t capacity,
    const size_t elem_sz
) {
    if (vec->cap > capacity) {
        vec->cap = capacity;

        vec->data = brad_allocator_realloc(
            brad_global_allocator,
            vec->data,
            elem_sz * vec->cap,
            elem_sz * capacity,
            0
        );

        vec->len = min(vec->len, capacity);
    }
}