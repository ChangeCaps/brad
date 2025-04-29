#include "ds.h"
#include "allocator.h"
#include "misc.h"

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
        const size_t old_cap = vec->cap;
        vec->cap = vec->cap == 0 ? 1 : vec->cap * 2;
        vec->data = brad_allocator_realloc(
            brad_global_allocator,
            vec->data,
            elem_sz * old_cap,
            elem_sz * vec->cap,
            0
        );
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
    const size_t old_cap = vec->cap;
    const size_t new_cap = old_cap + additional;

    if (old_cap < new_cap) {
        vec->cap = new_cap;
        vec->data = brad_allocator_realloc(
            brad_global_allocator,
            vec->data,
            elem_sz * old_cap,
            elem_sz * vec->cap,
            0
        );
    }
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