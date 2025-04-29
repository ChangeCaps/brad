#include "ds.h"
#include "allocator.h"

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