#include "../ds.h"

#include <assert.h>

struct my_custom_data {
    size_t a;
    float b;
};

bds_vec_def(vec_int_t, int);
bds_vec_def(vec_custom_t, struct my_custom_data);

int main(
    void
) {
    struct vec_int_t vec = {0};
    int v0 = 0;
    int v1 = 1;
    int v2 = 2;

    vec_push(&vec, &v0);
    vec_push(&vec, &v1);
    vec_push(&vec, &v2);

    assert(vec.len == 3);

    struct vec_custom_t vec2 = {0};
    struct my_custom_data data = {.a = 1, .b = 2.0f};
    vec_push(&vec2, &data);
    vec_push(&vec2, &data);
    vec_push(&vec2, &data);
    assert(vec_get(&vec2, 0)->a == 1);
}