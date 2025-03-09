#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

brad_ptr brad_alloc(
    brad_layout layout,
    brad_size size
) {
    printf("Allocating %lu bytes %lu times\n", layout.size, size);
    return (brad_ptr)malloc(layout.size * size);
}

void brad_free(
    brad_ptr ptr
) {
    printf("Freeing brad ptr %016lx\n", ptr);
    return free((void*)ptr);
}

void brad_print(
    brad_str string
) {
    char* data = malloc(string->length + 1);
    memcpy(data, string->data, string->length);
    data[string->length] = '\0';
    printf("%s", data);
}

brad_str brad_str_concat(
    brad_str a,
    brad_str b
) {
    brad_str result = malloc(sizeof(brad_str) + a->length + b->length);

    result->length = a->length + b->length;

    memcpy(result->data, a->data, a->length);
    memcpy(result->data + a->length, b->data, b->length);

    return result;
}
