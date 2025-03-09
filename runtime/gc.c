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
    printf("%s\n", data);
}
