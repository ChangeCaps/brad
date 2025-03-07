#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

BradPtr brad_alloc(
    BradMemoryLayout layout,
    BradSize size
) {
    printf("Allocating %lu bytes %lu times\n", layout.size, size);
    return (BradPtr)malloc(layout.size * size);
}

void brad_free(
    BradPtr ptr
) {
    printf("Freeing brad ptr %016lx\n", ptr);
    return free((void*)ptr);
}

void brad_print(
    BradString string
) {
    char* data = malloc(string->length + 1);
    memcpy(data, string->data, string->length);
    data[string->length] = '\0';
    printf("%s\n", data);
}
