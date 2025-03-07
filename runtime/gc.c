#include <stdio.h>
#include <stdlib.h>

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
