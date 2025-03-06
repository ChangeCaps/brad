#include "gc.h"
#include <stdio.h>

BradPtr brad_alloc(
    BradMemoryLayout layout,
    BradSize size
) {
    printf("Allocating %lu bytes %lu times\n", layout.size, size);
    return 0x1337;
}

void brad_free(
    BradPtr ptr
) {
    printf("Freeing brad ptr %016lx\n", ptr);
    return;
}
