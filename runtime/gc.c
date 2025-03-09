#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

brad_thread_context brad_context = {0};

static void brad_push_allocation(
    brad_ptr ptr
) {
    if (brad_context.allocations.len == 0) {
        brad_context.allocations.cap = 16;
        brad_context.allocations.allocations =
            malloc(brad_context.allocations.cap * sizeof(brad_ptr));
    } else if (brad_context.allocations.len >= brad_context.allocations.cap) {
        brad_context.allocations.cap *= 2;
        brad_context.allocations.allocations = realloc(
            brad_context.allocations.allocations,
            brad_context.allocations.cap * sizeof(brad_ptr)
        );
    }

    brad_context.allocations.allocations[brad_context.allocations.len++] = ptr;
}

brad_ptr brad_alloc(
    brad_size size,
    brad_marker marker
) {
    brad_allocation* allocation = malloc(sizeof(brad_allocation) + size);
    allocation->marker = marker;
    allocation->ref_count = 1;

    brad_ptr ptr = brad_allocation_to_ptr(allocation);

    brad_push_allocation(ptr);

    printf("Allocated %p, size: %d\n", (void*)ptr, (int)size);

    return ptr;
}

void brad_retain(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count++;

    printf(
        "Retaining %p, ref count: %d\n",
        (void*)ptr,
        (int)allocation->ref_count
    );
}

void brad_release(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count--;

    printf(
        "Releasing %p, ref count: %d\n",
        (void*)ptr,
        (int)allocation->ref_count
    );
}

void brad_mark(
    brad_ptr ptr
) {}

void brad_collect() {
    printf("Collecting garbage\n");

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        printf(
            "Marking root %p\n",
            (void*)brad_context.allocations.allocations[i]
        );
    }
}

void brad_print(
    brad_str string
) {
    fwrite(string->data, 1, string->length, stdout);

    brad_release((brad_ptr)string);
}

static void brad_str_marker(
    brad_ptr ptr
) {}

brad_str brad_str_concat(
    brad_str a,
    brad_str b
) {
    brad_size new_size = sizeof(brad_str) + a->length + b->length;
    brad_str result = (brad_str)brad_alloc(new_size, brad_str_marker);

    result->length = a->length + b->length;

    memcpy(result->data, a->data, a->length);
    memcpy(result->data + a->length, b->data, b->length);

    brad_release((brad_ptr)a);
    brad_release((brad_ptr)b);

    return result;
}
