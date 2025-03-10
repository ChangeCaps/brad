#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

brad_thread_context brad_context = {0};

void brad_init() {
    brad_context.allocations.len = 0;
    brad_context.allocations.cap = 0;
    brad_context.allocations.allocations = NULL;
}

void brad_deinit() {
    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        free((void*)brad_context.allocations.allocations[i]);
    }
}

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
    brad_marker marker,
    const char* name
) {
    brad_allocation* allocation = malloc(sizeof(brad_allocation) + size);
    allocation->name = name;
    allocation->marker = marker;
    allocation->ref_count = 1;

    brad_ptr ptr = brad_allocation_to_ptr(allocation);

    brad_push_allocation(ptr);

#ifdef DEBUG
    printf("Allocated %p, size: %d\n", (void*)ptr, (int)size);
#endif

    return ptr;
}

void brad_retain(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count++;

#ifdef DEBUG
    printf(
        "Retaining %p, ref count: %d\n",
        (void*)ptr,
        (int)allocation->ref_count
    );
#endif
}

void brad_release(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count--;

#ifdef DEBUG
    printf(
        "Releasing %p, ref count: %d\n",
        (void*)ptr,
        (int)allocation->ref_count
    );
#endif
}

void brad_mark(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);

#ifdef DEBUG
    printf("  marking %p\n", (void*)ptr);
#endif

    allocation->mark_count = 1;
    allocation->marker(ptr);
}

brad_marker brad_get_marker(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    return allocation->marker;
}

void brad_collect() {
#ifdef DEBUG
    printf("Collecting garbage:\n\n");
    printf("Unmarking all allocations:\n");
#endif

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        allocation->mark_count = 0;
    }

#ifdef DEBUG
    printf("\n");
    printf("Marking all referenced allocations:\n");
    printf("\n");
#endif

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        if (allocation->ref_count == 0) {
            continue;
        }

#ifdef DEBUG
        printf(
            "Marking root %p\n",
            (void*)brad_context.allocations.allocations[i]
        );
#endif

        brad_mark(brad_context.allocations.allocations[i]);
    }

#ifdef DEBUG
    printf("\n");
    printf("Freeing all unmarked allocations:\n");
    printf("\n");
#endif

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        if (allocation->mark_count == 0) {
#ifdef DEBUG
            printf(
                "Freeing %p\n",
                (void*)brad_context.allocations.allocations[i]
            );
#endif
            free(allocation);

            brad_context.allocations.allocations[i] = (brad_ptr)NULL;
        }
    }

#ifdef DEBUG
    printf("\n");
    printf("Garbage collection complete:\n");
    printf("\n");
    printf("Remaining allocations:\n");
#endif

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

#ifdef DEBUG
        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        printf(
            "%p, ref count: %d, name: %s\n",
            (void*)brad_context.allocations.allocations[i],
            (int)allocation->ref_count,
            allocation->name
        );
#endif
    }

#ifdef DEBUG
    printf("\n");
#endif
}
