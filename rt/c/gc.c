#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"

#include "debug.h"

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

    DEBUG_MESSAGE("Allocated %p, size: %d, name: %s\n", (void*)ptr, (int)size, allocation->name);

    return ptr;
}

void brad_retain(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count++;

    DEBUG_MESSAGE("Retaining %p, ref count: %d\n", (void*)ptr, (int)allocation->ref_count);
}

void brad_release(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);
    allocation->ref_count--;

    DEBUG_MESSAGE("Releasing %p, ref count: %d\n", (void*)ptr, (int)allocation->ref_count);
}

void brad_mark(
    brad_ptr ptr
) {
    brad_allocation* allocation = brad_allocation_from_ptr(ptr);

    DEBUG_MESSAGE("  marking %p\n", (void*)ptr);

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
    DEBUG_MESSAGE_SINGLE("Collecting garbage:\n\nUnmarking all allocations:\n");

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        allocation->mark_count = 0;
    }

    DEBUG_MESSAGE_SINGLE("\nMarking all referenced allocations:\n");

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        if (allocation->ref_count == 0) {
            continue;
        }

        DEBUG_MESSAGE(
            "Marking %p, ref count: %d\n",
            (void*)brad_context.allocations.allocations[i],
            (int)allocation->ref_count
        );

        brad_mark(brad_context.allocations.allocations[i]);
    }

    DEBUG_MESSAGE_SINGLE("\nFreeing all unmarked allocations:\n");

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        if (allocation->mark_count == 0) {
            DEBUG_MESSAGE(
                "Freeing %p, ref count: %d\n",
                (void*)brad_context.allocations.allocations[i],
                (int)allocation->ref_count
            );
            free(allocation);

            brad_context.allocations.allocations[i] = (brad_ptr)NULL;
        }
    }

    DEBUG_MESSAGE_SINGLE("\nGarbage collection complete:\n\nRemaining allocations:\n");

    for (brad_size i = 0; i < brad_context.allocations.len; i++) {
        if (!brad_context.allocations.allocations[i]) {
            continue;
        }

#ifdef DEBUG
        brad_allocation* allocation =
            brad_allocation_from_ptr(brad_context.allocations.allocations[i]);

        DEBUG_MESSAGE(
            "%p, ref count: %d, name: %s\n",
            (void*)brad_context.allocations.allocations[i],
            (int)allocation->ref_count,
            allocation->name
        );
#endif
    }

    DEBUG_MESSAGE_SINGLE("\n");
}
