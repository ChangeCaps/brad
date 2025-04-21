#pragma once

#include "async_op.h"

#include <stdbool.h>

struct brad_async_event_loop_fns {
    // Constructor
    struct brad_async_event_loop (*new)();
    // Destructor
    void (*drop)(struct brad_async_event_loop*);

    // Submit an async operation
    void (*submit)(struct brad_async_event_loop*, struct brad_async_op*);

    // Wait/poll
    void (*enter)(struct brad_async_event_loop*);
};

struct brad_async_event_loop {
    struct brad_async_event_loop_fns* fns;
    int32_t fd;
    void* data;
};

extern struct brad_async_event_loop_fns io_uring_loop_fns;