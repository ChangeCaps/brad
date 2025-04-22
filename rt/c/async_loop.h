#pragma once

#include "async_op.h"

#include <stdbool.h>

struct brad_async_event_loop_fns {
    // Constructor
    void* (*new)();

    // Destructor
    void (*drop)(void*);

    // Submit an async operation (pointer-unique)
    void (*submit)(void*, struct brad_async_op*);

    // Block until an operation is completed.
    struct brad_async_op* (*enter)(void*);
    // Mark operation as *read* which will allow for some cleanup.
    void (*leave)(void*, struct brad_async_op*);
};

struct brad_async_event_loop {
    struct brad_async_event_loop_fns* fns;
    void* data;
};

extern struct brad_async_event_loop_fns io_uring_loop_fns;