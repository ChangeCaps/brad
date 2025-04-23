#pragma once

#include "async_op.h"

struct brad_async_event_loop_fns {
    // Constructor
    void* (*new)();

    // Destructor
    void (*drop)(void*);

    // Submit an async operation (pointer-unique)
    void (*submit)(void*, struct brad_async_op_params*, struct brad_async_op*);
    void (*submitv)(void*, struct brad_async_op_params**, struct brad_async_op**, size_t, bool);

    // Block until an operation is completed.
    struct brad_async_op* (*enter)(void*);
    // Mark operation as *read* which will allow for some cleanup.
    void (*leave)(void*, const struct brad_async_op*);
};

struct brad_async_event_loop {
    struct brad_async_event_loop_fns* fns;
    void* data;
};

extern struct brad_async_event_loop_fns io_uring_loop_fns;