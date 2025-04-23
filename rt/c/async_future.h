#pragma once

#include "async_op.h"
#include "async_scheduler.h"

enum brad_async_future_state {
    BRAD_ASYNC_FUTURE_STATE_PENDING = 0,
    BRAD_ASYNC_FUTURE_STATE_READY,
    BRAD_ASYNC_FUTURE_STATE_CANCELED,
};

struct brad_async_future_fns {
    enum brad_async_future_state (*poll)(void*);
    void (*drop)(void*);
};

struct brad_async_future {
    const struct brad_async_future_fns* fns;
    void* state;
};

extern struct brad_async_future_fns brad_async_future_op_fns;

struct brad_async_future brad_async_future_from_op(struct brad_async_op* op);
