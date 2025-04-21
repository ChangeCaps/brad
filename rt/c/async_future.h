#pragma once

#include "async_scheduler.h"

enum brad_async_future_state {
    BRAD_FUTURE_STATE_PENDING,
    BRAD_FUTURE_STATE_READY,
    BRAD_FUTURE_STATE_CANCELED,
};

struct brad_async_future_fns {
    enum brad_async_future_state (*poll)(struct brad_async_future*);
    void (*drop)(struct brad_async_future*);
};

struct brad_async_future {
    const struct brad_async_future_fns* fns;
    void* state;
};