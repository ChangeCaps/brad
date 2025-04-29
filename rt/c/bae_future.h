#pragma once

#include "bae_op.h"
#include "debug.h"

enum bae_future_state {
    BAE_FUTURE_STATE_PENDING = 0,
    BAE_FUTURE_STATE_READY,
    BAE_FUTURE_STATE_CANCELED,
};

struct bae_future_ops {
    enum bae_future_state (*poll)(void*);
    void (*drop)(void*);
};

struct bae_future {
    const struct bae_future_ops* ops;
    void* state;
};

INLINE enum bae_future_state bae_future_poll(
    const struct bae_future future
) {
    return future.ops->poll(future.state);
}

INLINE void bae_future_drop(
    const struct bae_future future
) {
    future.ops->drop(future.state);
}

extern struct bae_future_ops bae_future_op_ops;

struct bae_future bae_future_from_op(struct bae_op* op);
