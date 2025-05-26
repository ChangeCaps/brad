#include "bae_future.h"

#include <stdatomic.h>

enum bae_future_state bae_future_op_poll(void* op);
void bae_future_op_drop(void* op);

struct bae_future_ops bae_future_op_ops = {
    .poll = bae_future_op_poll,
    .drop = bae_future_op_drop,
};

struct bae_future bae_future_from_op(
    struct bae_op* op
) {
    return (struct bae_future){.ops = &bae_future_op_ops, .state = op};
}

enum bae_future_state bae_future_op_poll(
    void* op
) {
    switch (atomic_load(&((struct bae_op*)op)->state)) {
        case BAE_OP_STATE_PENDING:
            return BAE_FUTURE_STATE_PENDING;
        case BAE_OP_STATE_READY:
            return BAE_FUTURE_STATE_READY;
        case BAE_OP_STATE_CANCELLED:
            return BAE_FUTURE_STATE_CANCELED;
        default:
            unreachable();
    }
}

void bae_future_op_drop(
    void* op
) {}