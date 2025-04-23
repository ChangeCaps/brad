#include "async_future.h"

#include <stdatomic.h>

enum brad_async_future_state brad_async_future_op_poll(void* op);
void brad_async_future_op_drop(void* op);

struct brad_async_future_fns brad_async_future_op_fns = {
    .poll = brad_async_future_op_poll,
    .drop = brad_async_future_op_drop,
};

struct brad_async_future brad_async_future_from_op(
    struct brad_async_op* op
) {
    return (struct brad_async_future){.fns = &brad_async_future_op_fns, .state = op};
}

enum brad_async_future_state brad_async_future_op_poll(
    void* op
) {
    switch (atomic_load(&((struct brad_async_op*)op)->state)) {
        case BRAD_ASYNC_OP_STATE_PENDING:
            return BRAD_ASYNC_FUTURE_STATE_PENDING;
        case BRAD_ASYNC_OP_STATE_READY:
            return BRAD_ASYNC_FUTURE_STATE_READY;
        case BRAD_ASYNC_OP_STATE_CANCELLED:
            return BRAD_ASYNC_FUTURE_STATE_CANCELED;
        default:
            unreachable();
    }
}

void brad_async_future_op_drop(
    void* op
) {}