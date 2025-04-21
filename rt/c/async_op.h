#pragma once
#include <stdint.h>

enum brad_async_op_t {
    BRAD_ASYNC_OP_NOP,
    BRAD_ASYNC_OP_NOTIFY,
};

/* Instance of an async operation, updated using atomic operations, specific operations inherit this
 * as its base struct.
 *
 * The parameters typically contain output values, these cannot be read until the event loop no
 * longer owns those fields, which is when the state no longer is pending.
 */
struct brad_async_op {
    enum brad_async_op_t op;
    enum {
        BRAD_ASYNC_OP_STATE_PENDING,
        BRAD_ASYNC_OP_STATE_READY,
        BRAD_ASYNC_OP_STATE_CANCELLED
    } state;
    void (*waker)(); // TODO; Waker needs some context?
};

struct brad_async_op_nop {
    struct brad_async_op base;
};

struct brad_async_op_notify {
    struct brad_async_op base;
    int32_t fd;
};