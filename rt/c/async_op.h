#pragma once
#include <linux/time_types.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum brad_async_op_t {
    BRAD_ASYNC_OP_NOP,
    BRAD_ASYNC_OP_MSG_EL,
    BRAD_ASYNC_OP_SLEEP, // simple timeout, no actions other than waiting for some time.
    BRAD_ASYNC_OP_CANCEL,
    BRAD_ASYNC_OP_CHAIN, // submit sequential operations
    BRAD_ASYNC_OP_BATCH, // batch submit concurrent operations.
};

/* Instance of an async operation, updated using atomic operations, specific operations inherit this
 * as its base struct.
 *
 * The parameters typically contain output values, these cannot be read until the event loop no
 * longer owns those fields, which is when the state no longer is pending.
 */
struct brad_async_op {
    enum brad_async_op_t op;
    _Atomic enum {
        BRAD_ASYNC_OP_STATE_PENDING = 0,
        BRAD_ASYNC_OP_STATE_READY,
        BRAD_ASYNC_OP_STATE_CANCELLED
    } state;
};

struct brad_async_op_msg_el {
    struct brad_async_op base;
    void* el_data;
};

struct brad_async_op_sleep {
    struct brad_async_op base;
    struct __kernel_timespec ts;
};

struct brad_async_op_cancel {
    struct brad_async_op base;
    struct brad_async_op* op; // cancellation target, a cancellation can be cancelled as well!
};

/* To be used with CHAIN or BATCH ops, TODO, how to cancel? */
struct brad_async_op_multiple {
    struct brad_async_op base;
    struct brad_async_op** ops;
    size_t count;
};

// Builder helpers, no-allocations.
inline void brad_async_prep_nop(
    struct brad_async_op* op
) {
    op->op = BRAD_ASYNC_OP_NOP;
}


inline void brad_async_prep_msg_el(
    struct brad_async_op_msg_el* op,
    void* el_data
) {
    op->base.op = BRAD_ASYNC_OP_MSG_EL;
    op->el_data = el_data;
}

inline void brad_async_prep_sleep(
    struct brad_async_op_sleep* op,
    const struct __kernel_timespec ts
) {
    op->base.op = BRAD_ASYNC_OP_SLEEP;
    op->ts = ts;
}

inline void brad_async_prep_cancel(
    struct brad_async_op_cancel* op,
    struct brad_async_op* target
) {
    op->base.op = BRAD_ASYNC_OP_CANCEL;
    op->op = target;
}

inline void brad_async_prep_multiple(
    struct brad_async_op_multiple* op,
    struct brad_async_op** ops,
    const bool chain
) {
    op->base.op = chain ? BRAD_ASYNC_OP_CHAIN : BRAD_ASYNC_OP_BATCH;
    op->ops = ops;
    op->count = 0;
}

inline void brad_async_prep_multiple_add(
    struct brad_async_op_multiple* op,
    struct brad_async_op* async_op
) {
    op->ops[op->count++] = async_op;
}