#pragma once
#include "allocator.h"

#include <linux/time_types.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* Supported basic operation, not guaranteed to be one syscall. */
enum brad_async_op_t {
    BRAD_ASYNC_OP_NOP,
    BRAD_ASYNC_OP_MSG_EL,
    BRAD_ASYNC_OP_SLEEP, // simple timeout, no actions other than waiting for some time.
    BRAD_ASYNC_OP_WRITE, // simple fd write
    BRAD_ASYNC_OP_READ,  // simple fd read
    BRAD_ASYNC_OP_CANCEL,
};

enum brad_async_op_state {
    BRAD_ASYNC_OP_STATE_PENDING = 0,
    BRAD_ASYNC_OP_STATE_READY,
    BRAD_ASYNC_OP_STATE_CANCELLED
};

/* An instance of an async operation that must live until completion
This struct is later extended to contain output fields.

This structure is allocated based on parameters.

TODO, we have an extra 4 bytes of data to do something with.
*/
struct brad_async_op {
    _Atomic enum brad_async_op_state state;
    void* waker_ref;
};

struct brad_async_op_sleep {
    struct brad_async_op base;
    // ts needs to outlive parameters.
    struct __kernel_timespec ts;
};

struct brad_async_op_rw {
    struct brad_async_op base;

    union {
        void* buf;
        struct iovec* iovecs;
        uint16_t buf_index; // TODO, how to abstract fixed buffers?
    };
};

/* Descriptor struct for any operation submitted to the loop,
this does not have to live beyond the submit call. */
struct brad_async_op_params {
    enum brad_async_op_t op;
};

struct brad_async_op_params_msg_el {
    struct brad_async_op_params base;
    void* el_data;
};

struct brad_async_op_params_sleep {
    struct brad_async_op_params base;
    struct __kernel_timespec ts;
};

struct brad_async_op_params_rw {
    struct brad_async_op_params base;
    int32_t fd;
    size_t nbytes;
    size_t offset;
};

struct brad_async_op_params_cancel {
    struct brad_async_op_params base;
    struct brad_async_op* op; // cancellation target, a cancellation can be cancelled as well!
};

// Builder helpers, no-allocations.
inline void brad_async_prep_nop(
    struct brad_async_op_params* op
) {
    op->op = BRAD_ASYNC_OP_NOP;
}

inline void brad_async_prep_msg_el(
    struct brad_async_op_params_msg_el* op,
    void* el_data
) {
    op->base.op = BRAD_ASYNC_OP_MSG_EL;
    op->el_data = el_data;
}

inline void brad_async_prep_sleep(
    struct brad_async_op_params_sleep* op,
    const struct __kernel_timespec ts
) {
    op->base.op = BRAD_ASYNC_OP_SLEEP;
    op->ts = ts;
}

inline void brad_async_prep_read(
    struct brad_async_op_params_rw* op,
    const int32_t fd,
    const size_t nbytes,
    const size_t offset
) {
    op->base.op = BRAD_ASYNC_OP_READ;
    op->fd = fd;
    op->nbytes = nbytes;
    op->offset = offset;
}

inline void brad_async_prep_write(
    struct brad_async_op_params_rw* op,
    const int32_t fd,
    const size_t nbytes,
    const size_t offset
) {
    op->base.op = BRAD_ASYNC_OP_WRITE;
    op->fd = fd;
    op->nbytes = nbytes;
    op->offset = offset;
}

inline void brad_async_prep_cancel(
    struct brad_async_op_params_cancel* op,
    struct brad_async_op* target
) {
    op->base.op = BRAD_ASYNC_OP_CANCEL;
    op->op = target;
}

inline size_t brad_async_op_size_from_params(
    const struct brad_async_op_params* params
) {

    switch (params->op) {
        case BRAD_ASYNC_OP_SLEEP:
            return sizeof(struct brad_async_op_sleep);
        case BRAD_ASYNC_OP_READ:
        case BRAD_ASYNC_OP_WRITE:
            return sizeof(struct brad_async_op_rw);
        default:
            return sizeof(struct brad_async_op);
    }
}

inline struct brad_async_op* brad_async_op_from_params(
    const struct brad_allocator allocator,
    const struct brad_async_op_params* params
) {
    return brad_allocator_alloc(allocator, brad_async_op_size_from_params(params), 1, 0);
}

inline struct brad_async_op** brad_async_ops_from_params(
    const struct brad_allocator allocator,
    struct brad_async_op_params** params,
    const size_t count
) {
    struct brad_async_op** ops =
        brad_allocator_alloc(allocator, sizeof(struct brad_async_op*), count, 0);
    for (size_t i = 0; i < count; i++) {
        ops[i] = brad_async_op_from_params(allocator, params[i]);
    }
    return ops;
}