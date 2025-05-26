#pragma once

#include "allocator.h"
#include "debug.h"

#include <linux/time_types.h>
#include <stddef.h>
#include <stdint.h>

/* Supported basic operation, not guaranteed to be one syscall. */
enum bae_op_t {
    BAE_OP_NOP,
    BAE_OP_MSG_EL,
    BAE_OP_SLEEP, // simple timeout, no actions other than waiting for some time.
    BAE_OP_WRITE, // simple fd write
    BAE_OP_READ,  // simple fd read
    BAE_OP_CANCEL,
};

enum bae_op_state { BAE_OP_STATE_PENDING = 0, BAE_OP_STATE_READY, BAE_OP_STATE_CANCELLED };

/* An instance of an async operation that must live until completion
This struct is later extended to contain output fields.

This structure is allocated based on parameters.

TODO, we have an extra 4 bytes of data to do something with.
If the atomic state is slow, perhaps a dynamic atomic is better?
*/
struct bae_op {
    _Atomic enum bae_op_state state;
    void* waker_ref;
};

struct bae_op_sleep {
    struct bae_op base;
    // ts needs to outlive parameters.
    struct __kernel_timespec ts;
};

struct bae_op_rw {
    struct bae_op base;

    union {
        uint16_t buf_index; // TODO, how to abstract fixed buffers?
    };
};

/* Descriptor struct for any operation submitted to the loop,
this does not have to live beyond the submit call. */
struct bae_op_params {
    enum bae_op_t op;
};

struct bae_op_msg_el_params {
    struct bae_op_params base;
    void* el_data;
};

struct bae_op_sleep_params {
    struct bae_op_params base;
    struct __kernel_timespec ts;
};

struct bae_op_rw_params {
    struct bae_op_params base;
    int32_t fd;
    size_t nbytes;
    size_t offset;

    enum {
        BAE_OP_RW_BUF_STABLE,
        BAE_OP_RW_BUF_FIXED,
    } buf_t;

    union {
        void* buf;
        struct iovec* iovecs;
        uint16_t buf_index;
    };
};

struct bae_op_cancel_params {
    struct bae_op_params base;
    struct bae_op* op; // cancellation target, a cancellation can be cancelled as well!
};

// Builder helpers, no-allocations.
INLINE void bae_prep_nop(
    struct bae_op_params* op
) {
    op->op = BAE_OP_NOP;
}

INLINE void bae_prep_msg_el(
    struct bae_op_msg_el_params* op,
    void* el_data
) {
    op->base.op = BAE_OP_MSG_EL;
    op->el_data = el_data;
}

INLINE void bae_prep_sleep(
    struct bae_op_sleep_params* op,
    const struct __kernel_timespec ts
) {
    op->base.op = BAE_OP_SLEEP;
    op->ts = ts;
}

INLINE void bae_prep_read(
    struct bae_op_rw_params* op,
    const int32_t fd,
    void* buf,
    const size_t nbytes,
    const size_t offset
) {
    op->base.op = BAE_OP_READ;
    op->fd = fd;
    op->buf_t = BAE_OP_RW_BUF_STABLE;
    op->buf = buf;
    op->nbytes = nbytes;
    op->offset = offset;
}

INLINE void bae_prep_write(
    struct bae_op_rw_params* op,
    const int32_t fd,
    void* buf,
    const size_t nbytes,
    const size_t offset
) {
    op->base.op = BAE_OP_WRITE;
    op->fd = fd;
    op->buf_t = BAE_OP_RW_BUF_STABLE;
    op->buf = buf;
    op->nbytes = nbytes;
    op->offset = offset;
}

INLINE void bae_prep_cancel(
    struct bae_op_cancel_params* op,
    struct bae_op* target
) {
    op->base.op = BAE_OP_CANCEL;
    op->op = target;
}

INLINE size_t bae_op_size_from_params(
    const struct bae_op_params* params
) {

    switch (params->op) {
        case BAE_OP_SLEEP:
            return sizeof(struct bae_op_sleep);
        case BAE_OP_READ:
        case BAE_OP_WRITE:
            return sizeof(struct bae_op_rw);
        default:
            return sizeof(struct bae_op);
    }
}

INLINE void bae_op_from_params(
    const struct brad_allocator allocator,
    const struct bae_op_params* params,
    struct bae_op** op
) {
    *op = brad_allocator_alloc(allocator, bae_op_size_from_params(params), 0);
}

INLINE void bae_ops_from_params(
    const struct brad_allocator allocator,
    struct bae_op_params** params,
    struct bae_op** ops,
    const size_t count
) {
    for (size_t i = 0; i < count; i++) {
        bae_op_from_params(allocator, params[i], &ops[i]);
    }
}

INLINE const char* bae_op_state_to_string(
    const enum bae_op_state state
) {
    switch (state) {
        case BAE_OP_STATE_PENDING:
            return "PENDING";
        case BAE_OP_STATE_READY:
            return "READY";
        case BAE_OP_STATE_CANCELLED:
            return "CANCELLED";
        default:
            return "UNKNOWN";
    }
}

INLINE const char* bae_op_t_to_string(
    const enum bae_op_t op
) {
    switch (op) {
        case BAE_OP_NOP:
            return "NOP";
        case BAE_OP_MSG_EL:
            return "MSG_EL";
        case BAE_OP_SLEEP:
            return "SLEEP";
        case BAE_OP_WRITE:
            return "WRITE";
        case BAE_OP_READ:
            return "READ";
        case BAE_OP_CANCEL:
            return "CANCEL";
        default:
            return "UNKNOWN";
    }
}
