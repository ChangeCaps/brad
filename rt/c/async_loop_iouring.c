#include "async_loop.h"
#include "debug.h"

#include <assert.h>
#include <liburing.h>
#include <stdlib.h>
#include <string.h>

void* new();
void drop(void* data);
void submit(void* data, struct brad_async_op_params* params, struct brad_async_op* op);
void submitv(
    void* data,
    struct brad_async_op_params** params,
    struct brad_async_op** ops,
    size_t count,
    bool chain
);
struct brad_async_op* enter(void* data);
void leave(void* data, const struct brad_async_op* op);

struct LoopContext {
    struct io_uring ring;
    struct io_uring_params params;
    struct io_uring_cqe* cqe; // current cqe for enter <-> leave.
};

void prepare_ctx_with_op(
    struct LoopContext* ctx,
    struct brad_async_op_params* params,
    struct brad_async_op* op,
    uint8_t flags
);

static enum brad_async_op_state pending_state = BRAD_ASYNC_OP_STATE_PENDING;

struct brad_async_event_loop_fns io_uring_loop_fns = {
    .new = new,
    .drop = drop,
    .submit = submit,
    .submitv = submitv,
    .enter = enter,
    .leave = leave,
};

void* new() {
    struct LoopContext* ctx = calloc(1, sizeof(struct LoopContext));

    ctx->params.flags |= IORING_SETUP_SQPOLL;

    // ctx->params.flags |= IORING_SETUP_ATTACH_WQ;
    // ctx->params.wq_fd = TODO;

    ctx->params.sq_thread_idle = 10; // Allow kernel 10 milliseconds of idle time. Optimially we
                                     // want to control this value dynamically.

    int ret = io_uring_queue_init_params(8, &ctx->ring, &ctx->params);

    DEBUG_MESSAGE("Initializing io_uring loop, ret: %d\n", ret);

    return ctx;
}

void drop(
    void* data
) {
    struct LoopContext* ctx = data;
    io_uring_queue_exit(&ctx->ring);
    free(ctx);
    DEBUG_MESSAGE_SINGLE("Exiting io_uring loop\n");
}

static struct __kernel_timespec ts_max = {
    .tv_sec = UINT64_MAX,
    .tv_nsec = UINT64_MAX,
};

void prepare_ctx_with_op(
    struct LoopContext* ctx,
    struct brad_async_op_params* params,
    struct brad_async_op* op,
    const uint8_t flags
) {
    struct io_uring_sqe* sqe = io_uring_get_sqe(&ctx->ring);
    assert(sqe != NULL);

    switch (params->op) {
        case BRAD_ASYNC_OP_NOP: {
            io_uring_prep_nop(sqe);
            break;
        }
        case BRAD_ASYNC_OP_MSG_EL: {
            const struct brad_async_op_params_msg_el* msg_el_params =
                (struct brad_async_op_params_msg_el*)params;
            const struct LoopContext* el_ctx = msg_el_params->el_data;
            io_uring_prep_msg_ring(sqe, el_ctx->ring.ring_fd, 0, 0, 0);
            break;
        }
        case BRAD_ASYNC_OP_SLEEP: {
            const struct brad_async_op_params_sleep* sleep_params =
                (struct brad_async_op_params_sleep*)params;
            struct brad_async_op_sleep* sleep_op = (struct brad_async_op_sleep*)op;
            memcpy(&sleep_op->ts, &sleep_params->ts, sizeof(struct __kernel_timespec));
            io_uring_prep_timeout(sqe, &sleep_op->ts, 0, IORING_TIMEOUT_ETIME_SUCCESS);
            break;
        }
        case BRAD_ASYNC_OP_READ: {
            const struct brad_async_op_params_rw* read_params =
                (struct brad_async_op_params_rw*)params;
            const struct brad_async_op_rw* read_op = (struct brad_async_op_rw*)op;
            io_uring_prep_read(
                sqe,
                read_params->fd,
                read_op->buf,
                read_params->nbytes,
                read_params->offset
            );
            break;
        }
        case BRAD_ASYNC_OP_WRITE: {
            const struct brad_async_op_params_rw* write_params =
                (struct brad_async_op_params_rw*)params;
            const struct brad_async_op_rw* write_op = (struct brad_async_op_rw*)op;
            io_uring_prep_write(
                sqe,
                write_params->fd,
                write_op->buf,
                write_params->nbytes,
                write_params->offset
            );
            break;
        }
        case BRAD_ASYNC_OP_CANCEL: {
            const struct brad_async_op_params_cancel* cancel_params =
                (struct brad_async_op_params_cancel*)params;

            // Preemptively mark the operation as cancelled.
            atomic_compare_exchange_strong(
                &cancel_params->op->state,
                &pending_state,
                BRAD_ASYNC_OP_STATE_CANCELLED
            );

            io_uring_prep_cancel(sqe, cancel_params->op, 0);
            break;
        }
        default: {
            DEBUG_MESSAGE("Unknown async op type: %d\n", params->op);
            break;
        }
    }

    // Configure each submission queue entry.
    io_uring_sqe_set_data(sqe, op);
    sqe->flags |= flags;
}

void submit(
    void* data,
    struct brad_async_op_params* params,
    struct brad_async_op* op
) {
    struct LoopContext* ctx = data;
    prepare_ctx_with_op(ctx, params, op, 0);
    const int ret = io_uring_submit(&ctx->ring);
    DEBUG_MESSAGE("Submitting io_uring sqe, ret: %d, op: %p\n", ret, op);
}

void submitv(
    void* data,
    struct brad_async_op_params** params,
    struct brad_async_op** ops,
    const size_t count,
    const bool chain
) {
    struct LoopContext* ctx = data;

    uint8_t flags = 0;

    if (chain) {
        flags |= IOSQE_IO_LINK;
    }

    for (int i = 0; i < count; i++) {
        prepare_ctx_with_op(ctx, params[i], ops[i], flags);
    }

    const int ret = io_uring_submit(&ctx->ring);
}

struct brad_async_op* enter(
    void* data
) {
    struct LoopContext* ctx = data;
    const int ret = io_uring_wait_cqe(&ctx->ring, &ctx->cqe);

    struct brad_async_op* op = io_uring_cqe_get_data(ctx->cqe);

    DEBUG_MESSAGE("Got cqe, ret: %d (%s), op: %p\n", ret, strerror(-ret), op);

    if (op != NULL) {
        // Transition from pending -> ready or cancelled.
        atomic_compare_exchange_strong(
            &op->state,
            &pending_state,
            ctx->cqe->res >= 0 ? BRAD_ASYNC_OP_STATE_READY : BRAD_ASYNC_OP_STATE_CANCELLED
        );
    }

    DEBUG_MESSAGE("io_uring cqe wait completed %d (%s)\n", ctx->cqe->res, strerror(-ctx->cqe->res));

    return op;
}

void leave(
    void* data,
    const struct brad_async_op* op
) {
    struct LoopContext* ctx = data;
    assert(op == io_uring_cqe_get_data(ctx->cqe));
    io_uring_cqe_seen(&ctx->ring, ctx->cqe);
}
