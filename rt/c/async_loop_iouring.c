#include "async_loop.h"
#include "debug.h"

#include <assert.h>
#include <liburing.h>
#include <stdlib.h>
#include <string.h>

void* new();
void drop(void* data);
void submit(void* data, struct brad_async_op* op);
struct brad_async_op* enter(void* data);
void leave(void* data, struct brad_async_op* op);

struct LoopContext {
    struct io_uring ring;
    struct io_uring_params params;
    struct io_uring_cqe* cqe; // current cqe for enter <-> leave.
};

void prepare_ctx_with_op(struct LoopContext* ctx, struct brad_async_op* op, uint8_t flags);

struct brad_async_event_loop_fns io_uring_loop_fns = {
    .new = new,
    .drop = drop,
    .submit = submit,
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
    struct brad_async_op* op,
    const uint8_t flags
) {
    struct io_uring_sqe* sqe = io_uring_get_sqe(&ctx->ring);
    assert(sqe != NULL);

    switch (op->op) {
        case BRAD_ASYNC_OP_NOP: {
            io_uring_prep_nop(sqe);
            break;
        }
        case BRAD_ASYNC_OP_MSG_EL: {
            const struct brad_async_op_msg_el* msg_el_op = (struct brad_async_op_msg_el*)op;
            const struct LoopContext* el_ctx = msg_el_op->el_data;
            io_uring_prep_msg_ring(sqe, el_ctx->ring.ring_fd, 0, 0, 0);
            break;
        }
        case BRAD_ASYNC_OP_SLEEP: {
            struct brad_async_op_sleep* sleep_op = (struct brad_async_op_sleep*)op;
            io_uring_prep_timeout(sqe, &sleep_op->ts, 0, IORING_TIMEOUT_ETIME_SUCCESS);
            break;
        }
        case BRAD_ASYNC_OP_CANCEL: {
            const struct brad_async_op_cancel* cancel_op = (struct brad_async_op_cancel*)op;
            io_uring_prep_cancel(sqe, cancel_op->op, 0);
            break;
        }
        case BRAD_ASYNC_OP_BATCH:
        case BRAD_ASYNC_OP_CHAIN: {
            unreachable();
        }
        default: {
            DEBUG_MESSAGE("Unknown async op type: %d\n", op->op);
            break;
        }
    }

    // Configure each submission queue entry.
    io_uring_sqe_set_data(sqe, op);
    sqe->flags |= flags;
}

void submit(
    void* data,
    struct brad_async_op* op
) {
    struct LoopContext* ctx = data;

    uint8_t flags = 0;

    switch (op->op) {
        case BRAD_ASYNC_OP_CHAIN: {
            flags |= IOSQE_IO_LINK;
        }
        case BRAD_ASYNC_OP_BATCH: {
            const struct brad_async_op_multiple* multiple_op = (struct brad_async_op_multiple*)op;
            assert(multiple_op->count > 1);

            for (size_t i = 0; i < multiple_op->count; i++) {
                prepare_ctx_with_op(ctx, multiple_op->ops[i], flags);
            }

            break;
        }
        default: {
            prepare_ctx_with_op(ctx, op, flags);
            break;
        }
    }

    const int ret = io_uring_submit(&ctx->ring);

    DEBUG_MESSAGE("Submitting io_uring sqe, ret: %d, op: %p\n", ret, op);
}

struct brad_async_op* enter(
    void* data
) {
    struct LoopContext* ctx = data;
    const int ret = io_uring_wait_cqe(&ctx->ring, &ctx->cqe);

    struct brad_async_op* op = io_uring_cqe_get_data(ctx->cqe);

    DEBUG_MESSAGE("Got cqe, ret: %d (%s), op: %p\n", ret, strerror(-ret), op);

    if (op != NULL) {
        atomic_store(
            &op->state,
            ctx->cqe->res >= 0 ? BRAD_ASYNC_OP_STATE_READY : BRAD_ASYNC_OP_STATE_CANCELLED
        );
    }

    DEBUG_MESSAGE("io_uring cqe wait completed %d (%s)\n", ctx->cqe->res, strerror(-ctx->cqe->res));

    return op;
}

void leave(
    void* data,
    struct brad_async_op* op
) {
    struct LoopContext* ctx = data;
    assert(op == io_uring_cqe_get_data(ctx->cqe));
    io_uring_cqe_seen(&ctx->ring, ctx->cqe);
}
