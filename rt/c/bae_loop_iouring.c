#include "bae_loop.h"
#include "debug.h"

#include <assert.h>
#include <liburing.h>
#include <stdlib.h>
#include <string.h>

static void* new();
static void drop(void* data);
static void submit(void* data, struct bae_op_params* params, struct bae_op* op);
static void submitv(
    void* data,
    struct bae_op_params** params,
    struct bae_op** ops,
    size_t count,
    bool chain
);
static struct bae_op* enter(void* data);
static void leave(void* data, const struct bae_op* op);

struct LoopContext {
    struct io_uring ring;
    struct io_uring_params params;
    struct io_uring_cqe* cqe; // current cqe for enter <-> leave.
};

void prepare_ctx_with_op(
    struct LoopContext* ctx,
    struct bae_op_params* params,
    struct bae_op* op,
    uint8_t flags
);

static enum bae_op_state pending_state = BAE_OP_STATE_PENDING;

struct bae_loop_ops io_uring_loop_ops = {
    .new = new,
    .drop = drop,
    .submit = submit,
    .submitv = submitv,
    .enter = enter,
    .leave = leave,
};

void* new() {
    struct LoopContext* ctx = calloc(1, sizeof(struct LoopContext));

    // This can make cancel operations "fail" in a sense.
    // somehow we would need to check for successful submissions, possibly possible.
    // ctx->params.flags |= IORING_SETUP_SQPOLL;

    // ctx->params.flags |= IORING_SETUP_ATTACH_WQ;
    // ctx->params.wq_fd = TODO;

    // ctx->params.sq_thread_idle = 10; // Allow kernel 10 milliseconds of idle time. Optimially we
    // want to control this value dynamically.

    const int ret = io_uring_queue_init_params(16, &ctx->ring, &ctx->params);

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

void prepare_ctx_with_op(
    struct LoopContext* ctx,
    struct bae_op_params* params,
    struct bae_op* op,
    const uint8_t flags
) {
    struct io_uring_sqe* sqe = io_uring_get_sqe(&ctx->ring);
    assert(sqe != NULL);

    switch (params->op) {
        case BAE_OP_NOP: {
            io_uring_prep_nop(sqe);
            break;
        }
        case BAE_OP_MSG_EL: {
            const struct bae_op_params_msg_el* msg_el_params = (struct bae_op_params_msg_el*)params;
            const struct LoopContext* el_ctx = msg_el_params->el_data;
            io_uring_prep_msg_ring(sqe, el_ctx->ring.ring_fd, 0, 0, 0);
            break;
        }
        case BAE_OP_SLEEP: {
            const struct bae_op_params_sleep* sleep_params = (struct bae_op_params_sleep*)params;
            struct bae_op_sleep* sleep_op = (struct bae_op_sleep*)op;
            memcpy(&sleep_op->ts, &sleep_params->ts, sizeof(struct __kernel_timespec));
            io_uring_prep_timeout(sqe, &sleep_op->ts, 0, IORING_TIMEOUT_ETIME_SUCCESS);
            break;
        }
        case BAE_OP_READ: {
            const struct bae_op_params_rw* read_params = (struct bae_op_params_rw*)params;
            io_uring_prep_read(
                sqe,
                read_params->fd,
                read_params->buf,
                read_params->nbytes,
                read_params->offset
            );
            break;
        }
        case BAE_OP_WRITE: {
            const struct bae_op_params_rw* write_params = (struct bae_op_params_rw*)params;
            io_uring_prep_write(
                sqe,
                write_params->fd,
                write_params->buf,
                write_params->nbytes,
                write_params->offset
            );
            break;
        }
        case BAE_OP_CANCEL: {
            const struct bae_op_params_cancel* cancel_params = (struct bae_op_params_cancel*)params;

            // Preemptively mark the operation as cancelled.
            atomic_compare_exchange_strong(
                &cancel_params->op->state,
                &pending_state,
                BAE_OP_STATE_CANCELLED
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

    DEBUG_MESSAGE("Preparing params with type %s, op: %p\n", bae_op_t_to_string(params->op), op);
}

void submit(
    void* data,
    struct bae_op_params* params,
    struct bae_op* op
) {
    struct LoopContext* ctx = data;
    prepare_ctx_with_op(ctx, params, op, 0);
    const int ret = io_uring_submit(&ctx->ring);

    DEBUG_MESSAGE(
        "Submitted to io_uring ret: %d (%s)\n",
        ret,
        ret < 0 ? strerror(-ret) : "Success"
    );
}

void submitv(
    void* data,
    struct bae_op_params** params,
    struct bae_op** ops,
    const size_t count,
    const bool chain
) {
    struct LoopContext* ctx = data;

    uint8_t flags = 0;

    if (chain) {
        flags |= IOSQE_IO_LINK;
    }

    for (size_t i = 0; i < count; i++) {
        prepare_ctx_with_op(ctx, params[i], ops[i], flags);
    }

    const int ret = io_uring_submit(&ctx->ring);

    DEBUG_MESSAGE(
        "Submitted to io_uring ret: %d (%s)\n",
        ret,
        ret < 0 ? strerror(-ret) : "Success"
    );
}

struct bae_op* enter(
    void* data
) {
    struct LoopContext* ctx = data;
    const int ret = io_uring_wait_cqe(&ctx->ring, &ctx->cqe);

    struct bae_op* op = io_uring_cqe_get_data(ctx->cqe);

#ifdef DEBUG
    if (ret < 0) {
        DEBUG_MESSAGE("Got cqe, ret: %d (%s), op: %p\n", ret, strerror(-ret), op);
    }
#endif

    if (op != NULL) {
        // Transition from pending -> ready or cancelled.
        atomic_compare_exchange_strong(
            &op->state,
            &pending_state,
            ctx->cqe->res >= 0 ? BAE_OP_STATE_READY : BAE_OP_STATE_CANCELLED
        );
    }

    DEBUG_MESSAGE(
        "io_uring wait for %p completed %d (%s)\n",
        op,
        ctx->cqe->res,
        strerror(-ctx->cqe->res)
    );

    return op;
}

void leave(
    void* data,
    const struct bae_op* op
) {
    struct LoopContext* ctx = data;
    assert(op == io_uring_cqe_get_data(ctx->cqe));
    io_uring_cqe_seen(&ctx->ring, ctx->cqe);
}
