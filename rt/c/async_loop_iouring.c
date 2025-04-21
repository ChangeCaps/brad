#include "async_loop.h"
#include "debug.h"

#include <liburing.h>
#include <stdlib.h>

struct brad_async_event_loop new();
void submit(struct brad_async_event_loop* el, struct brad_async_op* op);
void enter(struct brad_async_event_loop* el);
void drop(struct brad_async_event_loop* el);

struct LoopContext {
    struct io_uring ring;
    struct io_uring_params params;
};

struct brad_async_event_loop_fns io_uring_loop_fns = {
    .new = new,
    .drop = drop,
    .submit = submit,
    .enter = enter,
};

struct brad_async_event_loop new() {
    struct LoopContext* ctx = calloc(1, sizeof(struct LoopContext));

    int ret = io_uring_queue_init_params(8, &ctx->ring, &ctx->params);

    DEBUG_MESSAGE("Initializing io_uring loop, ret: %d\n", ret);

    return (struct brad_async_event_loop){
        .fns = &io_uring_loop_fns,
        .fd = ctx->ring.ring_fd,
        .data = ctx,
    };
}

void drop(
    struct brad_async_event_loop* el
) {
    struct LoopContext* ctx = el->data;
    io_uring_queue_exit(&ctx->ring);
    free(ctx);
    DEBUG_MESSAGE_SINGLE("Exiting io_uring loop\n");
}

void submit(
    struct brad_async_event_loop* el,
    struct brad_async_op* op
) {
    struct LoopContext* ctx = el->data;
    struct io_uring_sqe* sqe = io_uring_get_sqe(&ctx->ring);

    // Build ring based on operation
    switch (op->op) {
        case BRAD_ASYNC_OP_NOTIFY: {
            const struct brad_async_op_notify* notify_op = (struct brad_async_op_notify*)op;
            io_uring_prep_msg_ring(sqe, notify_op->fd, 0, 0, 0);
            break;
        }
        default: {
            DEBUG_MESSAGE("Unknown async op type: %d\n", op->op);
            return;
        }
    }

    // Set user-data.
    io_uring_sqe_set_data(sqe, op);
    // Submit ring.
    int ret = io_uring_submit(&ctx->ring);

    DEBUG_MESSAGE("Submitting io_uring sqe, ret: %d, op: %p\n", ret, op);
}

void enter(
    struct brad_async_event_loop* el
) {
    struct LoopContext* ctx = el->data;
    struct io_uring_cqe* cqe;

    int ret = io_uring_wait_cqe(&ctx->ring, &cqe);

    struct brad_async_op* op = io_uring_cqe_get_data(cqe);

    DEBUG_MESSAGE("Got cqe, ret: %d, op: %p\n", ret, op);

    if (op != NULL && op->waker != NULL) {
        op->waker();
    }

    DEBUG_MESSAGE("io_uring cqe wait completed %d\n", ret);

    io_uring_cqe_seen(&ctx->ring, cqe);
}
