#include "bae_scheduler.h"

#include <assert.h>
#include <stdlib.h>

static void* new(const struct bae_scheduler_params* params);
static void drop(void* data);
static void wake(void* data, struct bae_task* task);
static void enter(void* data);
static void interrupt(void* data);

struct bae_scheduler_ops bae_scheduler_simple_ops = {
    .new = new,
    .drop = drop,
    .wake = wake,
    .enter = enter,
    .interrupt = interrupt,
};

struct SimpleSchedulerContext {
    struct bae_loop loop;
    struct bae_task* task;
};

void* new(
    const struct bae_scheduler_params* params
) {
    assert(params->type == BAE_SCHEDULER_SIMPLE);
    struct SimpleSchedulerContext* ctx = malloc(sizeof(struct SimpleSchedulerContext));
    ctx->loop = params->loop;

    return ctx;
}

void drop(
    void* data
) {
    free(data);
}

void wake(
    void* data,
    struct bae_task* task
) {
    struct SimpleSchedulerContext* ctx = data;
    task->next = ctx->task;
    ctx->task = task;

    switch (bae_future_poll(task->future)) {
        case BAE_FUTURE_STATE_PENDING:
            break;
        case BAE_FUTURE_STATE_READY:
            // TODO, wake the task.
            break;
        case BAE_FUTURE_STATE_CANCELED:
            // TODO, cancel the task.
            break;
        default:
            unreachable();
    }
}

void enter(
    void* data
) {
    struct SimpleSchedulerContext* ctx = data;

    struct bae_op* op = bae_loop_enter(ctx->loop);

    if (op == nullptr || op->waker_ref == nullptr) {
        return;
    }

    wake(ctx, op->waker_ref);
}

void interrupt(
    void* data
) {}
