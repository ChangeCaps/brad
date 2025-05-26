#pragma once

#include "bae_loop.h"
#include "bae_task.h"
#include "debug.h"

enum bae_scheduler_t {
    BAE_SCHEDULER_SIMPLE,
};

struct bae_scheduler_params {
    enum bae_scheduler_t type;
    union {
        struct {
            struct bae_loop loop;
        }; // Simple scheduler params
    };
};

struct bae_scheduler_ops {
    void* (*new)(const struct bae_scheduler_params*);
    void (*drop)(void*);
    // Wake a task
    void (*wake)(void*, struct bae_task*);
    // Enter the scheduler
    void (*enter)(void*);
    // Interrupt handler
    void (*interrupt)(void*);
};

struct bae_scheduler {
    struct bae_scheduler_ops ops;
    void* data;
};

INLINE struct bae_scheduler bae_scheduler_new(
    const struct bae_scheduler_ops* ops,
    const struct bae_scheduler_params* params
) {
    return (struct bae_scheduler){.ops = *ops, .data = ops->new(params)};
}

INLINE void bae_scheduler_drop(
    const struct bae_scheduler scheduler
) {
    scheduler.ops.drop(scheduler.data);
}

INLINE void bae_scheduler_wake(
    const struct bae_scheduler scheduler,
    struct bae_task* task
) {
    scheduler.ops.wake(scheduler.data, task);
}

INLINE void bae_scheduler_enter(
    const struct bae_scheduler scheduler
) {
    scheduler.ops.enter(scheduler.data);
}

INLINE void bae_scheduler_interrupt(
    const struct bae_scheduler scheduler
) {
    scheduler.ops.interrupt(scheduler.data);
}

extern struct bae_scheduler_ops bae_scheduler_simple_ops;