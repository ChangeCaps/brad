#pragma once

#include "bae_op.h"
#include "debug.h"

struct bae_loop_ops {
    // Constructor
    void* (*new)();

    // Destructor
    void (*drop)(void*);

    // Submit an async operation (pointer-unique)
    void (*submit)(void*, struct bae_op_params*, struct bae_op*);
    void (*submitv)(void*, struct bae_op_params**, struct bae_op**, size_t, bool);

    // Block until an operation is completed.
    struct bae_op* (*enter)(void*);
    // Mark operation as *read* which will allow for some cleanup.
    void (*leave)(void*, const struct bae_op*);
};

struct bae_loop {
    struct bae_loop_ops* ops;
    void* data;
};

INLINE struct bae_loop bae_loop_new(
    struct bae_loop_ops* loop_ops
) {
    return (struct bae_loop){.ops = loop_ops, .data = loop_ops->new()};
}

INLINE void bae_loop_drop(
    const struct bae_loop loop
) {
    loop.ops->drop(loop.data);
}

INLINE void bae_loop_submit(
    const struct bae_loop loop,
    struct bae_op_params* params,
    struct bae_op* op
) {
    loop.ops->submit(loop.data, params, op);
}

INLINE void bae_loop_submitv(
    const struct bae_loop loop,
    struct bae_op_params** params,
    struct bae_op** ops,
    const size_t count,
    const bool seq
) {
    loop.ops->submitv(loop.data, params, ops, count, seq);
}

INLINE struct bae_op* bae_loop_enter(
    const struct bae_loop loop
) {
    return loop.ops->enter(loop.data);
}

INLINE void bae_loop_leave(
    const struct bae_loop loop,
    const struct bae_op* op
) {
    loop.ops->leave(loop.data, op);
}

// Implementations

extern struct bae_loop_ops io_uring_loop_ops;