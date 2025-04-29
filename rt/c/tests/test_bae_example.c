#include "../allocator.h"
#include "../bae_loop.h"
#include "../bae_op.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

volatile bool running = true;

void on_msg() {
    running = false;
    printf("on_msg\n");
}

int main(
    int argc,
    char** argv
) {
    const struct brad_allocator allocator = brad_allocator_new((void*)&brad_malloc_allocator);
    const struct bae_loop el = bae_loop_new(&io_uring_loop_ops);

    struct bae_op_params_rw params1;
    bae_prep_write(&params1, STDOUT_FILENO, "Hello world!\n", 14, 0);

    struct bae_op_rw* op1 =
        (struct bae_op_rw*)bae_op_from_params(allocator, (struct bae_op_params*)&params1);

    // Write hello world once
    bae_loop_submit(el, (struct bae_op_params*)&params1, (struct bae_op*)op1);

    // Sleep for 2 seconds then write hello world twice.
    struct bae_op_params_sleep params2;
    bae_prep_sleep(&params2, (struct __kernel_timespec){.tv_sec = 2, .tv_nsec = 0});

    struct bae_op_params* params[] = {
        (struct bae_op_params*)&params2,
        (struct bae_op_params*)&params1,
    };

    struct bae_op** ops = bae_ops_from_params(allocator, params, 2);
    ops[0]->waker_ref = (void*)on_msg;

    // Cancel sleep operation.
    struct bae_op_params_cancel params3;
    bae_prep_cancel(&params3, ops[0]);

    struct bae_op* op3 = bae_op_from_params(allocator, (struct bae_op_params*)&params3);

    bae_loop_submitv(el, params, ops, 2, true);
    bae_loop_submit(el, (struct bae_op_params*)&params3, op3);

    while (running) {
        const struct bae_op* op = bae_loop_enter(el);

        // Do something with op result data.
        if (op->waker_ref != NULL) {
            ((void (*)())op->waker_ref)();
        }

        bae_loop_leave(el, op);
    }

    bae_loop_drop(el);

    return 0;
}