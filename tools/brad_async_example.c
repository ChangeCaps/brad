#include "../rt/c/async_loop.h"

#include <stdio.h>
#include <stdlib.h>

volatile bool running = true;

void on_msg() {
    running = false;
    printf("on_msg\n");
}

int main(
    int argc,
    char** argv
) {
    const struct brad_async_event_loop el = {
        .fns = &io_uring_loop_fns,
        .data = io_uring_loop_fns.new()
    };

    struct brad_async_op_msg_el* op1 = calloc(1, sizeof(struct brad_async_op_msg_el));
    brad_async_prep_msg_el(op1, el.data);

    struct brad_async_op_sleep* op2 = calloc(1, sizeof(struct brad_async_op_sleep));
    brad_async_prep_sleep(op2, (struct __kernel_timespec){.tv_sec = 4, .tv_nsec = 0});

    struct brad_async_op* op0 = calloc(1, sizeof(struct brad_async_op));
    brad_async_prep_nop(op0);

    struct brad_async_op_multiple* op3 = calloc(1, sizeof(struct brad_async_op_multiple));
    brad_async_prep_multiple(op3, calloc(3, sizeof(struct brad_async_op*)), true);
    brad_async_prep_multiple_add(op3, op0);
    brad_async_prep_multiple_add(op3, (struct brad_async_op*)op2);
    brad_async_prep_multiple_add(op3, (struct brad_async_op*)op1);

    el.fns->submit(el.data, (struct brad_async_op*)op3);

    while (running) {
        struct brad_async_op* op = el.fns->enter(el.data);
        // Do something with op result data.

        if ((void*)op == (void*)op1) {
            printf("completed op1 with state: %d\n", op->state);
            on_msg();
        }

        if ((void*)op == (void*)op2) {
            printf("completed op2 with state: %d\n", op->state);
        }

        el.fns->leave(el.data, op);
    }

    el.fns->drop(el.data);

    return 0;
}