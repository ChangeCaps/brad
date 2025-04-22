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
    op1->base.op = BRAD_ASYNC_OP_MSG_EL;
    op1->el_data = el.data;

    struct brad_async_op_sleep* op2 = calloc(1, sizeof(struct brad_async_op_sleep));
    op2->base.op = BRAD_ASYNC_OP_SLEEP;
    op2->ts.tv_sec = 3;
    op2->ts.tv_nsec = 0;

    struct brad_async_op_sleep* op0 = calloc(1, sizeof(struct brad_async_op));
    op0->base.op = BRAD_ASYNC_OP_NOP;

    struct brad_async_op_multiple* op3 = calloc(1, sizeof(struct brad_async_op_multiple));
    op3->base.op = BRAD_ASYNC_OP_CHAIN;
    op3->count = 2;
    op3->ops = calloc(op3->count, sizeof(struct brad_async_op*));
    // op3->ops[0] = (struct brad_async_op*)op0;
    op3->ops[0] = (struct brad_async_op*)op2;
    op3->ops[1] = (struct brad_async_op*)op1;

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