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
    struct brad_async_event_loop el = io_uring_loop_fns.new();

    struct brad_async_op_notify* op = calloc(1, sizeof(struct brad_async_op_notify));
    op->base.op = BRAD_ASYNC_OP_NOTIFY;
    op->base.state = BRAD_ASYNC_OP_STATE_PENDING;
    op->base.waker = on_msg;
    op->fd = el.fd;

    el.fns->submit(&el, (struct brad_async_op*)op);

    while (running) {
        el.fns->enter(&el);
    }

    el.fns->drop(&el);

    return 0;
}