#include "../rt/c/allocator.h"
#include "../rt/c/async_loop.h"
#include "../rt/c/async_op.h"

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
    const struct brad_async_event_loop el = {
        .fns = &io_uring_loop_fns,
        .data = io_uring_loop_fns.new()
    };

    const struct brad_allocator allocator = brad_allocator_new((void*)&brad_malloc_allocator);

    struct brad_async_op_params_rw params1;
    brad_async_prep_write(&params1, STDOUT_FILENO, 14, 0);

    struct brad_async_op_rw* op1 = (struct brad_async_op_rw*)
        brad_async_op_from_params(allocator, (struct brad_async_op_params*)&params1);

    op1->buf = (char*)brad_allocator_alloc(allocator, 14, 1, 0);
    snprintf(op1->buf, 14, "Hello world!\n");

    el.fns->submit(el.data, (struct brad_async_op_params*)&params1, (struct brad_async_op*)op1);

    while (running) {
        struct brad_async_op* op = el.fns->enter(el.data);
        // Do something with op result data.
        el.fns->leave(el.data, op);
    }

    el.fns->drop(el.data);

    return 0;
}