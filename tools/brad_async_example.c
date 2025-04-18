#include "../rt/c/async.h"

#include <stdio.h>
#include <stdlib.h>

struct example_function_data {
    int counter;
};

enum brad_async_task_state example_function(
    struct brad_async_task* task
) {
    struct example_function_data* data = (struct example_function_data*)task->data;

    if (data->counter++ < 3) {
        return BRAD_ASYNC_TASK_STATE_PENDING;
    }

    return BRAD_ASYNC_TASK_STATE_READY;
}

int main(
    int argc,
    char** argv
) {
    const struct brad_async_params params = {
        .mode = BRAD_ASYNC_MODE_THIS,
    };

    if (!brad_async_init(&params).ok) {
        return EXIT_FAILURE;
    }

    struct example_function_data data = {0};

    struct brad_async_task main_task = {
        .mode = BRAD_ASYNC_TASK_MODE_COOP,
        .fp = example_function,
        .data = (uint8_t*)&data,
    };

    const struct brad_async_task_handle main_task_handle = brad_async_spawn(&main_task);

    while (brad_async_task_poll(main_task_handle) != BRAD_ASYNC_TASK_STATE_READY) {
        printf("Still running main task!\n");
    }

    if (!brad_async_deinit().ok) {
        return EXIT_FAILURE;
    }

    return 0;
}