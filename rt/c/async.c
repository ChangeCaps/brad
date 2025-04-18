#include "async.h"

#include <stdlib.h>
#include <string.h>
#include <threads.h>

/* brad async runtime for C11 + linux 6.10+ using a multi-ring io_uring model.

This coroutine implementation is allocation agnostic, and everything is handled
using either:

- Static stack allocations
- User provided buffers with limited sizes

The high level execution model is as follows:

- User requests a function be executed in one of two modes "preemptive" or "coop"
    * "coop" mode indicates that the function only uses supported primitives on shared thread
    * "preemptive" mode for blocking / cpu tasks / external functions etc on new thread
- The runtime adds the function to the scheduler, which selects its primary thread
- The scheduler runs per thread



The library provides functions that are prefixed with `brad_async_`

The top level context for this implementation is "ThreadContext".
*/

struct ThreadContext {
    struct brad_async_params params;
    struct io_uring ring;
    struct io_uring_params ring_params;
    struct brad_async_task** tasks;
};

thread_local struct ThreadContext* current_thread_context = NULL;

struct CoroutineContext {};

// END OF INTERNAL IMPLEMENTATION

// START OF PUBLIC API

struct brad_async_result brad_async_init(
    const struct brad_async_params* params
) {
    if (params->mode != BRAD_ASYNC_MODE_THIS) {
        return brad_async_result_err();
    }

    current_thread_context = malloc(sizeof(struct ThreadContext));
    current_thread_context->tasks = malloc(sizeof(struct brad_async_task*) * params->max_tasks);
    memcpy(&current_thread_context->params, params, sizeof(struct brad_async_params));

    return brad_async_result_ok();
}

struct brad_async_result brad_async_deinit() {
    if (current_thread_context) {
        free(current_thread_context);
        current_thread_context = NULL;
    }

    return brad_async_result_ok();
}

struct brad_async_task_handle brad_async_spawn(
    struct brad_async_task* task
) {
    current_thread_context->tasks[0] = task;
    return (struct brad_async_task_handle){.handle = 0};
}

enum brad_async_task_state brad_async_task_poll(
    const struct brad_async_task_handle handle
) {
    struct brad_async_task* task = current_thread_context->tasks[handle.handle];
    return task->fp(task);
}

void brad_async_cancel(
    struct brad_async_task_handle handle
) {}

void brad_async_suspend(
    struct brad_async_task_handle handle
) {}

void brad_async_resume(
    struct brad_async_task_handle handle
) {}