#pragma once
#include <liburing.h>
#include <stdint.h>

/* See implementation for internal structures, this file contains the public
 * interface for the brad async runtime, objects contained by the runtime are
 * referred to by typed handle, based on a completion port model. */

struct brad_async_result {
    bool ok;
};

inline struct brad_async_result brad_async_result_err() {
    return (struct brad_async_result){.ok = false};
}

inline struct brad_async_result brad_async_result_ok() {
    return (struct brad_async_result){.ok = true};
}

enum brad_async_mode {
    BRAD_ASYNC_MODE_THIS,    // Only use single thread (this thread), no preemptive tasks.
    BRAD_ASYNC_MODE_FIXED,   // Only use set amount of threads (pre-allocated)
    BRAD_ASYNC_MODE_DYNAMIC, // Use a dynamic amount of threads.
};

struct brad_async_params {
    enum brad_async_mode mode;
    size_t max_tasks;

    union {
        struct {}; // THIS mode params
        struct {
            size_t tc_coop;
            size_t tc_preempt;
        }; // FIXED mode params
        struct {
            size_t tc_coop_min;
            size_t tc_coop_max;
            size_t tc_preempt_min;
            size_t tc_preempt_max;
        }; // DYNAMIC mode params
    };
};

enum brad_async_task_mode {
    BRAD_ASYNC_TASK_MODE_COOP,
    BRAD_ASYNC_TASK_MODE_PREEMPTIVE,
};

enum brad_async_task_state {
    BRAD_ASYNC_TASK_STATE_PENDING,
    BRAD_ASYNC_TASK_STATE_READY,
};

struct brad_async_task {
    enum brad_async_task_mode mode;
    enum brad_async_task_state (*fp)(struct brad_async_task*);
    uint8_t* data;
};

struct brad_async_task_handle {
    uint64_t handle;
};

struct brad_async_result brad_async_init(const struct brad_async_params* params);
struct brad_async_result brad_async_deinit();

struct brad_async_task_handle brad_async_spawn(struct brad_async_task* task);

enum brad_async_task_state brad_async_task_poll(struct brad_async_task_handle handle);
void brad_async_cancel(struct brad_async_task_handle handle);
void brad_async_suspend(struct brad_async_task_handle handle);
void brad_async_resume(struct brad_async_task_handle handle);