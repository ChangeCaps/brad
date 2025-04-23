#

## Design

High level features:

- Support multiple task types, both natively async, externally async and blocking / CPU intensive tasks.
- Allocation free task abstraction (user allocates)

Based on familiar async primitives:

### Event loop + Future

- A future is a minimal unit associated with an event loop
- An event loop updates the *state* of a future
    * Ready/Cancelled
- A future can be shared across event loops and threads with some semantics
    * A future cannot be written to by another event loop or thread
    * A future cannot be read from another thread until it is ready

- An event loop is consumed by a scheduler

### Scheduler + Task

- A task is the unit which the scheduler orders based on `wake` invocations called inside future `poll`.
- A scheduler can span multiple threads and call into an event loop or not.
- The typical path to call logic inside a future is:
    * `Scheduler` -> `Task` -> `Future`
    * `Scheduler` -> `Loop` -> `Wake` -> `Scheduler` -> ...
- Scheduler coordinates multiple event loops and traditional threads.

## Sync primitives

- Locks
    * Needs knowledge of shared memory (no locks on non-shared data)
    * Needs knowledge of owner (different locks for different owners)
        * thread lock worst case
        * aio lock or no lock depending on usage of aio
    * based on operations performed (CPU task or not) decide on memory access pattern

mutex implementation that can register itself in all the io_uring threads its needed in
when it knows the poster will post it via io_uring as well.

Notes on SQPOLL

- Choose a core for the SQPOLL thread, every additional io_uring will share this with wq_fd
- This core will be excluded from high-cpu intensive tasks if possible.
- How do we dynamically choose the `sq_thread_idle`, or do we select something that fits our preemptive sched?

## Memory design

```
fn main {
    // queue with size 1
    let q = queue::new 1
       
    // task 1
    async loop {
        queue::push q 1
        queue::push q 2
        queue::push q 3
    }
    
    // task 2
    async loop {
        let v = queue::read q
        std::debug::print v
    }
}

alloc q (shared)

// task 1
alloc `task 1` pointer to q
3 states

switch (s) 
case s0 -> poll push 1 (then s1)
case s1 -> poll push 2 (then s2)
case s2 -> poll push 3 (then s0)


// task 2
alloc `task 2` pointer to q
1 state
switch (s)
case s0 -> poll read (then s0)
```

``` 
fn is_done x => ...
fn external_process x => ...

fn rec-tail x {
    match is_done x {
        false -> rec-tail external_process x
        true -> x
    }
}

fn rec-rec {
    if is_done x is false {
        return rec-rec external_process x
    }
    
    x
}

alloc x (shared)

// rec-tail
alloc task with pointer to x
2 states
switch (s)
case s0 -> alloc task with same data 

// rec-rec
alloc task with pointer to x

alloc task with new data

```

```

async {
  let other = 1
  let v = read 1024
}

alloc 1028 size buffer
give io_uring correctly offset buffer with read operation
waker returns to right after v.

```

Future to read 1024 bytes

Call poll with context

First run scheduled it in the event loop? submit

Second poll then ready after waker calls it, how does the future check if its done?

## C-API

A future is some state + a poll function + a cleanup function

A op is a future with 2 state fields, `state` and `waker_ref` (task).

To issue an op you provide its params to an allocator interface which based on the params will construct the operator
heap instance which is passed (together with the params descriptor) to the event loop, the parameters will be deallocated.

## Sources

- https://doc.rust-lang.org/std/task/index.html
- https://github.com/tokio-rs/tokio/pull/1657
- https://tokio.rs/blog/2019-10-scheduler
- https://github.com/tokio-rs/tokio-uring/

- https://docs.libuv.org/en/v1.x/api.html
- https://github.com/libuv/libuv