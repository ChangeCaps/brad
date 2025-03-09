#pragma once

#include <stddef.h>
#include <stdint.h>

typedef uint64_t brad_size;
typedef uintptr_t brad_ptr;

typedef struct {
} brad_void;

typedef struct {
    brad_size length;
    char data[];
}* brad_str;

typedef int64_t brad_int;

typedef void (*brad_marker)(brad_ptr ptr);

typedef struct {
    brad_marker marker;
    brad_size ref_count;
    uint8_t data[];
} brad_allocation;

typedef struct {
    brad_size len;
    brad_size cap;
    brad_ptr* allocations;
} brad_allocations;

typedef struct {
    brad_allocations allocations;
} brad_thread_context;

#define brad_allocation_from_ptr(ptr)                                          \
    ((brad_allocation*)(ptr - offsetof(brad_allocation, data)))

#define brad_allocation_to_ptr(allocation)                                     \
    ((brad_ptr)allocation + offsetof(brad_allocation, data))

extern brad_thread_context brad_context;

brad_ptr brad_alloc(brad_size size, brad_marker marker);
void brad_retain(brad_ptr ptr);
void brad_release(brad_size count);
void brad_collect();

void brad_mark(brad_ptr ptr);

void brad_print(brad_str string);

brad_str brad_str_concat(brad_str a, brad_str b);
