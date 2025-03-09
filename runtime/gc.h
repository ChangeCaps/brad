#pragma once

#include <stddef.h>
#include <stdint.h>

typedef uint64_t brad_size;
typedef uintptr_t brad_ptr;
typedef struct {
} brad_void;

typedef struct {
    brad_size size;
} brad_layout;

typedef struct {
    brad_size length;
    char data[];
}* brad_str;

typedef int64_t brad_int;

brad_ptr brad_alloc(brad_layout layout, brad_size size);
void brad_free(brad_ptr ptr);

void brad_print(brad_str string);

brad_str brad_str_concat(brad_str a, brad_str b);
