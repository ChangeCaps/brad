#pragma once

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
