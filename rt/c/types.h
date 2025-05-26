#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef uint64_t brad_size;
typedef uintptr_t brad_ptr;
typedef uint8_t brad_byte;
typedef char brad_char;

typedef struct {
} brad_void;

typedef struct {
    brad_size length;
    brad_char data[];
}* brad_str;

typedef struct {
    brad_size length;
    brad_size capacity;
    brad_byte data[];
}* brad_list;

typedef int64_t brad_int;
typedef double brad_float;

struct brad_result {
    bool ok;
};

inline struct brad_result brad_result_err() { return (struct brad_result){.ok = false}; }

inline struct brad_result brad_result_ok() { return (struct brad_result){.ok = true}; }
