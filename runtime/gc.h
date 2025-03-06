#pragma once

#include <stddef.h>
#include <stdint.h>

typedef uint64_t BradSize;
typedef uintptr_t BradPtr;
typedef struct {
    BradSize size;
} BradMemoryLayout;

BradPtr brad_alloc(BradMemoryLayout layout, BradSize size);
void brad_free(BradPtr ptr);
