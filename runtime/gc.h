#pragma once

#include <stddef.h>
#include <stdint.h>

typedef uint64_t BradSize;
typedef uintptr_t BradPtr;

typedef struct {
    BradSize size;
} BradMemoryLayout;

typedef struct {
    BradSize length;
    char data[];
}* BradString;

BradPtr brad_alloc(BradMemoryLayout layout, BradSize size);
void brad_free(BradPtr ptr);

void brad_print(BradString string);
