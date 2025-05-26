#pragma once

#include "allocator.h"

struct brad_arena {

};

struct brad_arena_allocator {
    struct brad_allocator allocator;
};

struct brad_allocator brad_arena_derive_allocator(struct brad_arena_allocator arena_allocator);